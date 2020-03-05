unit SoftwareRenderer;

interface

uses
  Types, Classes, Windows, SysUtils, Graphics,
  Generics.Collections, Math3D, BaseMesh,
  ColorTypes, Shader, StopWatch, DrawCall,
  RenderWorker;

type
  TDepthBuffer = array of array of Single;
  TRenderEvent = procedure(Canvas: TCanvas) of object;

  TSoftwareRenderer = class
  private
    FDepthBuffer: array[boolean] of TDepthBuffer;
    FZeroDepthBuffer: TDepthBuffer;
    FBackBuffer: array[boolean] of TBitmap;
    FDrawCalls: array[boolean] of TDrawCalls;
    FTexture: TBitmap;
    FMeshList: TObjectList<TBaseMesh>;
    FFPS: Integer;
    FPolyCount: Cardinal;
    FLineLength: LongInt;
    FFirstLine: PRGB32Array;
    FQuadSize: Integer;
    FResolutionX: Integer;
    FResolutionY: Integer;
    FOnAfterFrame: TRenderEvent;
    FTimer: TStopWatch;
    //buffers for rendering the frame
    FWorldMatrix: TMatrix4x4;
    FViewMatrix: TMatrix4x4;
    FProjectionMatrix: TMatrix4x4;  // Gerade dabei gewesen Viewtransformation einzubauen!! Seite 86 in TeilB
    FMoveMatrix: TMatrix4x4;
    FRotateXMatrix: TMatrix4x4;
    FRotateYMatrix: TMatrix4x4;
    FRotateZMatrix: TMatrix4x4;
    FWorkers: TObjectList<TRenderWorker>;
    FCurrentBuffer: Boolean;
    procedure SetDepthBufferSize(ABuffer: Boolean; AWidth, AHeight: Integer);
    procedure ClearDepthBuffer(ABuffer: Boolean);
    procedure TransformMesh(AMesh: TBaseMesh; AMatrix: TMatrix4x4; ATargetCall: PDrawCall);
    procedure DoAfterFrame(ACanvas: TCanvas);
    function GenerateDrawCalls: TDrawCalls;
    procedure DispatchCalls(ACanvas: TCanvas; ACalls: TDrawCalls);
    procedure SpinupWorkers(AWorkerCount: Integer);
    procedure TerminateWorkers;
    procedure UpdateBufferResolution(ABuffer: Boolean; AWidth, AHeight: Integer);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure SetResolution(AWidth, AHeight: Integer);
    procedure RenderFrame(ACanvas: TCanvas);
    function GetCurrentFPS(): Integer;
    function GetCurrentPolyCount(): Cardinal;
    property MeshList: TObjectList<TBaseMesh> read FMeshList;
    property QuadSize: Integer read FQuadSize;
    property OnAfterFrame: TRenderEvent read FOnAfterFrame write FOnAfterFrame;
    property ResolutionX: Integer read FResolutionX;
    property ResolutionY: Integer read FResolutionY;
  end;

  function RGB32(ARed, AGreen, ABlue, AAlpha: Byte): TRGB32;

implementation

uses
  Math,
  DateUtils,
  SolidColorShader,
  DepthColorShader,
  TextureShader,
  Rasterizer;

var
  GTest: Single  = 0;
  GTest2: Single = 0;

{ TSoftwareRenderer }

procedure TSoftwareRenderer.ClearDepthBuffer;
begin
  FDepthBuffer[ABuffer] := FZeroDepthBuffer;
end;

constructor TSoftwareRenderer.Create;
begin
  FBackBuffer[True] := TBitmap.Create();
  FBackbuffer[True].PixelFormat := pf32bit;
  FBackBuffer[False] := TBitmap.Create();
  FBackbuffer[False].PixelFormat := pf32bit;
  FDrawCalls[True] := TDrawCalls.Create();
  FDrawCalls[False] := TDrawCalls.Create();
  SetResolution(512, 512);
  FMeshList := TObjectList<TBaseMesh>.Create();
  FQuadSize := 8;

  FTexture := TBitmap.Create();
  FTexture.LoadFromFile('Crate.bmp');
  FTexture.PixelFormat := pf32bit;
  FTimer := TStopWatch.Create(False);

  FWorkers := TObjectList<TRenderWorker>.Create();
  SpinupWorkers(2);
end;

destructor TSoftwareRenderer.Destroy;
begin
  TerminateWorkers;
  FWorkers.Free;
  FMeshList.Free();
  FBackBuffer[True].Free();
  FBackBuffer[False].Free();
  FDrawCalls[True].Free;
  FDrawCalls[False].Free;
  FTexture.Free;
  FTimer.Free;
  inherited;
end;

procedure TSoftwareRenderer.DispatchCalls(ACanvas: TCanvas; ACalls: TDrawCalls);
var
  LWorker: TRenderWorker;
  LBackBuffer, LFrontBuffer: Boolean;
begin
  LBackBuffer := FCurrentBuffer;
  LFrontBuffer := not FCurrentBuffer;

  //ResetBackBuffer from last frame
  UpdateBufferResolution(LFrontBuffer, FResolutionX, FResolutionY);
  FBackBuffer[LFrontBuffer].Canvas.Brush.Color := clRed;
  FBackBuffer[LFrontBuffer].Canvas.FillRect(FBackBuffer[LFrontBuffer].Canvas.ClipRect);
  ClearDepthBuffer(LFrontBuffer);

  //wait for workers to finish frame
  for LWorker in FWorkers do
    LWorker.WaitForRender;

//  //load workers with new stuff and start
  for LWorker in FWorkers do
  begin
    LWorker.DrawCalls := ACalls;
    LWorker.Shader.InitTexture(FTexture);
    LWorker.Shader.PixelBuffer := FBackBuffer[LFrontBuffer];
    LWorker.ResolutionX := FResolutionX;
    LWorker.ResolutionY := FResolutionY;
    LWorker.StartRender;
  end;

  //Draw Backbuffer to FrontBuffer
  DoAfterFrame(FBackBuffer[LBackBuffer].Canvas);
  ACanvas.Draw(0, 0, FBackBuffer[LBackBuffer]);
  //flip buffers
  FCurrentBuffer := not FCurrentBuffer;
end;

procedure TSoftwareRenderer.DoAfterFrame(ACanvas: TCanvas);
begin
  if Assigned(FOnAfterFrame) then
  begin
    FOnAfterFrame(ACanvas);
  end;
end;

function TSoftwareRenderer.GenerateDrawCalls: TDrawCalls;
var
  LMesh: TBaseMesh;
  LCall: PDrawCall;
begin
  Result := FDrawCalls[not FCurrentBuffer];
  Result.Reset;
  FPolyCount := 0;
  for LMesh in FMeshList do
  begin
    LCall := Result.Add;;
    FMoveMatrix.SetAsMoveMatrix(LMesh.Position.X, LMesh.Position.Y, LMesh.Position.Z);
    FRotateXMatrix.SetAsRotationXMatrix(DegToRad(GTest));
    FRotateYMatrix.SetAsRotationYMatrix(DegToRad(GTest2));
    FRotateZMatrix.SetAsRotationZMatrix(DegToRad(0));
    FWorldMatrix  := FMoveMatrix;
    FWorldMatrix.MultiplyMatrix4D(FRotateXMatrix);
    FWorldMatrix.MultiplyMatrix4D(FRotateYMatrix);
    FWorldMatrix.MultiplyMatrix4D(FRotateZMatrix);
    FWorldMatrix.MultiplyMatrix4D(FViewMatrix);
    FProjectionMatrix.SetAsPerspectiveProjectionMatrix(100, 200, 0.7, FResolutionX/FResolutionY);
    FProjectionMatrix.MultiplyMatrix4D(FWorldMatrix);
    TransformMesh(LMesh, FProjectionMatrix, LCall);
  end;
end;

function TSoftwareRenderer.GetCurrentFPS: Integer;
begin
  Result := FFPS;
end;

function TSoftwareRenderer.GetCurrentPolyCount: Cardinal;
begin
  Result := FPolyCount;
end;

procedure TSoftwareRenderer.RenderFrame(ACanvas: TCanvas);
var
  LDrawCalls: TDrawCalls;
begin
  FTimer.Start();

  FViewMatrix.SetAsMoveMatrix(0, 0, 0);
  FRotateXMatrix.SetAsRotationXMatrix(DegToRad(0));
  FRotateYMatrix.SetAsRotationYMatrix(DegToRad(0));
  FRotateZMatrix.SetAsRotationZMatrix(DegToRad(0));
  FViewMatrix.MultiplyMatrix4D(FRotateXMatrix);
  FViewMatrix.MultiplyMatrix4D(FRotateYMatrix);
  FViewMatrix.MultiplyMatrix4D(FRotateZMatrix);
  FPolyCount := 0;
  LDrawCalls := GenerateDrawCalls();
  DispatchCalls(ACanvas, LDrawCalls);

  FTimer.Stop();
  FFPS := 1000000 div FTimer.ElapsedMicroseconds;

  GTest := GTest + 0.25;
//  GTest := 45;//GTest + 0.25;
  GTest2 := 45;
end;

procedure TSoftwareRenderer.SetDepthBufferSize(ABuffer: Boolean; AWidth, AHeight: Integer);
var
  i: Integer;
begin
  SetLength(FDepthBuffer[ABuffer], AHeight);
  SetLength(FZeroDepthBuffer, AHeight);
  for i := 0 to AHeight - 1 do
  begin
    SetLength(FDepthBuffer[ABuffer][i], AWidth);
    SetLength(FZeroDepthBuffer[i], AWidth);
    ZeroMemory(FZeroDepthBuffer[i], AWidth);
  end;
end;

procedure TSoftwareRenderer.SetResolution(AWidth, AHeight: Integer);
begin
  FResolutionX := AWidth;
  FResolutionY := AHeight;
end;

procedure TSoftwareRenderer.SpinupWorkers(AWorkerCount: Integer);
var
  i: Integer;
  LWorker: TRenderWorker;
begin
  for i := 0 to Pred(AWorkerCount) do
  begin
    LWorker := TRenderWorker.Create();
    LWorker.BlockSteps := AWorkerCount;
    LWorker.BlockOffset := i;
    FWorkers.Add(LWorker);
    LWorker.Start;
  end;
end;

procedure TSoftwareRenderer.TerminateWorkers;
var
  LWorker: TRenderWorker;
begin
  for LWorker in FWorkers do
    LWorker.Terminate;
end;

procedure TSoftwareRenderer.TransformMesh(AMesh: TBaseMesh; AMatrix: TMatrix4x4; ATargetCall: PDrawCall);
var
  i: Integer;
  LVertex: TFloat4;
  LTriangle: TTriangleClass;
begin
  for i := 0 to AMesh.Vertices.Count - 1 do
  begin
    LVertex.Element[0] := AMesh.Vertices[i].X;
    LVertex.Element[1] := AMesh.Vertices[i].Y;
    LVertex.Element[2] := AMesh.Vertices[i].Z;
    LVertex.Element[3] := 1;
    LVertex := AMatrix.Transform(LVertex);
    LVertex.NormalizeKeepW;
    ATargetCall.AddVertex(LVertex);
  end;
  for LTriangle in AMesh.Triangles do
    ATargetCall.AddTriangle(LTriangle);
end;

procedure TSoftwareRenderer.UpdateBufferResolution(ABuffer: Boolean; AWidth, AHeight: Integer);
begin
  if (FBackBuffer[ABuffer].Width <> AWidth) or (FBackBuffer[ABuffer].Height <> AHeight) then
  begin
    FBackBuffer[ABuffer].SetSize(AWidth, Aheight);
    FFirstLIne := FBackBuffer[ABuffer].ScanLine[0];
    FLineLength := (Longint(FBackBuffer[ABuffer].Scanline[1]) - Longint(FFirstLine)) div SizeOf(TRGB32);
    FBackBuffer[ABuffer].Canvas.Pen.Color := clBlack;
    FBackBuffer[ABuffer].Canvas.Brush.Color := clBlack;
    SetDepthBufferSize(ABuffer, AWidth, AHeight);
    ClearDepthBuffer(ABuffer);
  end;
end;

{ some functions }

function RGB32(ARed, AGreen, ABlue, AAlpha: Byte): TRGB32;
begin
  Result.R := ARed;
  Result.G := AGreen;
  Result.B := ABlue;
  Result.A := AAlpha;
end;

end.
