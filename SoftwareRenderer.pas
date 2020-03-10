unit SoftwareRenderer;

interface

uses
  Types, Classes, Windows, SysUtils, Graphics,
  Generics.Collections, Math3D, BaseMesh,
  ColorTypes, Shader, StopWatch, DrawCall,
  RenderWorker, Camera;

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
    FLineLength: NativeInt;
    FFirstLine: PRGB32Array;
    FQuadSize: Integer;
    FResolutionX: Integer;
    FResolutionY: Integer;
    FOnAfterFrame: TRenderEvent;
    FTimer: TStopWatch;
    FWorkers: TObjectList<TRenderWorker>;
    FRenderFences: TArray<THandle>;
    FCurrentBuffer: Boolean;
    FWorkerFPS: Integer;
    FCamera: TCamera;
    procedure SetDepthBufferSize(ABuffer: Boolean; AWidth, AHeight: Integer);
    procedure ClearDepthBuffer(ABuffer: Boolean);
    procedure TransformMesh(AMesh: TBaseMesh; AMatrix: TMatrix4x4; ATargetCall: PDrawCall);
    procedure DoAfterFrame(ACanvas: TCanvas);
    function GenerateDrawCalls(const AViewMatrix: TMatrix4x4): TDrawCalls;
    procedure DispatchCalls(ACanvas: TCanvas; ACalls: TDrawCalls);
    procedure SpinupWorkers(AWorkerCount: Integer);
    procedure TerminateWorkers;
    procedure WaitForRender;
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
    property Camera: TCamera read FCamera;
  end;

  function RGB32(ARed, AGreen, ABlue, AAlpha: Byte): TRGB32;

implementation

uses
  Math,
  DateUtils,
  SolidColorShader,
  DepthColorShader,
  TextureShader,
  Rasterizer,
  RenderTypes;

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
  FCamera := TCamera.Create();
  SetResolution(512, 512);
  FMeshList := TObjectList<TBaseMesh>.Create();
  FQuadSize := 8;

  FTexture := TBitmap.Create();
  FTexture.LoadFromFile('Crate.bmp');
  FTexture.PixelFormat := pf32bit;
  FTimer := TStopWatch.Create(False);

  FWorkers := TObjectList<TRenderWorker>.Create();
  SpinupWorkers(1);
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
  FCamera.Free;
  inherited;
end;

procedure TSoftwareRenderer.DispatchCalls(ACanvas: TCanvas; ACalls: TDrawCalls);
var
  LWorker: TRenderWorker;
  LBackBuffer, LFrontBuffer: Boolean;
  LFPS: Integer;
begin
  LBackBuffer := FCurrentBuffer;
  LFrontBuffer := not FCurrentBuffer;

  //ResetBackBuffer from last frame
  UpdateBufferResolution(LFrontBuffer, FResolutionX, FResolutionY);
  FBackBuffer[LFrontBuffer].Canvas.Brush.Color := clRed;
  FBackBuffer[LFrontBuffer].Canvas.FillRect(FBackBuffer[LFrontBuffer].Canvas.ClipRect);
  ClearDepthBuffer(LFrontBuffer);

  //wait for workers to finish frame
  WaitForRender;

//  //load workers with new stuff and start
  FWorkerFPS := High(FWorkerFPS);
  for LWorker in FWorkers do
  begin
    LWorker.DrawCalls := ACalls;
//    LWorker.Shader.InitTexture(FTexture);
    LWorker.PixelBuffer := FBackBuffer[LFrontBuffer];
    LWorker.ResolutionX := FResolutionX;
    LWorker.ResolutionY := FResolutionY;
    LFPS := LWorker.FPS;
    if LFPS < FWorkerFPS then
      FWorkerFPS := LFPS;
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

function TSoftwareRenderer.GenerateDrawCalls(const AViewMatrix: TMatrix4x4): TDrawCalls;
var
  LMesh: TBaseMesh;
  LCall: PDrawCall;
  LMove: TMatrix4x4;
  LRotationX, LRotationY, LRotationZ: TMatrix4x4;
  LProjection: TMatrix4x4;
begin
  Result := FDrawCalls[not FCurrentBuffer];
  Result.Reset;
  FPolyCount := 0;
  for LMesh in FMeshList do
  begin
    LCall := Result.Add;;
    LMove.SetAsMoveMatrix(LMesh.Position.X, LMesh.Position.Y, LMesh.Position.Z);
    LRotationX.SetAsRotationXMatrix(DegToRad(LMesh.Rotation.X));
    LRotationY.SetAsRotationYMatrix(DegToRad(LMesh.Rotation.Y));
    LRotationZ.SetAsRotationZMatrix(DegToRad(LMesh.Rotation.Z));
    LMove.MultiplyMatrix4D(LRotationX);
    LMove.MultiplyMatrix4D(LRotationY);
    LMove.MultiplyMatrix4D(LRotationZ);
    LMove.MultiplyMatrix4D(AViewMatrix);
    LProjection.SetAsPerspectiveProjectionMatrix(100, 200, 0.7, FResolutionX/FResolutionY);
    LProjection.MultiplyMatrix4D(LMove);
    TransformMesh(LMesh, LProjection, LCall);
    LCall.Shader := LMesh.Shader;
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
  LViewMoveMatrix: TMatrix4x4;
  LRotationX, LRotationY, LRotationZ: TMatrix4x4;
begin
  FTimer.Start();

  LViewMoveMatrix.SetAsMoveMatrix(FCamera.Position.X, FCamera.Position.Y, FCamera.Position.Z);
  LRotationX.SetAsRotationXMatrix(DegToRad(FCamera.Rotation.X));
  LRotationY.SetAsRotationYMatrix(DegToRad(FCamera.Rotation.Y));
  LRotationZ.SetAsRotationZMatrix(DegToRad(FCamera.Rotation.Z));
  LViewMoveMatrix.MultiplyMatrix4D(LRotationX);
  LViewMoveMatrix.MultiplyMatrix4D(LRotationY);
  LViewMoveMatrix.MultiplyMatrix4D(LRotationZ);
  FPolyCount := 0;
  LDrawCalls := GenerateDrawCalls(LViewMoveMatrix);
  DispatchCalls(ACanvas, LDrawCalls);

  FTimer.Stop();
  FFPS := Min(FWorkerFPS, 1000000 div FTimer.ElapsedMicroseconds);
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
  SetLength(FRenderFences, AWorkerCount);
  for i := 0 to Pred(AWorkerCount) do
  begin
    LWorker := TRenderWorker.Create();
    LWorker.BlockSteps := AWorkerCount;
    LWorker.BlockOffset := i;
    FWorkers.Add(LWorker);
    FRenderFences[i] := LWorker.RenderFence;
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
  LShader: TShader;
  LBuffer: TVertexAttributeBuffer;
  LBufferSize: Integer;
  LVInput: TVertexShaderInput;
begin
  LBufferSize := AMesh.Shader.GetAttributeBufferSize;
  SetLength(LBuffer, LBufferSize);
  LShader := AMesh.Shader.Create();
  try
    for i := 0 to AMesh.Vertices.Count - 1 do
    begin
      LVertex.Element[0] := AMesh.Vertices[i].X;
      LVertex.Element[1] := AMesh.Vertices[i].Y;
      LVertex.Element[2] := AMesh.Vertices[i].Z;
      LVertex.Element[3] := 1;
      LVInput.VertexID := i;
      LShader.VertexShader(AMatrix, LVertex, LVInput, LBuffer);
      LVertex.NormalizeKeepW;
      ATargetCall.AddVertex(LVertex, LBuffer);
    end;
    for LTriangle in AMesh.Triangles do
      ATargetCall.AddTriangle(LTriangle);
  finally
    LShader.Free;
  end;
end;

procedure TSoftwareRenderer.UpdateBufferResolution(ABuffer: Boolean; AWidth, AHeight: Integer);
begin
  if (FBackBuffer[ABuffer].Width <> AWidth) or (FBackBuffer[ABuffer].Height <> AHeight) then
  begin
    FBackBuffer[ABuffer].SetSize(AWidth, Aheight);
    FFirstLIne := FBackBuffer[ABuffer].ScanLine[0];
    FLineLength := (NativeInt(FBackBuffer[ABuffer].Scanline[1]) - NativeInt(FFirstLine)) div SizeOf(TRGB32);
    FBackBuffer[ABuffer].Canvas.Pen.Color := clBlack;
    FBackBuffer[ABuffer].Canvas.Brush.Color := clBlack;
    SetDepthBufferSize(ABuffer, AWidth, AHeight);
    ClearDepthBuffer(ABuffer);
  end;
end;

procedure TSoftwareRenderer.WaitForRender;
begin
  WaitForMultipleObjects(Length(FRenderFences), @FRenderFences[0], True, INFINITE);
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
