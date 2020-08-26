unit Main;

interface

uses
  Generics.Collections,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  Mundus.Renderer,
  Mundus.Diagnostics.StopWatch,
  Mundus.Diagnostics.SamplerGraph,
  Mundus.Mesh.Plane,
  Mundus.Mesh.Cube,
  Mundus.Mesh,
  Mundus.ValueBuffer,
  Mundus.Math,
  Mundus.Texture,
  Mundus.Camera,
  Mundus.Material;

type
  TForm1 = class(TForm)
    GameTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure GameTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private-Deklarationen }
    FScene: TMeshGroup;
    FSoftwareRenderer: TMundusRenderer;
    FMinFPS, FMaxFPS: Integer;
    FLastReset: TDateTime;
    FGraph: TSamplerGraph;
    FDrawGraph: Boolean;
    FColors: TArray<TFloat4>;
    FCamera: TCamera;
    FLastTick: TDateTime;
    FSpeed: TFloat3;
    FRotationSpeed: TFloat3;
    FRotation: TFloat3;
    FLastMousePos: TPoint;
    FTextures: TObjectList<TTexture>;
    FTextureCache: TDictionary<string, TTexture>;
    procedure HandleAfterFrame(ACanvas: TCanvas);
    procedure HandleException(Sender: TObject; E: Exception);
    procedure HandleInitBuffer(AMesh: TMesh; const ABuffer: PValueBuffers);
    procedure FetchInput;
    procedure Tick(const ASeconds: Single);
    function GetTexture(const AMaterial: TMaterial): TTexture;
  public
    { Public-Deklarationen }
    procedure SetResolution(AWidth, AHeight: Integer);
  end;

var
  Form1: TForm1;

implementation

uses
  IOUtils,
  DateUtils,
  Math,
  System.UITypes,
  Mundus.Types,
  Mundus.Shader.Texture,
  Mundus.Shader.VertexGradient,
  Mundus.Shader.DepthColor,
  Mundus.Mesh.Loader;

{$R *.dfm}

const
  CColors: array[0..7] of TFloat4 = (
    (B: 1; G: 0.5; R: 0.5; A: 0),
    (B: 0.5; G: 1; R: 0.5; A: 0),
    (B: 1; G: 1; R: 0.5; A: 0),
    (B: 1; G: 0.5; R: 0.5; A: 0),
    (B: 0.5; G: 1; R: 0.5; A: 0),
    (B: 1; G: 1; R: 0.5; A: 0),
    (B: 1; G: 0.5; R: 0.5; A: 0),
    (B: 0.5; G: 1; R: 0.5; A: 0)
  );

const
  CMoveSpeed = 1000;
  CRotationSpeed = 5;
  CLevelPath = 'C:\Users\Alexander\Downloads\doom-e1m1-hangar-map\source\DOOM_E1M1\doom_E1M1.obj';
  CDefault = 'Default';

procedure TForm1.FetchInput;
var
  LPos: TPoint;
begin
  FSpeed := Default(TFloat3);
  FRotationSpeed := Default(TFloat3);
  if GetAsyncKeyState(Ord('A')) <> 0 then
    FSpeed.X := CMoveSpeed;
  if GetAsyncKeyState(Ord('D')) <> 0 then
    FSpeed.X := FSpeed.X - CMoveSpeed;
  if GetAsyncKeyState(Ord('W')) <> 0 then
    FSpeed.Z := CMoveSpeed;
  if GetAsyncKeyState(Ord('S')) <> 0then
    FSpeed.Z := FSpeed.Y - CMoveSpeed;

  if Focused then
  begin
    LPos := Mouse.CursorPos;
    FRotationSpeed.X := (FLastMousePos.Y - LPos.Y) * CRotationSpeed;
    FRotationSpeed.Y := (FLastMousePos.X - LPos.X) * CRotationSpeed;
    Mouse.CursorPos := FLastMousePos;
  end;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  Mouse.CursorPos := ClientToScreen(Point(Width div 2, Height div 2));
  FLastMousePos := Mouse.CursorPos;
  ShowCursor(False);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FSoftwareRenderer.Free;
  FGraph.Free;
  FTextureCache.Free;
  FTextures.Free;
  FScene.Free;
  Application.OnException := nil;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  LMesh: TMesh;
  LTexture: TTexture;
begin
  SetLength(FColors, Length(CColors));
  CopyMemory(@FColors[0], @CColors[0], SizeOf(CColors));
  Application.OnException := HandleException;
  FGraph := TSamplerGraph.Create();
  FTextures := TObjectList<TTexture>.Create();
  FTextureCache := TDictionary<string, TTexture>.Create();

//  LMesh := TPlane.Create(320, 180, 20);
//  LMesh.Position := Float3(0, 0, 246);
//  LMesh.Rotation := Float3(180, 0, 0);
//  LMesh.Shader := TTextureShader;
//  FScene := TMeshGroup.Create();
//  FScene.Meshes.Add(LMesh);
  FScene := TMeshLoaders.LoadFromFile(CLevelPath, 3);
  LTexture := TTexture.Create();
  LTexture.LoadFromFile('..\..\Crate_256.bmp');
  FTextures.Add(LTexture);
  FTextureCache.Add(CDefault, LTexture);
  FSoftwareRenderer := TMundusRenderer.Create(12);
  for LMesh in FScene.Meshes do
  begin
    LMesh.Shader := TTextureShader;
    LMesh.Position := Float3(0, -500, 12000);
    FSoftwareRenderer.MeshList.Add(LMesh);
    FSoftwareRenderer.Occluders.Add(LMesh);
  end;
  FSoftwareRenderer.OnInitValueBuffer := HandleInitBuffer;
  FSoftwareRenderer.OnAfterFrame := HandleAfterFrame;
  SetResolution(1280, 720);
  FCamera := FSoftwareRenderer.Camera;

  FLastReset := Now();
  FMinFPS := 1000;
  FMaxFPS := 0;
  FLastTick := Now();
  GameTimer.Enabled := True;
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin
  ShowCursor(True);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (WindowState = wsMaximized) then
  begin
    WindowState := wsNormal;
    FormStyle := fsNormal;
  end;

  if (Key = VK_RETURN) and (WindowState = wsNormal) then
  begin
    WindowState := wsMaximized;
    FormStyle := fsStayOnTop;
  end;

  if Key = VK_F1 then
    FDrawGraph := not FDrawGraph;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if WindowState = wsMaximized then
  begin
    BorderStyle := bsNone;
    ClientWidth := Screen.Width;
    ClientHeight := Screen.Height;
    Left := 0;
    Top := 0;
  end
  else
  begin
    BorderStyle := bsSizeable;
  end;
  SetResolution(ClientWidth, ClientHeight);
end;

procedure TForm1.GameTimerTimer(Sender: TObject);
var
  LNow: TDateTime;
begin
  FetchInput;
  LNow := Now();
  Tick(MilliSecondsBetween(LNow, FLastTick) / 1000);
  FLastTick := LNow;
  FSoftwareRenderer.RenderFrame(Canvas);
end;

function TForm1.GetTexture(const AMaterial: TMaterial): TTexture;
var
  LFile: string;
  LTexture: TTexture;
begin
  if not FTextureCache.TryGetValue(AMaterial.Name, Result) then
  begin
    LFile := ExtractFilePath(CLevelPath) + '\' + AMaterial.Texture;
    if TFile.Exists(LFile) then
    begin
      LTexture := TTexture.Create();
      LTexture.LoadFromFile(LFile);
      FTextures.Add(LTexture);
      FTextureCache.Add(AMaterial.Name, LTexture);
      Result := LTexture;
    end
    else
      FTextureCache.Add(AMaterial.Name, FTextureCache[CDefault]);
  end;
  if not Assigned(Result) then
    Result := FTextureCache[CDefault];
end;

procedure TForm1.HandleAfterFrame(ACanvas: TCanvas);
var
  LFPS: Integer;
  LNow: TDateTime;
begin
  ACanvas.Brush.Color := clWhite;
  LFPS := FSoftwareRenderer.GetCurrentFPS;
  if LFPS > 0 then
  begin
    if FDrawGraph then
    begin
      FGraph.AddSample(LFPS);
      FGraph.DrawGraph(ACanvas, Rect(10, ACanvas.ClipRect.Bottom-200, 10+400, ACanvas.ClipRect.Bottom-10));
    end;
    LNow := Now();
    if SecondsBetween(FLastReset, LNow) >= 5 then
    begin
      FMaxFPS := LFPS;
      FMinFPS := LFPS;
      FLastReset := LNow;
    end
    else
    begin
      FMaxFPS := Max(FMaxFPS, LFPS);
      FMinFPS := Min(FMinFPS, LFPS);
    end;
    ACanvas.TextOut(10, 10, IntToStr(FSoftwareRenderer.ResolutionX) + 'x' + IntToStr(FSoftwareRenderer.ResolutionY));
    ACanvas.TextOut(10, 30, Format('FPS: %.4d MinFPS: %.4d MaxFPS: %.4d', [LFPS, FMinFPS, FMaxFPS]));
  end;
end;

procedure TForm1.HandleException(Sender: TObject; E: Exception);
begin
  GameTimer.Enabled := False;
  MessageDlg('Renderer crashed with: ' + sLineBreak + E.ToString, mtError, [], 0);
end;

procedure TForm1.HandleInitBuffer(AMesh: TMesh;
  const ABuffer: PValueBuffers);
begin
  ABuffer.Float2Array[ABuffer.Float2Array.Bind('UV0')] := AMesh.UV;
  ABuffer.Texture[ABuffer.Texture.Bind('Tex0')] := GetTexture(AMesh.Material);
//  ABuffer.Float4Array[ABuffer.Float4Array.Bind('Color0')] := FColors;
end;

procedure TForm1.SetResolution(AWidth, AHeight: Integer);
begin
  FSoftwareRenderer.SetResolution(AWidth, AHeight);
  ClientWidth := AWidth;
  ClientHeight := AHeight;
  Caption := 'SoftwareRenderer ' + IntToSTr(AWidth) + 'x' + IntToStr(AHeight);
end;

procedure TForm1.Tick(const ASeconds: Single);
var
  LSpeed: TFloat3;
  LMoveSpeed: TFloat4;
  LRotationX, LRotationY, LRotation: TMatrix4x4;
begin
  LSpeed := FRotationSpeed;
  LSpeed.Mul(ASeconds);
  FRotation.Add(LSpeed);
  if FRotation.X < -90 then
    FRotation.X := -90;
  if FRotation.X > 90 then
    FRotation.X := 90;

  LRotationX.SetAsRotationXMatrix(DegToRad(FRotation.X));
  LRotationY.SetAsRotationYMatrix(DegToRad(FRotation.Y));
  LRotationY.MultiplyMatrix4D(LRotationX.Inverse);
  LRotation.SetAsIdentMatrix4D;
  LRotation.MultiplyMatrix4D(LRotationY);
  FCamera.Rotation := LRotation;

  LSpeed := FSpeed;
  LSpeed.Mul(ASeconds);
  LMoveSpeed := Float4(LSpeed.X, LSpeed.Y, LSpeed.Z, 1);
  LMoveSpeed := LRotation.Transform(LMoveSpeed);
  FCamera.Position.Add(Float3(LMoveSpeed.X, LMoveSpeed.Y, LMoveSpeed.Z));
end;

end.
