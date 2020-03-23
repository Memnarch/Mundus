unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  Mundus.Renderer,
  Mundus.Diagnostics.StopWatch,
  Mundus.Diagnostics.SamplerGraph,
  Mundus.Mesh.Cube,
  Mundus.Mesh,
  Mundus.ValueBuffer,
  Mundus.Math,
  Mundus.Texture;

type
  TForm1 = class(TForm)
    GameTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure GameTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
    FSoftwareRenderer: TMundusRenderer;
    FWatch: TStopWatch;
    FMinFPS, FMaxFPS: Integer;
    FCube: TCube;
    FLastReset: TDateTime;
    FGraph: TSamplerGraph;
    FDrawGraph: Boolean;
    FColors: TArray<TFloat4>;
    FTexture: TTexture;
    procedure HandleAfterFrame(ACanvas: TCanvas);
    procedure HandleException(Sender: TObject; E: Exception);
    procedure HandleInitBuffer(AMesh: TMesh; const ABuffer: PValueBuffers);
  public
    { Public-Deklarationen }
    procedure SetResolution(AWidth, AHeight: Integer);
  end;

var
  Form1: TForm1;

implementation

uses
  DateUtils,
  Math,
  System.UITypes,
  Mundus.Types,
  Mundus.Shader.Texture,
  Mundus.Shader.VertexGradient,
  Mundus.Shader.DepthColor;

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

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FSoftwareRenderer.Free;
  FWatch.Free;
  FGraph.Free;
  FTexture.Free;
  Application.OnException := nil;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetLength(FColors, Length(CColors));
  CopyMemory(@FColors[0], @CColors[0], SizeOf(CColors));
  Application.OnException := HandleException;
  FGraph := TSamplerGraph.Create();
  FCube := TCube.Create();
  FCube.Position := Float3(0, 0, 132);
  FCube.Rotation := Float3(0, 0, 45);
  FCube.Shader := TTextureShader;
  FTexture := TTexture.Create();
  FTexture.LoadFromFile('..\..\Crate_256.bmp');
  FSoftwareRenderer := TMundusRenderer.Create();
  FSoftwareRenderer.MeshList.Add(FCube);
  FSoftwareRenderer.OnInitValueBuffer := HandleInitBuffer;
  FSoftwareRenderer.OnAfterFrame := HandleAfterFrame;
  SetResolution(1280, 720);

  FLastReset := Now();
  FMinFPS := 1000;
  FMaxFPS := 0;
  FWatch := TStopWatch.Create(False);
  GameTimer.Enabled := True;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_LEFT) then
  begin
    FCube.Position := Float3(FCube.Position.X + 3, FCube.Position.Y, FCube.Position.Z);
  end;
  if (Key = VK_RIGHT) then
  begin
    FCube.Position := Float3(FCube.Position.X - 3, FCube.Position.Y, FCube.Position.Z);
  end;
  if (Key = VK_UP) then
  begin
    FCube.Position := Float3(FCube.Position.X, FCube.Position.Y, FCube.Position.Z + 3);
  end;
  if (Key = VK_DOWN) then
  begin
    FCube.Position := Float3(FCube.Position.X, FCube.Position.Y, FCube.Position.Z - 3);
  end;
  if (Key = VK_ESCAPE) and (WindowState = wsMaximized) then
  begin
    WindowState := wsNormal;
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
begin
  FSoftwareRenderer.RenderFrame(Canvas);
  FCube.Rotation := Float3(FCube.Rotation.X + 0.25, FCube.Rotation.Y, FCube.Rotation.Z);
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
  ABuffer.Texture[ABuffer.Texture.Bind('Tex0')] := FTexture;
//  ABuffer.Float4Array[ABuffer.Float4Array.Bind('Color0')] := FColors;
end;

procedure TForm1.SetResolution(AWidth, AHeight: Integer);
begin
  FSoftwareRenderer.SetResolution(AWidth, AHeight);
  ClientWidth := AWidth;
  ClientHeight := AHeight;
  Caption := 'SoftwareRenderer ' + IntToSTr(AWidth) + 'x' + IntToStr(AHeight);
end;

end.
