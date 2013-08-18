unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SoftwareRenderer, StopWatch, Cube;

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
    FSoftwareRenderer: TSoftwareRenderer;
    FLastCall: TDateTime;
    FWatch: TStopWatch;
    FMinFPS, FMaxFPS: Integer;
    FCube: TCube;
    procedure HandleAfterFrame(ACanvas: TCanvas);
  public
    { Public-Deklarationen }
    procedure SetResolution(AWidth, AHeight: Integer);
  end;

var
  Form1: TForm1;

implementation

uses
  DateUtils, BaseMesh, Math;
{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FSoftwareRenderer.Free;
  FWatch.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCube := TCube.Create();
  FCube.Position := Vector(0, 0, 132);
  FSoftwareRenderer := TSoftwareRenderer.Create();
  FSoftwareRenderer.MeshList.Add(FCube);

  FSoftwareRenderer.OnAfterFrame := HandleAfterFrame;
  SetResolution(1280, 720);
//  LCube := TCube.Create();
//  LCube.Position := Vector(-20, 0, LCube.Position.Z);
//  FSoftwareRenderer.MeshList.Add(LCube);
  FLastCall := Now();
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
    FCube.Position := Vector(FCube.Position.X + 3, FCube.Position.Y, FCube.Position.Z);
  end;
  if (Key = VK_RIGHT) then
  begin
    FCube.Position := Vector(FCube.Position.X - 3, FCube.Position.Y, FCube.Position.Z);
  end;
  if (Key = VK_UP) then
  begin
    FCube.Position := Vector(FCube.Position.X, FCube.Position.Y, FCube.Position.Z + 3);
  end;
  if (Key = VK_DOWN) then
  begin
    FCube.Position := Vector(FCube.Position.X, FCube.Position.Y, FCube.Position.Z - 3);
  end;
  if (Key = VK_ESCAPE) and (WindowState = wsMaximized) then
  begin
    WindowState := wsNormal;
  end;
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
end;

procedure TForm1.HandleAfterFrame(ACanvas: TCanvas);
var
  LFPS: Integer;
begin
  ACanvas.Brush.Color := clWhite;
  LFPS := FSoftwareRenderer.GetCurrentFPS;
  if LFPS > 0 then
  begin
    FMaxFPS := Max(FMaxFPS, LFPS);
    FMinFPS := Min(FMinFPS, LFPS);
    ACanvas.TextOut(10, 10, IntToStr(FSoftwareRenderer.ResolutionX) + 'x' + IntToStr(FSoftwareRenderer.ResolutionY));
    ACanvas.TextOut(10, 30, 'FPS: ' + IntToStr(LFPS) + ' Min-Fps: ' +
        IntToStr(FMinFPS) + ' Max-Fps: ' + IntToStr(FMaxFPS) + ' PolyCount: ' +
        IntToStr(FSoftwareRenderer.GetCurrentPolyCount));
  end;
end;

procedure TForm1.SetResolution(AWidth, AHeight: Integer);
begin
  FSoftwareRenderer.SetResolution(AWidth div FSoftwareRenderer.QuadSize * FSoftwareRenderer.QuadSize,
    AHeight div FSoftwareRenderer.QuadSize * FSoftwareRenderer.QuadSize);
  ClientWidth := AWidth;
  ClientHeight := AHeight;
  Caption := 'SoftwareRenderer ' + IntToSTr(AWidth) + 'x' + IntToStr(AHeight);
end;

end.
