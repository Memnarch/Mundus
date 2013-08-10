unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SoftwareRenderer, StopWatch, Cube;

type
  TForm1 = class(TForm)
    RenderInfo: TLabel;
    GameTimer: TTimer;
    GameScreen: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure GameTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private-Deklarationen }
    FSoftwareRenderer: TSoftwareRenderer;
    FLastCall: TDateTime;
    FWatch: TStopWatch;
    FMinFPS, FMaxFPS: Integer;
    FCube: TCube;
  public
    { Public-Deklarationen }
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
end;

procedure TForm1.GameTimerTimer(Sender: TObject);
var
  LFPS: Integer;
begin
  FSoftwareRenderer.RenderFrame(GameScreen.Canvas);
  LFPS := FSoftwareRenderer.GetCurrentFPS;
  FMaxFPS := Max(FMaxFPS, LFPS);
  FMinFPS := Min(FMinFPS, LFPS);
  RenderInfo.Caption := 'FPS: ' + IntToStr(LFPS) + ' Min-Fps: ' +
      IntToStr(FMinFPS) + ' Max-Fps: ' + IntToStr(FMaxFPS) + ' PolyCount: ' +
      IntToStr(FSoftwareRenderer.GetCurrentPolyCount);
end;

end.
