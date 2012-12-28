unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SoftwareRenderer, StopWatch;

type
  TForm1 = class(TForm)
    RenderInfo: TLabel;
    GameTimer: TTimer;
    GameScreen: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure GameTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
    FSoftwareRenderer: TSoftwareRenderer;
    FLastCall: TDateTime;
    FWatch: TStopWatch;
    FMinFPS, FMaxFPS: Integer;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses
  DateUtils, Cube, BaseMesh, Math;
{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FSoftwareRenderer.Free;
  FWatch.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSoftwareRenderer := TSoftwareRenderer.Create();
  FSoftwareRenderer.MeshList.Add(TCube.Create());
//  LCube := TCube.Create();
//  LCube.Position := Vector(-20, 0, LCube.Position.Z);
//  FSoftwareRenderer.MeshList.Add(LCube);
  FLastCall := Now();
  FMinFPS := 1000;
  FMaxFPS := 0;
  FWatch := TStopWatch.Create(False);
  GameTimer.Enabled := True;
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
