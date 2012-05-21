unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SoftwareRenderer;

type
  TForm1 = class(TForm)
    RenderInfo: TLabel;
    GameTimer: TTimer;
    GameScreen: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure GameTimerTimer(Sender: TObject);
  private
    { Private-Deklarationen }
    FSoftwareRenderer: TSoftwareRenderer;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation
uses
  Cube, BaseMesh;
{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSoftwareRenderer := TSoftwareRenderer.Create();
  FSoftwareRenderer.MeshList.Add(TCube.Create());
//  LCube := TCube.Create();
//  LCube.Position := Vector(-20, 0, LCube.Position.Z);
//  FSoftwareRenderer.MeshList.Add(LCube);
  GameTimer.Enabled := True;
end;

procedure TForm1.GameTimerTimer(Sender: TObject);
begin
  FSoftwareRenderer.RenderFrame(GameScreen.Canvas);
  RenderInfo.Caption := 'FPS: ' + IntToStr(FSoftwareRenderer.GetCurrentFPS) + ' PolyCount: ' + IntToStr(FSoftwareRenderer.GetCurrentPolyCount);
end;

end.
