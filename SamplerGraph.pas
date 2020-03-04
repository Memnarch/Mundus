unit SamplerGraph;

interface

uses
  Types,
  Classes,
  Graphics;

type
  TSamplerGraph = class
    FSamples: TArray<Integer>;
    FStart: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSample(const ASanple: Integer);
    procedure DrawGraph(ATarget: TCanvas; ARect: TRect);
  end;

implementation

{ TSamplerGraph }

procedure TSamplerGraph.AddSample(const ASanple: Integer);
begin
  FStart := (FStart + 1) mod Length(FSamples);
  FSamples[FStart] := ASanple;
end;

constructor TSamplerGraph.Create;
begin
  SetLength(FSamples, 1024);
end;

destructor TSamplerGraph.Destroy;
begin
  inherited;
end;

procedure TSamplerGraph.DrawGraph(ATarget: TCanvas; ARect: TRect);
var
  LSample: Integer;
  LEnd, LIndex, LY: Integer;
  LX, LXStep: Double;
begin
  LEnd := (FStart - 1) mod Length(FSamples);
  if LEnd < 0 then
    LEnd := High(FSamples);
  LIndex := FStart;
  LX := 0;
  LY := ARect.Bottom;
  ATarget.Pen.Width := 1;
  ATarget.Pen.Color := clGreen;
  ATarget.MoveTo(Trunc(LX), LY);
  LXStep := ARect.Width / Length(FSamples);
  repeat
    LY := ARect.Bottom - FSamples[LIndex];
    ATarget.LineTo(Trunc(LX), LY);
    LIndex := (LIndex + 1) mod Length(FSamples);
    LX := LX + LXStep;
  until LIndex = LEnd;
end;

end.
