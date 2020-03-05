unit SamplerGraph;

interface

uses
  Types,
  Classes,
  Graphics;

type
  TSamplerGraph = class
  private
    FSamples: TArray<Integer>;
    FAverages: TArray<Integer>;
    FMax: Integer;
    FStart: Integer;
    procedure DrawValues(ATarget: TCanvas; ARect: TRect; const AValues: TArray<Integer>; AColor: TColor);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSample(const ASanple: Integer);
    procedure DrawGraph(ATarget: TCanvas; ARect: TRect);
  end;

implementation

{ TSamplerGraph }

procedure TSamplerGraph.AddSample(const ASanple: Integer);
var
  LAverage, LValue: Integer;
begin
  FStart := (FStart + 1) mod Length(FSamples);
  FSamples[FStart] := ASanple;
  FMax := 0;
  LAverage := 0;
  for LValue in FSamples do
  begin
    Inc(LAverage, LValue);
    if LValue > FMax then
      FMax := LValue;
  end;
  LAverage := LAverage div Length(FSamples);
  if LAverage > FMax then
    FMax := LAverage;
  FAverages[FStart] := LAverage;
end;

constructor TSamplerGraph.Create;
begin
  SetLength(FSamples, 512);
  SetLength(FAverages, 512);
end;

destructor TSamplerGraph.Destroy;
begin
  inherited;
end;

procedure TSamplerGraph.DrawGraph(ATarget: TCanvas; ARect: TRect);
begin
  DrawValues(ATarget, ARect, FSamples, clGreen);
  DrawValues(ATarget, ARect, FAverages, clBlue);
  ATarget.Pen.Color := clBlack;
  ATarget.MoveTo(ARect.Left, ARect.Top);
  ATarget.LineTo(ARect.Left, ARect.Bottom);
  ATarget.LineTo(ARect.Right, ARect.Bottom);
  ATarget.LineTo(ARect.Right, ARect.Top);
end;

procedure TSamplerGraph.DrawValues(ATarget: TCanvas; ARect: TRect;
  const AValues: TArray<Integer>; AColor: TColor);
var
  LSample: Integer;
  LEnd, LIndex, LY: Integer;
  LX, LXStep, LYStep: Double;
begin
  LEnd := (FStart - 1) mod Length(AValues);
  if LEnd < 0 then
    LEnd := High(AValues);
  LIndex := FStart;
  LX := ARect.Left;
  LY := ARect.Bottom;
  if FMax > 0 then
    LYStep := ARect.Height / FMax
  else
    LYStep := 0;
  ATarget.Pen.Width := 1;
  ATarget.Pen.Color := AColor;
  ATarget.MoveTo(Trunc(LX), LY);
  LXStep := ARect.Width / Length(AValues);
  repeat
    LY := Trunc(ARect.Bottom - AValues[LIndex] * LYStep);
    ATarget.LineTo(Trunc(LX), LY);
    LIndex := (LIndex + 1) mod Length(AValues);
    LX := LX + LXStep;
  until LIndex = LEnd;
end;

end.
