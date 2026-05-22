unit Mundus.Renderer.Worker.Sync;

interface

uses
  System.SyncObjs;

type
  TStageSync = class
  private
    FEvent: TEvent;
    FCount: Integer;
    FMaxCount: Integer;
  public
    constructor Create(ACount: Integer);
    destructor Destroy; override;
    procedure Reset;
    procedure Sync;
  end;

implementation

{ TStageSync }

constructor TStageSync.Create;
begin
  inherited Create();
  FEvent := TEvent.Create(nil, True, False, '');
  FMaxCount := ACount;
end;

destructor TStageSync.Destroy;
begin
  FEvent.Free;
  inherited;
end;

procedure TStageSync.Reset;
begin
  FEvent.ResetEvent;
  FCount := FMaxCount;
end;

procedure TStageSync.Sync;
begin
  if AtomicDecrement(FCount) = 0 then
    FEvent.SetEvent
  else
    FEvent.WaitFor(INFINITE);
end;

end.
