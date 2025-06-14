unit JPRSE.MediaTimer;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs;

type
  TMediaTimer = class(TComponent)
  private
    fEvent:TEvent;
    FInterval: Cardinal;
    FOnMediaTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure SetOnMediaTimer(const Value: TNotifyEvent);
    procedure SetInterval(const Value: Cardinal);
    procedure SetEnabled(const Value: Boolean);
    procedure WaitForInterval;
  public
    constructor Create(AnOwner:TComponent); override;
    destructor Destroy; override;
  published
    property Enabled:Boolean read FEnabled write SetEnabled;
    property Interval:Cardinal read FInterval write SetInterval;
    property OnMediaTimer:TNotifyEvent read FOnMediaTimer write SetOnMediaTimer;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TMediaTimer]);
end;

{ TMediaTimer }

constructor TMediaTimer.Create(AnOwner:TComponent);
begin
  inherited;
  fEvent := TEvent.Create;
  fEnabled := false;
  fInterval := 1000;
end;

destructor TMediaTimer.Destroy;
begin
  fEvent.SetEvent;
  inherited;
end;

procedure TMediaTimer.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
    begin
    FEnabled := Value;
    if fEnabled then
      WaitForInterval
    else
      fEvent.SetEvent;
    end;
end;

procedure TMediaTimer.SetOnMediaTimer(const Value: TNotifyEvent);
begin
  FOnMediaTimer := Value;
end;

procedure TMediaTimer.SetInterval(const Value: Cardinal);
begin
  FInterval := Value;
end;

procedure TMediaTimer.WaitForInterval;
begin
 TThread.CreateAnonymousThread(
  procedure
  var
    res:TWaitResult;
  begin
    while true do
      begin
      fEvent.ResetEvent;
      res := FEvent.WaitFor(fInterval);
      if res <> TWaitResult.wrTimeout then exit;
      if Assigned(fOnMediaTimer) then
        begin
        TThread.Synchronize(nil,
         procedure
         begin
         fOnMediaTimer(Self);
         end);
        end;
      end;
  end
 ).Start;
end;

end.
