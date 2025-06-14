unit ControlLoop;

interface

uses
  System.SysUtils, System.Types, System.Classes, FMX.Types;

type
  TControlLoop = Class(TComponent)
  strict private
    FOnClock: TNotifyEvent;
    FTimer: TTimer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnClock: TNotifyEvent read FOnClock write FOnClock;
  End;

var
  GlobalControlLoop: TControlLoop;

implementation

{ TControlLoop }

constructor TControlLoop.Create(AOwner: TComponent);
begin
  if GlobalControlLoop <> Nil then
    Abort
  else
    begin
      inherited Create(AOwner);
      GlobalControlLoop := Self;
    end;

end;

destructor TControlLoop.Destroy;
begin
  if GlobalControlLoop = Self then
    GlobalControlLoop := Nil;

  inherited Destroy;
end;

procedure FreeGlobalObjects;
begin
  if GlobalControlLoop <> nil then
    GlobalControlLoop.Free;
end;


begin
  AddExitProc(FreeGlobalObjects);
end.
