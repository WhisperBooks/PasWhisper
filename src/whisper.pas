unit whisper;

interface

uses SysUtils, WhisperExternal, WhisperTypes;

type
  TWhisper = class
  private
    FCtx: PWhisperContext;
    FModel: String;
    FCParams: TWhisperContextParams;
    procedure Init;
  public
    constructor Create;
    destructor Destroy; override;
    function Test(const AModel: String): TWhisperModel;
  end;

var
  PModel: PAnsiChar;

implementation

{ TWhisper }

constructor TWhisper.Create;
begin
  inherited Create;
  if Pointer(@WhisperInitFromFileWithParams) = Nil then
    Raise Exception.Create('Whisper library not available');
end;

destructor TWhisper.Destroy;
begin

  inherited;
end;

function TWhisper.Test(const AModel: String): TWhisperModel;
begin
  if FileExists(AModel) then
    begin
      Self.FModel := AModel;
      PModel := PAnsiChar(Pointer(AnsiString(Self.FModel)));
      Init;
    end;
  Result := default(TWhisperModel);
end;

procedure TWhisper.Init;
var
  PCtx: PWhisperContext;
  CParams: TWhisperContextParams;
  PParams: PWhisperContextParams;
begin
  PParams := WhisperContextDefaultParams;
  CParams := PParams^;
  Self.FCParams := CParams;
  PCtx := WhisperInitFromFileWithParams(PModel, @FCParams);
  if(PCtx <> Nil) then
    WhisperFree(PCtx);
end;

end.
