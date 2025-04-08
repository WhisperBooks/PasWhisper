unit whisper;

interface

uses SysUtils, WhisperExternal, WhisperTypes;

type
  TWhisper = class
  strict private
    FCtx: TWhisperContext;
    FModel: String;
    FCParams: TWhisperContextParams;
    procedure Init;
    function GetNlen                : Int32;
    function GetNlenFromState       : Int32;
    function GetNvocab              : Int32;
    function GetNtextCtx            : Int32;
    function GetNaudioCtx           : Int32;
    function GetIsMultilingual      : Int32;
    function GetModelNvocab         : Int32;
    function GetModelNaudioCtx      : Int32;
    function GetModelNaudioState    : Int32;
    function GetModelNaudioHead     : Int32;
    function GetModelNaudioLayer    : Int32;
    function GetModelNtextCtx       : Int32;
    function GetModelNtextState     : Int32;
    function GetModelNtextHead      : Int32;
    function GetModelNtextLayer     : Int32;
    function GetModelNmels          : Int32;
    function GetModelftype          : Int32;
    function GetModeltype           : Int32;
  public
    constructor Create;
    destructor Destroy; override;
    function Test(const AModel: String): TWhisperModel;
    property Nlen                : Int32 Read GetNlen;
    property NlenFromState       : Int32 Read GetNlenFromState;
    property Nvocab              : Int32 Read GetNvocab;
    property NtextCtx            : Int32 Read GetNtextCtx;
    property NaudioCtx           : Int32 Read GetNaudioCtx;
    property IsMultilingual      : Int32 Read GetIsMultilingual;
    property ModelNvocab         : Int32 Read GetModelNvocab;
    property ModelNaudioCtx      : Int32 Read GetModelNaudioCtx;
    property ModelNaudioState    : Int32 Read GetModelNaudioState;
    property ModelNaudioHead     : Int32 Read GetModelNaudioHead;
    property ModelNaudioLayer    : Int32 Read GetModelNaudioLayer;
    property ModelNtextCtx       : Int32 Read GetModelNtextCtx;
    property ModelNtextState     : Int32 Read GetModelNtextState;
    property ModelNtextHead      : Int32 Read GetModelNtextHead;
    property ModelNtextLayer     : Int32 Read GetModelNtextLayer;
    property ModelNmels          : Int32 Read GetModelNmels;
    property Modelftype          : Int32 Read GetModelftype;
    property Modeltype           : Int32 Read GetModeltype;
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
  if(FCtx <> Nil) then
    WhisperFree(FCtx);

  inherited;
end;

function TWhisper.GetIsMultilingual: Int32;
begin
  Result := WhisperIsMultilingual(FCtx);
end;

function TWhisper.GetModelftype: Int32;
begin
  Result := WhisperModelftype(FCtx);
end;

function TWhisper.GetModelNaudioCtx: Int32;
begin
  Result := WhisperModelNaudioCtx(FCtx);
end;

function TWhisper.GetModelNaudioHead: Int32;
begin
  Result := WhisperModelNaudioHead(FCtx);
end;

function TWhisper.GetModelNaudioLayer: Int32;
begin
  Result := WhisperModelNaudioLayer(FCtx);
end;

function TWhisper.GetModelNaudioState: Int32;
begin
  Result := WhisperModelNaudioState(FCtx);
end;

function TWhisper.GetModelNmels: Int32;
begin
  Result := WhisperModelNmels(FCtx);
end;

function TWhisper.GetModelNtextCtx: Int32;
begin
  Result := WhisperModelNtextCtx(FCtx);
end;

function TWhisper.GetModelNtextHead: Int32;
begin
  Result := WhisperModelNtextHead(FCtx);
end;

function TWhisper.GetModelNtextLayer: Int32;
begin
  Result := WhisperModelNtextLayer(FCtx);
end;

function TWhisper.GetModelNtextState: Int32;
begin
  Result := WhisperModelNtextState(FCtx);
end;

function TWhisper.GetModelNvocab: Int32;
begin
  Result := WhisperModelNvocab(FCtx);
end;

function TWhisper.GetModeltype: Int32;
begin
  Result := WhisperModeltype(FCtx);
end;

function TWhisper.GetNaudioCtx: Int32;
begin
  Result := WhisperNaudioCtx(FCtx);
end;

function TWhisper.GetNlen: Int32;
begin
  Result := WhisperNlen(FCtx);
end;

function TWhisper.GetNlenFromState: Int32;
begin
  Result := WhisperNlenFromState(FCtx);
end;

function TWhisper.GetNtextCtx: Int32;
begin
  Result := WhisperNtextCtx(FCtx);
end;

function TWhisper.GetNvocab: Int32;
begin
  Result := WhisperNvocab(FCtx);
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
//  PCtx: TWhisperContext;
  CParams: TWhisperContextParams;
  PParams: PWhisperContextParams;
begin
  PParams := WhisperContextDefaultParams;
  CParams := PParams^;
  Self.FCParams := CParams;
  FCtx := WhisperInitFromFileWithParams(PModel, @FCParams);
end;

end.
