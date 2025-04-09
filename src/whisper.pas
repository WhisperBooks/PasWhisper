unit whisper;

interface

uses SysUtils, WhisperExternal, WhisperTypes;

type
  TWhisper = class
  strict private
    FCtx: TWhisperContext;
    FState: TWhisperState;
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
    function GetLogits              : PFloat;
    function GetLogitsFromState     : PFloat;
    function GetTokenEot: TWhisperToken;
    function GetTokenSot: TWhisperToken;
    function GetTokenSolm: TWhisperToken;
    function GetTokenPrev: TWhisperToken;
    function GetTokenNosp: TWhisperToken;
    function GetTokenNot: TWhisperToken;
    function GetTokenBeg: TWhisperToken;
    function GetTokenTranslate: TWhisperToken;
    function GetTokenTranscribe: TWhisperToken;
  	function GetTokenLang(const LangId: Int32): TWhisperToken;
    function GetTokenToStr(const Token: TWhisperToken): PAnsiString;
    function GetModelTypeReadable: PAnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    function  GetTimings: PWhisperTimings;
    procedure PrintTimings;
    procedure ResetTimings;
    function  LoadModel(const AModel: String): Boolean;
    function  SetMel(const Data: PFloat; NLen, NMel: Integer): Integer;
    function  Encode(const Offset, NThreads: Integer): Integer;
    function  Decode(Tokens: PWhisperTokens; const NTokens, NPast, NThreads: Integer): Integer;
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
    property Logits              : PFloat Read GetLogits;
    property LogitsFromState     : PFloat Read GetLogitsFromState;
    property TokenEot		         : TWhisperToken Read GetTokenEot;
    property TokenSot            : TWhisperToken Read GetTokenSot;
    property TokenSolm           : TWhisperToken Read GetTokenSolm;
    property TokenPrev           : TWhisperToken Read GetTokenPrev;
    property TokenNosp           : TWhisperToken Read GetTokenNosp;
    property TokenNot            : TWhisperToken Read GetTokenNot;
    property TokenBeg            : TWhisperToken Read GetTokenBeg;
    property TokenTranslate      : TWhisperToken Read GetTokenTranslate;
    property TokenTranscribe     : TWhisperToken Read GetTokenTranscribe;
    property TokenLang[const LangId: Int32] : TWhisperToken Read GetTokenLang;
    property TokenToStr[const Token: TWhisperToken] : PAnsiString Read GetTokenToStr;
    property ModelTypeReadable   : PAnsiString Read GetModelTypeReadable;
 end;

var
  PModel: PAnsiChar;

implementation

{ TWhisper }

constructor TWhisper.Create;
begin
  if Not WhisperLibraryIsLoaded then
    Raise Exception.Create('Whisper library not available');
end;

function TWhisper.Decode(Tokens: PWhisperTokens; const NTokens, NPast,
  NThreads: Integer): Integer;
begin
  Result := WhisperDecode(FCtx, Tokens, NTokens, NPast, NThreads);
end;

destructor TWhisper.Destroy;
begin
  if(FCtx <> Nil) then
    WhisperFree(FCtx);

  inherited;
end;

function TWhisper.Encode(const Offset, NThreads: Integer): Integer;
begin
  Result := WhisperEncode(FCtx, Offset, NThreads);
end;

function TWhisper.GetIsMultilingual: Int32;
begin
  Result := WhisperIsMultilingual(FCtx);
end;

function TWhisper.GetLogits: PFloat;
begin
  Result := WhisperGetLogits(FCtx);
end;

function TWhisper.GetLogitsFromState: PFloat;
begin
  if FState = Nil then
    Exception.Create('GetLogitsFromState : State not set');
  Result := WhisperGetLogitsFromState(FState);
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

function TWhisper.GetModelTypeReadable: PAnsiString;
begin
  Result := WhisperModelTypeReadable(FCtx);
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
  if FState = Nil then
    Exception.Create('GetNlenFromState : State not set');
  Result := WhisperNlenFromState(FState);
end;

function TWhisper.GetNtextCtx: Int32;
begin
  Result := WhisperNtextCtx(FCtx);
end;

function TWhisper.GetNvocab: Int32;
begin
  Result := WhisperNvocab(FCtx);
end;

function TWhisper.GetTimings: PWhisperTimings;
begin
  Result := WhisperGetTimings(FCtx);
end;

function TWhisper.GetTokenBeg: TWhisperToken;
begin
  Result := WhisperTokenBeg(FCtx);
end;

function TWhisper.GetTokenEot: TWhisperToken;
begin
  Result := WhisperTokenEot(FCtx);
end;

function TWhisper.GetTokenLang(const LangId: Int32): TWhisperToken;
begin
  Result := WhisperTokenLang(FCtx, LangId);
end;

function TWhisper.GetTokenNosp: TWhisperToken;
begin
  Result := WhisperTokenNosp(FCtx);
end;

function TWhisper.GetTokenNot: TWhisperToken;
begin
  Result := WhisperTokenNot(FCtx);
end;

function TWhisper.GetTokenPrev: TWhisperToken;
begin
  Result := WhisperTokenPrev(FCtx);
end;

function TWhisper.GetTokenSolm: TWhisperToken;
begin
  Result := WhisperTokenSolm(FCtx);
end;

function TWhisper.GetTokenSot: TWhisperToken;
begin
  Result := WhisperTokenSot(FCtx);
end;

function TWhisper.GetTokenToStr(const Token: TWhisperToken): PAnsiString;
begin
  Result := WhisperTokenToStr(FCtx, Token);
end;

function TWhisper.GetTokenTranscribe: TWhisperToken;
begin
  Result := WhisperTokenTranscribe(FCtx);
end;

function TWhisper.GetTokenTranslate: TWhisperToken;
begin
  Result := WhisperTokenTranslate(FCtx);
end;

function TWhisper.LoadModel(const AModel: String): Boolean;
begin
  Result := False;
  if FileExists(AModel) then
    begin
      FModel := AModel;
      Init;
      Result := True;
    end;
end;

procedure TWhisper.Init;
var
  CParams: TWhisperContextParams;
  PParams: PWhisperContextParams;
begin
  FCParams := WhisperContextDefaultParams;
  FCtx := WhisperInitFromFileWithParams(PAnsiChar(Pointer(AnsiString(FModel))), @FCParams);
end;

procedure TWhisper.PrintTimings;
begin
  WhisperPrintTimings(FCtx);
end;

procedure TWhisper.ResetTimings;
begin
  WhisperResetTimings(FCtx);
end;

function TWhisper.SetMel(const Data: PFloat; NLen, NMel: Integer): Integer;
begin
  Result := WhisperSetMel(FCtx, Data, NLen, NMel);
end;

end.
