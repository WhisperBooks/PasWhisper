unit whisper;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses SysUtils, WhisperExternal, WhisperTypes, ggmlTypes, WhisperDynlib;

type
  TWhisper = class
  strict private
    FCtx: TWhisperContext;
    FState: TWhisperState;
    FModel: String;
    FCParams: TWhisperContextParams;
    FContextHasState: Boolean;
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
    function  GetActivity: PWhisperActivity;
    procedure FreeState;
    procedure PrintTimings;
    procedure ResetTimings;
    procedure LoadBackends;
    function  GetSystemInfoJson: String;
    function  LoadBestBackend(const ADeviceType: String; const APath: String = ''): PGgmlBackendReg;
    function  LoadModel(const AModel: String; const WithState: Boolean = False; const NoGPU: Boolean = False; const useFlash: Boolean = False): Boolean;
    function  SetMel(const Data: PFloat; NLen, NMel: Integer): Integer;
    function  Encode(const Offset, NThreads: Integer): Integer;
    function  Decode(Tokens: PWhisperToken; const NTokens, NPast, NThreads: Integer): Integer; overload;
    function  Decode(Tokens: TWhisperTokenArray; const NTokens, NPast, NThreads: Integer): Integer; overload;
    function  LangAutoDetect(OffsetMs: Int32; NThreads: Int32; LangProbs: PFloat): Integer;
    function  GetBackendCount: Integer;
    function  GetIndexedBackend(AIndex: Integer): TBackendDevice;
    function  GetPreferredBackend: TBackendDevice;
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
    property ContextHasState     : Boolean Read FContextHasState;
  end;

var
  PModel: PAnsiChar;

procedure SetWhisperLibraryPath(const APath: String = '');

implementation

uses
  {$IF DEFINED(MSWINDOWS)}
  Windows,
  {$ENDIF}
  WhisperLog,WhisperUtils,GgmlExternal;

{ TWhisper }

constructor TWhisper.Create;
begin
  if not GgmlLibraryIsLoaded then
    try
      InitializeGgmlLibrary;
    finally
      if Not WhisperLibraryIsLoaded then
        try
          InitializeWhisperLibrary;
        finally
          // Everything Loaded
        end;
    end
  else
    begin
      if Not WhisperLibraryIsLoaded then
        try
          InitializeWhisperLibrary;
        finally
          // Whisper Loaded
        end;
    end;
end;

function TWhisper.Decode(Tokens: TWhisperTokenArray; const NTokens, NPast,
  NThreads: Integer): Integer;
begin
  SafeMaskFPUExceptions(True);
  try
    if FContextHasState then
      Result := WhisperDecode(FCtx, @Tokens[0], NTokens, NPast, NThreads)
    else
      Result := WhisperDecodeWithState(FCtx, FState, @Tokens[0], NTokens, NPast, NThreads);
  finally
    SafeMaskFPUExceptions(False);
  end;
end;

function TWhisper.Decode(Tokens: PWhisperToken; const NTokens, NPast,
  NThreads: Integer): Integer;
begin
  SafeMaskFPUExceptions(True);
    try
      if FContextHasState then
        Result := WhisperDecode(FCtx, Tokens, NTokens, NPast, NThreads)
      else
        Result := WhisperDecodeWithState(FCtx, FState, Tokens, NTokens, NPast, NThreads);
    finally
      SafeMaskFPUExceptions(False);
  end;
end;

destructor TWhisper.Destroy;
begin
  if (FContextHasState = False) and (FState <> Nil) then
    FreeState;
  if(FCtx <> Nil) then
    WhisperFree(FCtx);

  inherited;
end;

function TWhisper.Encode(const Offset, NThreads: Integer): Integer;
begin
  SafeMaskFPUExceptions(True);
  try
    if FContextHasState then
      Result := WhisperEncode(FCtx, Offset, NThreads)
    else
      Result := WhisperEncodeWithState(FCtx, FState, Offset, NThreads);
  finally
     SafeMaskFPUExceptions(False);
  end;
end;

procedure TWhisper.FreeState;
begin
  if FState <> Nil then
    WhisperFreeState(FState);
  FState := Nil;
end;

function TWhisper.GetBackendCount: Integer;
begin
  if FState <> Nil then
    Result := WhisperGetBackendCount(FState)
  else
    Result := -1;
end;

function TWhisper.GetIndexedBackend(AIndex: Integer): TBackendDevice;
var
  backend: PGgmlBackend;
  device: PGgmlBackendDevice;
begin
  Result := default(TBackendDevice);
  if FState = Nil then
    Exit;

  backend := WhisperGetIndexedBackend(FState, AIndex);
  if backend = nil then
    Exit;

  device := backend.Device;
  if device = nil then
    Exit;

  Result.devName := String(device.IFace.GetName(device));
  Result.devDesc := String(device.IFace.GetDescription(device));
  device.IFace.GetMemory(device, @Result.memoryFree, @Result.memoryTotal);
  Result.devType := device.IFace.GetType(device);
end;
function TWhisper.GetIsMultilingual: Int32;
begin
  Result := WhisperIsMultilingual(FCtx);
end;

function TWhisper.GetSystemInfoJson: String;
begin
  Result := String(AnsiString(PAnsiChar(WhisperGetSystemInfoJson)));
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

function TWhisper.GetPreferredBackend: TBackendDevice;
var
  backend: PGgmlBackend;
  device: PGgmlBackendDevice;
begin
  Result := default(TBackendDevice);
  if FState = Nil then
    Exit;

  backend := WhisperGetPreferredBackend(FState);
  if backend = nil then
    Exit;

  device := backend.Device;
  if device = nil then
    Exit;

  Result.devName := String(device.IFace.GetName(device));
  Result.devDesc := String(device.IFace.GetDescription(device));
  device.IFace.GetMemory(device, @Result.memoryFree, @Result.memoryTotal);
  Result.devType := device.IFace.GetType(device);
end;

function TWhisper.GetActivity: PWhisperActivity;
begin
    Result := WhisperGetActivityWithState(FState);
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

function TWhisper.LangAutoDetect(OffsetMs, NThreads: Int32;
  LangProbs: PFloat): Integer;
begin
  if FContextHasState then
    Result := WhisperLangAutoDetect(FCtx, OffsetMs, NThreads, LangProbs)
  else
    Result := WhisperLangAutoDetectWithState(FCtx, FState, OffsetMs, NThreads, LangProbs);
end;

procedure TWhisper.LoadBackends;
begin
  SafeMaskFPUExceptions(True);
  try
    if WhisperGlobalLibraryPath.IsEmpty then
      GgmlBackendLoadAll
    else
      GgmlBackendLoadAllFromPath(PAnsiChar(Pointer(AnsiString(WhisperGlobalLibraryPath))));
  finally
    SafeMaskFPUExceptions(False);
  end;
end;

function TWhisper.LoadBestBackend(const ADeviceType, APath: String): PGgmlBackendReg;
var
  LPath: String;
  LLib: PGGMLBackendReg;
begin
  if WhisperGlobalLibraryPath.IsEmpty then
    LPath := APath
  else
    begin
      if APath.IsEmpty then
        LPath := WhisperGlobalLibraryPath
      else
        LPath := APath;
    end;

  SafeMaskFPUExceptions(True);
  try
    Result := Nil;

    DebugLog.Debug('Trying to load device "%s" library from "%s"', [ADeviceType, LPath]);
    LLib := GgmlBackendTryLoadBest(PAnsiChar(Pointer(AnsiString(ADeviceType))), PAnsiChar(Pointer(AnsiString(LPath))));
    if LLib <> Nil then
      DebugLog.Debug('API Version is %d', [LLib^.ApiVersion])
    else
      DebugLog.Debug('Load failed');

  finally
    SafeMaskFPUExceptions(False);
  end;
end;

function TWhisper.LoadModel(const AModel: String; const WithState: Boolean = False; const NoGPU: Boolean = False; const useFlash: Boolean = False): Boolean;
begin
  Result := False;
  if FileExists(AModel) then
    begin
      FModel := AModel;
      FCParams := WhisperContextDefaultParams;
      if NoGPU then
        FCParams.use_gpu := False;
      if useFlash then
        FCParams.flash_attn := True;
      if WithState then
        begin
          SafeMaskFPUExceptions(True);
          try
            FCtx := WhisperInitFromFileWithParamsNoState(PAnsiChar(Pointer(AnsiString(FModel))), FCParams);
            FState := WhisperInitState(FCtx);
          finally
            SafeMaskFPUExceptions(False);
          end;
          FContextHasState := False;
          Result := True;
        end
      else
        begin
          SafeMaskFPUExceptions(True);
          try
            FCtx := WhisperInitFromFileWithParams(PAnsiChar(Pointer(AnsiString(FModel))), FCParams);
            FState := WhisperGetStateFromContext(FCtx);
          finally
            SafeMaskFPUExceptions(False);
          end;
          FContextHasState := True;
          Result := True;
        end;
    end;
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
  if FContextHasState then
    Result := WhisperSetMel(FCtx, Data, NLen, NMel)
  else
    Result := WhisperSetMelWithState(FCtx, FState, Data, NLen, NMel);
end;

procedure SetWhisperLibraryPath(const APath: String = '');
begin
  WhisperGlobalLibraryPath := APath;
  DebugLog.Debug('Library Path Set To : "%s"', [APath]);
  {$IF DEFINED(MSWINDOWS)}
//  if IsDebuggerPresent() then
  SetDllDirectory(PWideChar(Pointer(AnsiString(APath))));
  {$ENDIF}

end;

initialization
  SetWhisperLibraryPath;

finalization


end.
