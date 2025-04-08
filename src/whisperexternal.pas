unit WhisperExternal;

interface

{$define RaiseExeptionOnImportError}
{$ALIGN 4}
uses
  System.Generics.Collections,
  SysUtils,
  WhisperTypes,
  DynLib, ggmlExternal;


var
  WhisperLibrary: TDynLib;

  WhisperInitFromFileWithParams: function (const AModelFile: PAnsiChar; const params: PWhisperContextParams): TWhisperContext; CDecl;
  WhisperContextDefaultParams: function(): PWhisperContextParams; CDecl;
  WhisperFree: procedure (ctx: TWhisperContext); CDecl;

{
    WHISPER_API struct whisper_context * whisper_init_from_file_with_params  (const char * path_model,              struct whisper_context_params params);
    WHISPER_API struct whisper_context * whisper_init_from_buffer_with_params(void * buffer, size_t buffer_size,    struct whisper_context_params params);
    WHISPER_API struct whisper_context * whisper_init_with_params            (struct whisper_model_loader * loader, struct whisper_context_params params);
}
    WhisperNlen                : function (Ctx: TWhisperContext): Int32; CDecl; // mel length
    WhisperNlenFromState       : function (State: TWhisperState): Int32; CDecl; // mel length
    WhisperNvocab              : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperNtextCtx            : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperNaudioCtx           : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperIsMultilingual      : function (Ctx: TWhisperContext): Int32; CDecl;

    WhisperModelNvocab         : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperModelNaudioCtx      : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperModelNaudioState    : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperModelNaudioHead     : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperModelNaudioLayer    : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperModelNtextCtx       : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperModelNtextState     : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperModelNtextHead      : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperModelNtextLayer     : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperModelNmels          : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperModelftype          : function (Ctx: TWhisperContext): Int32; CDecl;
    WhisperModeltype           : function (Ctx: TWhisperContext): Int32; CDecl;

    // Token logits obtained from the last call to whisper_decode()
    // The logits for the last token are stored in the last row
    // Rows: n_tokens
    // Cols: n_vocab
    WhisperGetLogits           : function (Ctx: TWhisperContext): PFloat; CDecl;
    WhisperGetLogitsFromState  : function (State: TWhisperState): PFloat; CDecl;

    // Token Id -> String. Uses the vocabulary in the provided context
    WhisperTokenToStr          : function (Ctx: TWhisperContext; Token: TWhisperToken): PAnsiString; CDecl;
    WhisperModelTypeReadable   : function (Ctx: TWhisperContext): PAnsiString; CDecl;


    // Special tokens
    WhisperTokenEot            : function (Ctx: TWhisperContext): TWhisperToken; CDecl;
    WhisperTokenSot            : function (Ctx: TWhisperContext): TWhisperToken; CDecl;
    WhisperTokenSolm           : function (Ctx: TWhisperContext): TWhisperToken; CDecl;
    WhisperTokenPrev           : function (Ctx: TWhisperContext): TWhisperToken; CDecl;
    WhisperTokenNosp           : function (Ctx: TWhisperContext): TWhisperToken; CDecl;
    WhisperTokenNot            : function (Ctx: TWhisperContext): TWhisperToken; CDecl;
    WhisperTokenBeg            : function (Ctx: TWhisperContext): TWhisperToken; CDecl;
    WhisperTokenLang           : function (Ctx: TWhisperContext; LangId: Int32): TWhisperToken; CDecl;

    // Task tokens
    WhisperTokenTranslate      : function (Ctx: TWhisperContext): TWhisperToken; CDecl;
    WhisperTokenTranscribe     : function (Ctx: TWhisperContext): TWhisperToken; CDecl;
const
  DLLPath = '';//../../../data/cpu/';

  WhisperLibraryName =
    {$if defined(MACOS)} // macOS
    'libwhisper.dylib'
    {$elseif defined(UNIX)}
    'libwhisper_api.so'
    {$elseif defined(MSWINDOWS) and defined(CPUX64)}
    DLLPath + 'whisper.dll'
    {$else}
    // Steam library not available on this platform
    ''
    {$endif};


procedure InitializeWhisperLibrary;
procedure FinalizeWhisperLibrary;

implementation

uses
  CheapLog;


procedure FinalizeWhisperLibrary;
begin
  Pointer(@WhisperInitFromFileWithParams) := Nil;
  Pointer(@WhisperContextDefaultParams) := Nil;
  Pointer(@WhisperFree) := Nil;

  Pointer(@WhisperNlen) := Nil;
  Pointer(@WhisperNlenFromState) := Nil;
  Pointer(@WhisperNvocab) := Nil;
  Pointer(@WhisperNtextCtx) := Nil;
  Pointer(@WhisperNaudioCtx) := Nil;
  Pointer(@WhisperIsMultilingual) := Nil;
  Pointer(@WhisperModelNvocab) := Nil;
  Pointer(@WhisperModelNaudioCtx) := Nil;
  Pointer(@WhisperModelNaudioState) := Nil;
  Pointer(@WhisperModelNaudioHead) := Nil;
  Pointer(@WhisperModelNaudioLayer) := Nil;
  Pointer(@WhisperModelNtextCtx) := Nil;
  Pointer(@WhisperModelNtextState) := Nil;
  Pointer(@WhisperModelNtextHead) := Nil;
  Pointer(@WhisperModelNtextLayer) := Nil;
  Pointer(@WhisperModelNmels) := Nil;
  Pointer(@WhisperModelftype) := Nil;
  Pointer(@WhisperModeltype) := Nil;
  Pointer(@WhisperGetLogits) := Nil;
  Pointer(@WhisperGetLogitsFromState) := Nil;
  Pointer(@WhisperTokenToStr) := Nil;
  Pointer(@WhisperModelTypeReadable) := Nil;
  Pointer(@WhisperTokenEot) := Nil;
  Pointer(@WhisperTokenSot) := Nil;
  Pointer(@WhisperTokenSolm) := Nil;
  Pointer(@WhisperTokenPrev) := Nil;
  Pointer(@WhisperTokenNosp) := Nil;
  Pointer(@WhisperTokenNot) := Nil;
  Pointer(@WhisperTokenBeg) := Nil;
  Pointer(@WhisperTokenLang) := Nil;
  Pointer(@WhisperTokenTranslate) := Nil;
  Pointer(@WhisperTokenTranscribe) := Nil;

  FreeAndNil(WhisperLibrary);
end;

procedure InitializeWhisperLibrary;
begin
  FinalizeWhisperLibrary;

  WhisperLibrary := Nil;

  if WhisperLibraryName <> '' then
    begin
      if FileExists(WhisperLibraryName) then
        WhisperLibrary := TDynLib.Load(WhisperLibraryName, true)
      else
        WriteLnLog(WhisperLibraryName + ' not found');
    end;

  if WhisperLibrary <> Nil then
    begin
      Pointer(@WhisperInitFromFileWithParams) := WhisperLibrary.Symbol('whisper_init_from_file_with_params');
      {$ifdef RaiseExeptionOnImportError}
      if Pointer(@WhisperInitFromFileWithParams) = Nil then
        Raise Exception.Create('Couldn''t link to whisper_init_from_file_with_params');
      {$endif}
      Pointer(@WhisperContextDefaultParams) := WhisperLibrary.Symbol('whisper_context_default_params');
      {$ifdef RaiseExeptionOnImportError}
      if Pointer(@WhisperInitFromFileWithParams) = Nil then
        Raise Exception.Create('Couldn''t link to whisper_context_default_params');
      {$endif}
      Pointer(@WhisperFree) := WhisperLibrary.Symbol('whisper_free');
      {$ifdef RaiseExeptionOnImportError}
      if Pointer(@WhisperInitFromFileWithParams) = Nil then
        Raise Exception.Create('Couldn''t link to whisper_free');
      {$endif}

      Pointer(@WhisperNlen) := WhisperLibrary.Symbol('whisper_n_len');
      Pointer(@WhisperNlenFromState) := WhisperLibrary.Symbol('whisper_n_len_from_state');
      Pointer(@WhisperNvocab) := WhisperLibrary.Symbol('whisper_n_vocab');
      Pointer(@WhisperNtextCtx) := WhisperLibrary.Symbol('whisper_n_text_ctx');
      Pointer(@WhisperNaudioCtx) := WhisperLibrary.Symbol('whisper_n_audio_ctx');
      Pointer(@WhisperIsMultilingual) := WhisperLibrary.Symbol('whisper_is_multilingual');
      Pointer(@WhisperModelNvocab) := WhisperLibrary.Symbol('whisper_model_n_vocab');
      Pointer(@WhisperModelNaudioCtx) := WhisperLibrary.Symbol('whisper_model_n_audio_ctx');
      Pointer(@WhisperModelNaudioState) := WhisperLibrary.Symbol('whisper_model_n_audio_state');
      Pointer(@WhisperModelNaudioHead) := WhisperLibrary.Symbol('whisper_model_n_audio_head');
      Pointer(@WhisperModelNaudioLayer) := WhisperLibrary.Symbol('whisper_model_n_audio_layer');
      Pointer(@WhisperModelNtextCtx) := WhisperLibrary.Symbol('whisper_model_n_text_ctx');
      Pointer(@WhisperModelNtextState) := WhisperLibrary.Symbol('whisper_model_n_text_state');
      Pointer(@WhisperModelNtextHead) := WhisperLibrary.Symbol('whisper_model_n_text_head');
      Pointer(@WhisperModelNtextLayer) := WhisperLibrary.Symbol('whisper_model_n_text_layer');
      Pointer(@WhisperModelNmels) := WhisperLibrary.Symbol('whisper_model_n_mels');
      Pointer(@WhisperModelftype) := WhisperLibrary.Symbol('whisper_model_ftype');
      Pointer(@WhisperModeltype) := WhisperLibrary.Symbol('whisper_model_type');
      Pointer(@WhisperGetLogits) := WhisperLibrary.Symbol('whisper_get_logits');
      Pointer(@WhisperGetLogitsFromState) := WhisperLibrary.Symbol('whisper_get_logits_from_state');
      Pointer(@WhisperTokenToStr) := WhisperLibrary.Symbol('whisper_token_to_str');
      Pointer(@WhisperModelTypeReadable) := WhisperLibrary.Symbol('whisper_model_type_readable');
      Pointer(@WhisperTokenEot) := WhisperLibrary.Symbol('whisper_token_eot');
      Pointer(@WhisperTokenSot) := WhisperLibrary.Symbol('whisper_token_sot');
      Pointer(@WhisperTokenSolm) := WhisperLibrary.Symbol('whisper_token_solm');
      Pointer(@WhisperTokenPrev) := WhisperLibrary.Symbol('whisper_token_prev');
      Pointer(@WhisperTokenNosp) := WhisperLibrary.Symbol('whisper_token_nosp');
      Pointer(@WhisperTokenNot) := WhisperLibrary.Symbol('whisper_token_not');
      Pointer(@WhisperTokenBeg) := WhisperLibrary.Symbol('whisper_token_beg');
      Pointer(@WhisperTokenLang) := WhisperLibrary.Symbol('whisper_token_lang');
      Pointer(@WhisperTokenTranslate) := WhisperLibrary.Symbol('whisper_token_translate');
      Pointer(@WhisperTokenTranscribe) := WhisperLibrary.Symbol('whisper_token_transcribe');
    end
  else
    Raise Exception.Create('Couldn''t import whisper library');

end;

initialization
  InitializeWhisperLibrary;

finalization
  FinalizeWhisperLibrary;
end.
