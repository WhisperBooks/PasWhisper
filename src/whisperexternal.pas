unit WhisperExternal;

{$I platform.inc}

{$IFDEF FPC}
  {$packrecords C}
{$ELSE}
  {$ALIGN 4}
{$ENDIF}
{$MinEnumSize 4}

interface

{$ALIGN 4}
uses
  SysUtils,
  WhisperTypes,
  WhisperDynLib, ggmlTypes;


var
  WhisperLibrary: TDynLib;
  WhisperLibraryIsLoaded: Boolean;

  WhisperInitFromFileWithParams: function (const AModelFile: PAnsiChar; const params: TWhisperContextParams): TWhisperContext; CDecl;
  WhisperInitFromFileWithParamsNoState: function (const AModelFile: PAnsiChar; const params: TWhisperContextParams): TWhisperContext; CDecl;
  WhisperContextDefaultParams: function(): TWhisperContextParams; CDecl;
  WhisperFree: procedure (ctx: TWhisperContext); CDecl;
  WhisperInitState: function(Ctx: TWhisperContext): TWhisperState; CDecl;
  WhisperFreeState: procedure(State: TWhisperState); CDecl;

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

    // This can be used to set a custom log mel spectrogram inside the default state of the provided whisper context.
    // Use this instead of whisper_pcm_to_mel() if you want to provide your own log mel spectrogram.
    // n_mel must be 80
    // Returns 0 on success
    WhisperSetMel: function(Ctx: TWhisperContext; const Data: PFloat; NLen: Int32; NMel: Int32): Int32; CDecl;
    WhisperSetMelWithState: function(Ctx: TWhisperContext; State: TWhisperState; const Data: PFloat; NLen: Int32; NMel: Int32): Int32; CDecl;

    // Run the Whisper encoder on the log mel spectrogram stored inside the default state in the provided whisper context.
    // Make sure to call whisper_pcm_to_mel() or whisper_set_mel() first.
    // offset can be used to specify the offset of the first frame in the spectrogram.
    // Returns 0 on success
    WhisperEncode: function(Ctx: TWhisperContext; Offset: Int32; NThreads: Int32): Int32; CDecl;
    WhisperEncodeWithState: function(Ctx: TWhisperContext; State: TWhisperState; Offset: Int32; NThreads: Int32): Int32; CDecl;

    // Run the Whisper decoder to obtain the logits and probabilities for the next token.
    // Make sure to call whisper_encode() first.
    // tokens + n_tokens is the provided context for the decoder.
    // n_past is the number of tokens to use from previous decoder calls.
    // Returns 0 on success
    // TODO: add support for multiple decoders
    WhisperDecode: function(Ctx: TWhisperContext; Tokens: PWhisperToken; NTokens: Int32; NPast: Int32; NThreads: Int32): Int32; CDecl;
    WhisperDecodeWithState: function(Ctx: TWhisperContext; State: TWhisperState; Tokens: PWhisperToken; NTokens: Int32; NPast: Int32; NThreads: Int32): Int32; CDecl;

    // Convert the provided text into tokens.
    // The tokens pointer must be large enough to hold the resulting tokens.
    // Returns the number of tokens on success, no more than n_max_tokens
    // Returns a negative number on failure - the number of tokens that would have been returned
    // TODO: not sure if correct
    WhisperTokenize: function(Ctx: TWhisperContext; Text: PAnsiString; Tokens: PWhisperToken; MaxTokens: Int32): Int32; CDecl;

    // Return the number of tokens in the provided text
    // Equivalent to: -whisper_tokenize(ctx, text, NULL, 0)
    WhisperTokenCount: function(Ctx: TWhisperContext; Text: PAnsiString): Int32; CDecl;

    // Largest language id (i.e. number of available languages - 1)
    WhisperLangMaxId: function : Int32; CDecl;

    // Return the id of the specified language, returns -1 if not found
    // Examples:
    //   "de" -> 2
    //   "german" -> 2
    WhisperLangId: function(const Lang: PAnsiString): Int32; CDecl;

    // Return the short string of the specified language id (e.g. 2 -> "de"), returns nullptr if not found
    WhisperLangStr: function(id: Int32): PAnsiString; CDecl;

    // Return the short string of the specified language name (e.g. 2 -> "german"), returns nullptr if not found
    WhisperLangStrFull: function(id: Int32): PAnsiString; CDecl;

    // Use mel data at offset_ms to try and auto-detect the spoken language
    // Make sure to call whisper_pcm_to_mel() or whisper_set_mel() first
    // Returns the top language id or negative on failure
    // If not null, fills the lang_probs array with the probabilities of all languages
    // The array must be whisper_lang_max_id() + 1 in size
    // ref: https://github.com/openai/whisper/blob/main/whisper/decoding.py#L18-L69
    WhisperLangAutoDetect: function(Ctx: TWhisperContext; OffsetMs: Int32; NThreads: Int32; LangProbs: PFloat): Int32; CDecl;
    WhisperLangAutoDetectWithState: function(Ctx: TWhisperContext; State: TWhisperState; OffsetMs: Int32; NThreads: Int32; LangProbs: PFloat): Int32; CDecl;

    WhisperGetStateFromContext: function(Ctx: TWhisperContext): TWhisperState; CDecl;
    WhisperGetActivityWithState: function(State: TWhisperState): PWhisperActivity; CDecl;
    WhisperGetSystemInfoJson: function(): PChar; CDecl;

    WhisperGetBackendCount: function(State: TWhisperState): Integer; CDecl;
    WhisperGetIndexedBackend: function(State: TWhisperState; Index: Integer): PGgmlBackend; CDecl;
    WhisperGetPreferredBackend: function(State: TWhisperState): PGgmlBackend; CDecl;

    WhisperPrintTimings: procedure(Ctx: TWhisperContext); CDecl;
    WhisperResetTimings: procedure(Ctx: TWhisperContext); CDecl;

const
  {$IF DEFINED(OS_WIN64)}
  WhisperLibraryName = 'whisper.dll';
  {$ELSEIF DEFINED(OS_WIN32)}
  WhisperLibraryName = 'whisper.dll';
  {$ELSEIF DEFINED(OS_LINUX64)}
  WhisperLibraryName = 'libwhisper.so';
  {$ELSEIF DEFINED(OS_LINUX64ARM)}
  WhisperLibraryName = 'libwhisper.so';
  {$ELSEIF DEFINED(OS_OSX64ARM)}
  WhisperLibraryName = 'libwhisper.dylib';
  {$ELSEIF DEFINED(OS_OSX64)}
  WhisperLibraryName = 'libwhisper.dylib';
  {$ENDIF}

procedure InitializeWhisperLibrary;
procedure FinalizeWhisperLibrary;

implementation

uses
  WhisperUtils;


procedure FinalizeWhisperLibrary;
begin
  WhisperLibraryIsLoaded := False;

  Pointer({$ifndef FPC}@{$endif} WhisperInitFromFileWithParams) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperInitFromFileWithParamsNoState) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperContextDefaultParams) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperFree) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperInitState)    := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperFreeState)    := Nil;

  Pointer({$ifndef FPC}@{$endif} WhisperNlen) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperNlenFromState) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperNvocab) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperNtextCtx) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperNaudioCtx) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperIsMultilingual) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModelNvocab) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModelNaudioCtx) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModelNaudioState) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModelNaudioHead) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModelNaudioLayer) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModelNtextCtx) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModelNtextState) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModelNtextHead) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModelNtextLayer) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModelNmels) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModelftype) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModeltype) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperGetLogits) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperGetLogitsFromState) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenToStr) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperModelTypeReadable) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenEot) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenSot) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenSolm) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenPrev) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenNosp) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenNot) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenBeg) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenLang) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenTranslate) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenTranscribe) := Nil;

  Pointer({$ifndef FPC}@{$endif} WhisperSetMel)          := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperSetMelWithState) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperEncode)          := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperEncodeWithState) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperDecode)          := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperDecodeWithState) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenize)        := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperTokenCount)      := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperLangMaxId)       := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperLangId)          := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperLangStr)         := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperLangStrFull)     := Nil;

  Pointer({$ifndef FPC}@{$endif} WhisperPrintTimings)    := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperResetTimings)    := Nil;

  Pointer({$ifndef FPC}@{$endif} WhisperGetActivityWithState) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperGetStateFromContext) := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperGetSystemInfoJson)   := Nil;

  Pointer({$ifndef FPC}@{$endif} WhisperGetBackendCount)     := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperGetIndexedBackend)   := Nil;
  Pointer({$ifndef FPC}@{$endif} WhisperGetPreferredBackend) := Nil;

  FreeAndNil(WhisperLibrary);
end;

procedure InitializeWhisperLibrary;

begin
  FinalizeWhisperLibrary;

  WhisperLibrary := TDynLib.Load(WhisperLibraryName, true);

  if WhisperLibrary <> Nil then
    begin
      WhisperLibrary.SymbolError := seRaise;
      Pointer({$ifndef FPC}@{$endif} WhisperInitFromFileWithParams) := WhisperLibrary.Symbol('whisper_init_from_file_with_params');
      Pointer({$ifndef FPC}@{$endif} WhisperInitFromFileWithParamsNoState) := WhisperLibrary.Symbol('whisper_init_from_file_with_params_no_state');
      Pointer({$ifndef FPC}@{$endif} WhisperContextDefaultParams) := WhisperLibrary.Symbol('whisper_context_default_params');
      Pointer({$ifndef FPC}@{$endif} WhisperFree) := WhisperLibrary.Symbol('whisper_free');
      Pointer({$ifndef FPC}@{$endif} WhisperInitState)    := WhisperLibrary.Symbol('whisper_init_state');
      Pointer({$ifndef FPC}@{$endif} WhisperFreeState)    := WhisperLibrary.Symbol('whisper_free_state');

      Pointer({$ifndef FPC}@{$endif} WhisperNlen) := WhisperLibrary.Symbol('whisper_n_len');
      Pointer({$ifndef FPC}@{$endif} WhisperNlenFromState) := WhisperLibrary.Symbol('whisper_n_len_from_state');
      Pointer({$ifndef FPC}@{$endif} WhisperNvocab) := WhisperLibrary.Symbol('whisper_n_vocab');
      Pointer({$ifndef FPC}@{$endif} WhisperNtextCtx) := WhisperLibrary.Symbol('whisper_n_text_ctx');
      Pointer({$ifndef FPC}@{$endif} WhisperNaudioCtx) := WhisperLibrary.Symbol('whisper_n_audio_ctx');
      Pointer({$ifndef FPC}@{$endif} WhisperIsMultilingual) := WhisperLibrary.Symbol('whisper_is_multilingual');
      Pointer({$ifndef FPC}@{$endif} WhisperModelNvocab) := WhisperLibrary.Symbol('whisper_model_n_vocab');
      Pointer({$ifndef FPC}@{$endif} WhisperModelNaudioCtx) := WhisperLibrary.Symbol('whisper_model_n_audio_ctx');
      Pointer({$ifndef FPC}@{$endif} WhisperModelNaudioState) := WhisperLibrary.Symbol('whisper_model_n_audio_state');
      Pointer({$ifndef FPC}@{$endif} WhisperModelNaudioHead) := WhisperLibrary.Symbol('whisper_model_n_audio_head');
      Pointer({$ifndef FPC}@{$endif} WhisperModelNaudioLayer) := WhisperLibrary.Symbol('whisper_model_n_audio_layer');
      Pointer({$ifndef FPC}@{$endif} WhisperModelNtextCtx) := WhisperLibrary.Symbol('whisper_model_n_text_ctx');
      Pointer({$ifndef FPC}@{$endif} WhisperModelNtextState) := WhisperLibrary.Symbol('whisper_model_n_text_state');
      Pointer({$ifndef FPC}@{$endif} WhisperModelNtextHead) := WhisperLibrary.Symbol('whisper_model_n_text_head');
      Pointer({$ifndef FPC}@{$endif} WhisperModelNtextLayer) := WhisperLibrary.Symbol('whisper_model_n_text_layer');
      Pointer({$ifndef FPC}@{$endif} WhisperModelNmels) := WhisperLibrary.Symbol('whisper_model_n_mels');
      Pointer({$ifndef FPC}@{$endif} WhisperModelftype) := WhisperLibrary.Symbol('whisper_model_ftype');
      Pointer({$ifndef FPC}@{$endif} WhisperModeltype) := WhisperLibrary.Symbol('whisper_model_type');
      Pointer({$ifndef FPC}@{$endif} WhisperGetLogits) := WhisperLibrary.Symbol('whisper_get_logits');
      Pointer({$ifndef FPC}@{$endif} WhisperGetLogitsFromState) := WhisperLibrary.Symbol('whisper_get_logits_from_state');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenToStr) := WhisperLibrary.Symbol('whisper_token_to_str');
      Pointer({$ifndef FPC}@{$endif} WhisperModelTypeReadable) := WhisperLibrary.Symbol('whisper_model_type_readable');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenEot) := WhisperLibrary.Symbol('whisper_token_eot');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenSot) := WhisperLibrary.Symbol('whisper_token_sot');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenSolm) := WhisperLibrary.Symbol('whisper_token_solm');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenPrev) := WhisperLibrary.Symbol('whisper_token_prev');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenNosp) := WhisperLibrary.Symbol('whisper_token_nosp');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenNot) := WhisperLibrary.Symbol('whisper_token_not');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenBeg) := WhisperLibrary.Symbol('whisper_token_beg');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenLang) := WhisperLibrary.Symbol('whisper_token_lang');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenTranslate) := WhisperLibrary.Symbol('whisper_token_translate');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenTranscribe) := WhisperLibrary.Symbol('whisper_token_transcribe');

      Pointer({$ifndef FPC}@{$endif} WhisperSetMel)          := WhisperLibrary.Symbol('whisper_set_mel');
      Pointer({$ifndef FPC}@{$endif} WhisperSetMelWithState) := WhisperLibrary.Symbol('whisper_set_mel_with_state');
      Pointer({$ifndef FPC}@{$endif} WhisperEncode)          := WhisperLibrary.Symbol('whisper_encode');
      Pointer({$ifndef FPC}@{$endif} WhisperEncodeWithState) := WhisperLibrary.Symbol('whisper_encode_with_state');
      Pointer({$ifndef FPC}@{$endif} WhisperDecode)          := WhisperLibrary.Symbol('whisper_decode');
      Pointer({$ifndef FPC}@{$endif} WhisperDecodeWithState) := WhisperLibrary.Symbol('whisper_decode_with_state');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenize)        := WhisperLibrary.Symbol('whisper_tokenize');
      Pointer({$ifndef FPC}@{$endif} WhisperTokenCount)      := WhisperLibrary.Symbol('whisper_token_count');
      Pointer({$ifndef FPC}@{$endif} WhisperLangMaxId)       := WhisperLibrary.Symbol('whisper_lang_max_id');
      Pointer({$ifndef FPC}@{$endif} WhisperLangId)          := WhisperLibrary.Symbol('whisper_lang_id');
      Pointer({$ifndef FPC}@{$endif} WhisperLangStr)         := WhisperLibrary.Symbol('whisper_lang_str');
      Pointer({$ifndef FPC}@{$endif} WhisperLangStrFull)     := WhisperLibrary.Symbol('whisper_lang_str_full');
      Pointer({$ifndef FPC}@{$endif} WhisperLangAutoDetect)  := WhisperLibrary.Symbol('whisper_lang_auto_detect');
      Pointer({$ifndef FPC}@{$endif} WhisperLangAutoDetectWithState) := WhisperLibrary.Symbol('whisper_lang_auto_detect_with_state');

      Pointer({$ifndef FPC}@{$endif} WhisperPrintTimings)    := WhisperLibrary.Symbol('whisper_print_timings');
      Pointer({$ifndef FPC}@{$endif} WhisperResetTimings)    := WhisperLibrary.Symbol('whisper_reset_timings');

      Pointer({$ifndef FPC}@{$endif} WhisperGetStateFromContext) := WhisperLibrary.Symbol('whisper_flat_get_state_from_context');
      Pointer({$ifndef FPC}@{$endif} WhisperGetActivityWithState) := WhisperLibrary.Symbol('whisper_flat_get_activity_with_state');
      Pointer({$ifndef FPC}@{$endif} WhisperGetSystemInfoJson)   := WhisperLibrary.Symbol('whisper_flat_get_system_info_json');

      Pointer({$ifndef FPC}@{$endif} WhisperGetBackendCount)   := WhisperLibrary.Symbol('whisper_flat_get_backend_count');
      Pointer({$ifndef FPC}@{$endif} WhisperGetIndexedBackend)   := WhisperLibrary.Symbol('whisper_flat_get_indexed_backend');
      Pointer({$ifndef FPC}@{$endif} WhisperGetPreferredBackend)   := WhisperLibrary.Symbol('whisper_flat_get_preferred_backend');

      WhisperLibraryIsLoaded := True;

    end
  else
    Raise Exception.Create('Couldn''t import whisper library');

end;

Initialization

Finalization
  if WhisperLibraryIsLoaded then
    begin
      FinalizeWhisperLibrary;
      // TODO : Unload Libray?
    end;

end.
