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
    WhisperDecode: function(Ctx: TWhisperContext; Tokens: PWhisperTokens; NTokens: Int32; NPast: Int32; NThreads: Int32): Int32; CDecl;
    WhisperDecodeWithState: function(Ctx: TWhisperContext; State: TWhisperState; Tokens: PWhisperTokens; NTokens: Int32; NPast: Int32; NThreads: Int32): Int32; CDecl;

    // Convert the provided text into tokens.
    // The tokens pointer must be large enough to hold the resulting tokens.
    // Returns the number of tokens on success, no more than n_max_tokens
    // Returns a negative number on failure - the number of tokens that would have been returned
    // TODO: not sure if correct
    WhisperTokenize: function(Ctx: TWhisperContext; Text: PAnsiString; Tokens: PWhisperTokens; MaxTokens: Int32): Int32; CDecl;

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

  Pointer(@WhisperSetMel)          := Nil;
  Pointer(@WhisperSetMelWithState) := Nil;
  Pointer(@WhisperEncode)          := Nil;
  Pointer(@WhisperEncodeWithState) := Nil;
  Pointer(@WhisperDecode)          := Nil;
  Pointer(@WhisperDecodeWithState) := Nil;
  Pointer(@WhisperTokenize)        := Nil;
  Pointer(@WhisperTokenCount)      := Nil;
  Pointer(@WhisperLangMaxId)       := Nil;
  Pointer(@WhisperLangId)          := Nil;
  Pointer(@WhisperLangStr)         := Nil;
  Pointer(@WhisperLangStrFull)     := Nil;

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
      WhisperLibrary.SymbolError := seRaise;
      Pointer(@WhisperInitFromFileWithParams) := WhisperLibrary.Symbol('whisper_init_from_file_with_params');
      Pointer(@WhisperContextDefaultParams) := WhisperLibrary.Symbol('whisper_context_default_params');
      Pointer(@WhisperFree) := WhisperLibrary.Symbol('whisper_free');

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

      Pointer(@WhisperSetMel)          := WhisperLibrary.Symbol('whisper_set_mel');
      Pointer(@WhisperSetMelWithState) := WhisperLibrary.Symbol('whisper_set_mel_with_state');
      Pointer(@WhisperEncode)          := WhisperLibrary.Symbol('whisper_encode');
      Pointer(@WhisperEncodeWithState) := WhisperLibrary.Symbol('whisper_encode_with_state');
      Pointer(@WhisperDecode)          := WhisperLibrary.Symbol('whisper_decode');
      Pointer(@WhisperDecodeWithState) := WhisperLibrary.Symbol('whisper_decode_with_state');
      Pointer(@WhisperTokenize)        := WhisperLibrary.Symbol('whisper_tokenize');
      Pointer(@WhisperTokenCount)      := WhisperLibrary.Symbol('whisper_token_count');
      Pointer(@WhisperLangMaxId)       := WhisperLibrary.Symbol('whisper_lang_max_id');
      Pointer(@WhisperLangId)          := WhisperLibrary.Symbol('whisper_lang_id');
      Pointer(@WhisperLangStr)         := WhisperLibrary.Symbol('whisper_lang_str');
      Pointer(@WhisperLangStrFull)     := WhisperLibrary.Symbol('whisper_lang_str_full');
      Pointer(@WhisperLangAutoDetect)  := WhisperLibrary.Symbol('whisper_lang_auto_detect');
      Pointer(@WhisperLangAutoDetectWithState) := WhisperLibrary.Symbol('whisper_lang_auto_detect_with_state');

    end
  else
    Raise Exception.Create('Couldn''t import whisper library');

end;

initialization
  InitializeWhisperLibrary;

finalization
  FinalizeWhisperLibrary;
end.
