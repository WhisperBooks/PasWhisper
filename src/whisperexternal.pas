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

  WhisperInitFromFileWithParams: function (const AModelFile: PAnsiChar; const params: PWhisperContextParams): PWhisperContext; CDecl;
  WhisperContextDefaultParams: function(): PWhisperContextParams; CDecl;
  WhisperFree: procedure (ctx: PWhisperContext); CDecl;
{
    WHISPER_API struct whisper_context * whisper_init_from_file_with_params  (const char * path_model,              struct whisper_context_params params);
    WHISPER_API struct whisper_context * whisper_init_from_buffer_with_params(void * buffer, size_t buffer_size,    struct whisper_context_params params);
    WHISPER_API struct whisper_context * whisper_init_with_params            (struct whisper_model_loader * loader, struct whisper_context_params params);
}
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
    end
  else
    Raise Exception.Create('Couldn''t import whisper library');

end;

initialization
  InitializeWhisperLibrary;

finalization
  FinalizeWhisperLibrary;
end.
