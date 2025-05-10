unit GgmlExternal;

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
  WhisperDynlib, ggmlTypes;

var
  GgmlLibrary: TDynLib;
  GgmlLibraryIsLoaded: Boolean;

  GgmlBackendLoad: function (const BackendLibrary: PAnsiChar): PGgmlBackendReg; CDecl;
  GgmlBackendLoadAll: procedure (); CDecl;
  GgmlBackendLoadAllFromPath: procedure (const LibraryPath: PAnsiChar = Nil); CDecl;
  GgmlBackendTryLoadBest: function (const BackendDeviceClass: PAnsiChar; const LibraryPath: PAnsiChar = Nil): PGgmlBackendReg; CDecl;
  GgmlBackendGetDeviceCount: function(): Int32; CDecl;
const
  {$IF DEFINED(OS_WIN64)}
  GGMLLibraryName = 'ggml.dll';
  {$ELSEIF DEFINED(OS_WIN32)}
  GGMLLibraryName = 'ggml.dll';
  {$ELSEIF DEFINED(OS_LINUX64)}
  GGMLLibraryName = 'libggml.so';
  {$ELSEIF DEFINED(OS_LINUX64ARM)}
  GGMLLibraryName = 'libggml.so';
  {$ELSEIF DEFINED(OS_OSX64ARM)}
  GGMLLibraryName = 'libggml.dylib';
  {$ELSEIF DEFINED(OS_OSX64)}
  GGMLLibraryName = 'libggml.dylib';
  {$ENDIF}

procedure InitializeGgmlLibrary;
procedure FinalizeGgmlLibrary;


implementation

procedure FinalizeGgmlLibrary;
begin
  GgmlLibraryIsLoaded := False;

  Pointer({$ifndef FPC}@{$endif} GgmlBackendLoad) := Nil;
  Pointer({$ifndef FPC}@{$endif} GgmlBackendLoadAll) := Nil;
  Pointer({$ifndef FPC}@{$endif} GgmlBackendLoadAllFromPath) := Nil;
  Pointer({$ifndef FPC}@{$endif} GgmlBackendTryLoadBest) := Nil;
  Pointer({$ifndef FPC}@{$endif} GgmlBackendGetDeviceCount)   := Nil;

  FreeAndNil(GgmlLibrary);
end;

procedure InitializeGgmlLibrary;
begin
  FinalizeGgmlLibrary;

  GgmlLibrary := TDynLib.Load(GgmlLibraryName, '', true);

  if GgmlLibrary <> Nil then
    begin
      GgmlLibrary.SymbolError := seRaise;
      Pointer({$ifndef FPC}@{$endif} GgmlBackendLoad) := GgmlLibrary.Symbol('ggml_backend_load');
      Pointer({$ifndef FPC}@{$endif} GgmlBackendLoadAll) := GgmlLibrary.Symbol('ggml_backend_load_all');
      Pointer({$ifndef FPC}@{$endif} GgmlBackendLoadAllFromPath) := GgmlLibrary.Symbol('ggml_backend_load_all_from_path');
      Pointer({$ifndef FPC}@{$endif} GgmlBackendTryLoadBest)   := GgmlLibrary.Symbol('ggml_backend_try_load_best');
      Pointer({$ifndef FPC}@{$endif} GgmlBackendGetDeviceCount)   := GgmlLibrary.Symbol('ggml_backend_dev_count');
      GgmlLibraryIsLoaded := True;

    end
  else
    Raise Exception.Create('Couldn''t import ggml library');

end;

Initialization

Finalization
  if GgmlLibraryIsLoaded then
    begin
      FinalizeGgmlLibrary;
      // TODO : Unload Libray?
    end;


end.
