unit GgmlExternal;


{$ALIGN 4}
{$MinEnumSize 4}
interface

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
  {$IF DEFINED(MSWINDOWS)}
  GGMLLibraryName = 'ggml.dll';
  {$ELSEIF DEFINED(LINUX64)}
  GGMLLibraryName = 'libggml.so';
  {$ELSEIF DEFINED(MACOS)}
  GGMLLibraryName = 'libggml.dylib';
  {$ENDIF}

procedure InitializeGgmlLibrary;
procedure FinalizeGgmlLibrary;


implementation

procedure FinalizeGgmlLibrary;
begin
  GgmlLibraryIsLoaded := False;

  Pointer(@GgmlBackendLoad) := Nil;
  Pointer(@GgmlBackendLoadAll) := Nil;
  Pointer(@GgmlBackendLoadAllFromPath) := Nil;
  Pointer(@GgmlBackendTryLoadBest) := Nil;
  Pointer(@GgmlBackendGetDeviceCount)   := Nil;

  FreeAndNil(GgmlLibrary);
end;

procedure InitializeGgmlLibrary;
begin
  FinalizeGgmlLibrary;

  GgmlLibrary := TDynLib.Load(GgmlLibraryName, '', true);

  if GgmlLibrary <> Nil then
    begin
      GgmlLibrary.SymbolError := seRaise;
      Pointer(@GgmlBackendLoad) := GgmlLibrary.Symbol('ggml_backend_load');
      Pointer(@GgmlBackendLoadAll) := GgmlLibrary.Symbol('ggml_backend_load_all');
      Pointer(@GgmlBackendLoadAllFromPath) := GgmlLibrary.Symbol('ggml_backend_load_all_from_path');
      Pointer(@GgmlBackendTryLoadBest)   := GgmlLibrary.Symbol('ggml_backend_try_load_best');
      Pointer(@GgmlBackendGetDeviceCount)   := GgmlLibrary.Symbol('ggml_backend_dev_count');
      GgmlLibraryIsLoaded := True;

    end
  else
    Raise Exception.Create('Couldn''t import ggml library');

end;
{
Initialization

Finalization

  if GgmlLibraryIsLoaded then
    begin
      FinalizeGgmlLibrary;
      // TODO : Unload Libray?
    end;
}

end.
