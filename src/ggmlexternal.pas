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
  DynLib, ggmlTypes;

var
  GgmlLibrary: TDynLib;
  GgmlLibraryIsLoaded: Boolean;

  GgmlBackendLoad: function (const BackendLibrary: PAnsiChar): PGgmlBackendReg; CDecl;
  GgmlBackendLoadAll: procedure (); CDecl;
const
  {$IF DEFINED(OS_WIN64)}
  GGMLLibraryName = 'ggml.dll';
  {$ELSEIF DEFINED(LINUX64)}
  GGMLLibraryName = 'libggml.so';
  {$ELSEIF DEFINED(OSXARM64)}
  GGMLLibraryName = 'libggml.dylib';
  {$ELSEIF DEFINED(OSX64)}
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

  FreeAndNil(GgmlLibrary);
end;

procedure InitializeGgmlLibrary;
begin
  FinalizeGgmlLibrary;

  GgmlLibrary := Nil;

  if GgmlLibraryName <> '' then
    begin
      if FileExists(GgmlLibraryName) then
        GgmlLibrary := TDynLib.Load(GgmlLibraryName, true)
      else
        Raise Exception.Create(GgmlLibraryName + ' not found');
    end;

  if GgmlLibrary <> Nil then
    begin
      Pointer({$ifndef FPC}@{$endif} GgmlBackendLoad) := GgmlLibrary.Symbol('ggml_backend_load');
      if @GgmlBackendLoad = Nil then raise Exception.Create('GgmlBackendLoad failed to load');
      Pointer({$ifndef FPC}@{$endif} GgmlBackendLoadAll) := GgmlLibrary.Symbol('ggml_backend_load_all');
      if @GgmlBackendLoadAll = Nil then raise Exception.Create('GgmlBackendLoadAll failed to load');

      GgmlLibraryIsLoaded := True;

    end
  else
    Raise Exception.Create('Couldn''t import ggml library');

end;

initialization
  InitializeGgmlLibrary;

finalization
  FinalizeGgmlLibrary;

end.
