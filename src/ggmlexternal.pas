unit GgmlExternal;

{$I platform.inc}

interface

{$ALIGN 4}
uses
  SysUtils,
  DynLib, ggmlTypes;

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

implementation


end.
