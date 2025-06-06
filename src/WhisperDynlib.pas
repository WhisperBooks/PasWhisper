{
  Copyright 2003-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Dynamic libraries loading (TDynLib). }
unit WhisperDynlib;

{$define LOGNOTRAISE}

interface

uses SysUtils, {$ifdef MSWINDOWS}Windows, {$endif}
     WhisperPlatform, Math, WhisperLog;

type
  EInternalError = class(Exception);
  TDynLibHandle = HModule;

const
  { Invalid TDynLibHandle value (meaning : LoadLibrary failed) }
  InvalidDynLibHandle: TDynLibHandle = 0;

  type
   { }
  EDynLibError = class(Exception);

  TDynLibSymbolError = (seRaise, seReturnNil, seWarnAndReturnNil);

  (*Load functions from dynamic libraries.

    This class allows to load functions from dynamic libraries (.dll on Windows,
    .so on most Unix platforms, .dylib on macOS and iOS).

    Features:

    @unorderedList(
      @item(The @link(Load) and @link(Symbol) functions
        @italic(by default do error checking) (and raise necessary exceptions).)

      @item(The field SymbolError allows to
        specify @italic(once for all subsequent Symbol calls)
        what error checking we want.
        Default is to check errors and raise exceptions.
        There is also a very usefull value reWarnAndContinue:
        it allows you to run program @italic(once) and see all symbols that
        are missing from dynamic library.)

      @item(The interface of this is OS-independent and works for
        Delphi.)

      @item(macOS-specific extra feature: when loading a library, we also look
        for it inside the bundle. This allows to distribute macOS dynamic
        libraries (libxxx.dylib) simply inside the application bundle,
        in Contents/MacOS/ directory (alongside executable).
        This is useful to distribute libraries like libpng, libvorbisfile,
        libsteam_api with your application.)
    )

    Typical usage:

    @longCode(#
      var
        ALLibrary: TDynLib = nil;
      initialization
        ALLibrary := TDynLib.Load('libopenal.so.1');
        { ... some calls to ALLibrary.Symbol() ... }
      finalization
        FreeAndNil(ALLibrary);
      end.
    #)

    It is important that ALLibrary is initialized to nil and that in finalization
    you use FreeAndNil. This allows you to exit gracefully if library does not
    exist on the system and @link(Load) will raise an exception: ALLibrary will
    stay then as nil.
  *)
  TDynLib = class
  private
    { In this class, we always have a valid FHandle. }
    FHandle: TDynLibHandle;
    FName: string;
    FSymbolError: TDynLibSymbolError;
  public
    { Standard constructor, requires a valid TDynLibHandle already.
      Usually you will prefer to use @link(Load) method instead of
      directly calling this constructor.

      @raises(ECheckFailed if you supply invalid handle.) }
    constructor Create(const AName: string; AHandle: TDynLibHandle);
    destructor Destroy; override;

    { Name of the library to link to. In practice, file name of the *.so
      or *.dylib or *.dll file.

      A precise strategy where this library is searched
      is specific to a platform, see the semantics of SysUtils.LoadLibrary
      call on given OS. }
    property Name: string read FName;

    { Link to a dynamic library specified by Name. Returns created
      TDynLib instance.

      If the library is not found and RaiseExceptionOnError is @false,
      @nil will be returned.
      If RaiseExceptionOnError is @true then EDynLibError will be raised
      in case library is not found.
      So if RaiseExceptionOnError is @true, @nil is
      never returned.

      Note that the default situation prevents from unintentionally ignoring
      an error and @italic(that's good).

      @raises(EDynLibError If library not found and RaiseExceptionOnError
        is @true.) }
    class function Load(const AName: string; const APath: String = ''; RaiseExceptionOnError: boolean = true): TDynLib;

    { What happens when @link(Symbol) fails. }
    property SymbolError: TDynLibSymbolError
      read FSymbolError write FSymbolError default seRaise;

    { Return address of given symbol (function name etc.) from loaded dynamic
      library. If the symbol doesn't exist, then SymbolError
      says what happens:

      @unorderedList(
        @item(seRaise (default), then EDynLibError will be raised.)
        @item(seReturnNil, then return @nil (and continue, ignoring error).)
        @item(seWarnAndReturnNil, then write warning (using WarningWrite)
          and return @nil (and continue, ignoring error).

          This is useful for debugging : you can easily open the library and after
          one run of the program you can see what symbols (that you requested)
          were missing from the library. This is useful when you have a library
          but you are not sure whether it is compatible and contains all the
          symbols that you want.)
      )

      @raises(EDynLibError If SymbolName doesn't exist and
        SymbolError is seRaise.)

    }
    function Symbol(const SymbolName: PChar): Pointer;
  end;

var
  { Disable all dynamic libraries loading.
    All TDynLib.Load calls will return InvalidDynLibHandle.
    This is useful:

    @orderedList(
      @item(For testing purposes. CGE applications should gracefully work
        even without various dynamic libraries.
        E.g. without OpenAL library, no sound is played.
        Without LibPng we use PNG internal implementation using FpImage.
      )

      @item(By the build tool. Commands like "castle-engine compile"
        and "castle-engine clear" want to delete/override dll files,
        but it is impossible on Windows, because the build tool would use dll
        files in the project's directory.

        The build tool doesn't need dynamic libraries for anything,
        but some code pulled by dependencies could accidentally load them.
        E.g. OpenAL unit loads it in initialization.
      )
    )
  }
  InternalDisableDynamicLibraries: Boolean = false;

var
  FPUMASK: TArithmeticExceptionMask;

procedure SafeMaskFPUExceptions(ExceptionsMasked : boolean);

implementation

uses WhisperUtils, WhisperTypes;

procedure SafeMaskFPUExceptions(ExceptionsMasked : boolean);
begin
  {$IF Defined(CPUX86) or Defined(CPUX64)}
  if ExceptionsMasked then
    begin
    FPUMASK := GetExceptionMask;
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
      exOverflow, exUnderflow, exPrecision]);
    end
  else
    SetExceptionMask(FPUMASK);
  {$ELSE}
//    MaskFPUExceptions(ExceptionsMasked);
  {$IFEND}
end;

constructor TDynLib.Create(const AName: string; AHandle: TDynLibHandle);
begin
  inherited Create;
  FName := AName;
  FHandle := AHandle;
  if (AHandle = InvalidDynLibHandle) then
    raise Exception.Create('TDynLib can not be created with invalid DynLibHandle');
  SymbolError := seWarnAndReturnNil; // debug seRaise;
end;

destructor TDynLib.Destroy;
begin
  { Delphi on Posix (Linux) implementation of FreeLibrary has a trivial error:
    It returns "Result := LongBool(dlclose(Module));" which is the opposite
    of what it should return, because dlclose returns 0 on success.

    But FreeLibrary should return True on success, False on failure,
    Delphi evidently wanted it to be consistent with FreeLibrary
    on WinAPI ( https://learn.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-freelibrary )
    that has "If the function succeeds, the return value is nonzero".

    So Delphi FreeLibrary should be fixed to report
    "Result := dlclose(Module) = 0;"
    Reported to Embarcadero: https://quality.embarcadero.com/browse/RSP-44047
  }
  {$if (not defined(MSWINDOWS))}
  FreeLibrary(FHandle);
  {$else}
  if not FreeLibrary(FHandle) then
    Raise EDynLibError.Create('Unloading library ' + Name + ' failed');
  {$endif}

  inherited;
end;

class function TDynLib.Load(const AName: string; const APath: String = ''; RaiseExceptionOnError: boolean = True): TDynLib;

  { On Unix, right now this simply uses LoadLibrary that calls dlopen(..., RTLD_LAZY)

    Historic notes:

    - Long time ago we used here explicit
        dlopen(.., RTLD_LAZY or RTLD_GLOBAL)
      Reasons: loading GLU under Linux required RTLD_GLOBAL
      (Maybe GLU requires that someone else loads GL symbols for it?)

      Later, this workaround doesn't seem needed anymore,
      and we actually don't load GLU using CastleDynLib.

    - Note that on Android, RTLD_GLOBAL *cannot* be used
      if you want to successfully open
      libopenal.so on Android (tested necessity on Nexus 5).
      And it seems that RTLD_NOW or RTLD_LAZY don't matter.
      Found thanks to mentions in
      http://grokbase.com/t/gg/android-ndk/133mh6mk8b/unable-to-dlopen-libtest-so-cannot-load-library-link-image-1995-failed-to-link-libtest-so

    In summary, simply using LoadLibrary is perfect now.
  }

var
  Handle: TDynLibHandle;
  LPath: String;
begin
  if InternalDisableDynamicLibraries then
    Handle := InvalidDynLibHandle
  else if APath.IsEmpty then
    begin
      if WhisperGlobalLibraryPath.IsEmpty then
        begin
          LPath := InclPathDelim(AppPath()) + AName;

          DebugLog.Debug('Trying to load library from %s', [LPath]);
          Handle := LoadLibrary(PChar(LPath));
          { On macOS, search for dynamic libraries in the bundle too.
            This fallback makes sense for libpng, libvorbisfile, libsteam_api...
            It seems that for everything, so just do it always. }
          {$ifdef MACOS}
          if (Handle = InvalidDynLibHandle) and (BundlePath <> '') then
            begin
              LPath := BundlePath + '/Contents/MacOS/' + AName;
              WriteLn(Format('Fail - Trying to load library from %s', [LPath]));
              Handle := LoadLibrary(PChar(LPath));
            end;
          {$endif}
        end
      else
        begin
          {$IFDEF MSWINDOWS}
          SetDllDirectory(PWideChar(Pointer(String(InclPathDelim(WhisperGlobalLibraryPath)))));
          {$ENDIF}
          LPath := InclPathDelim(WhisperGlobalLibraryPath) + AName;
          DebugLog.Debug('Trying to load library from %s', [LPath]);
          Handle := LoadLibrary(PChar(LPath));
          {$IFDEF MSWINDOWS}
          SetDllDirectory(Nil);
          {$ENDIF}
        end;
    end
  else
    begin
      LPath := InclPathDelim(APath) + AName;
      DebugLog.Debug('Trying to load library from "%s"', [LPath]);
      Handle := LoadLibrary(PChar(LPath));
    end;

  if Handle = InvalidDynLibHandle then
  begin
    if RaiseExceptionOnError then
    {$ifdef LOGNOTRAISE}
      begin
        Result := nil;
      end
    {$else}
      raise EDynLibError.Create('Cannot load dynamic library "' +AName+ '"')
    {$endif}
    else
      Result := nil;
  end else
    Result := Self.Create(AName, Handle);
end;

function TDynLib.Symbol(const SymbolName: PChar): Pointer;

  function ErrStr: string;
  begin
    Result := 'Symbol "' + SymbolName + '" not found in library "' + Name + '"';
  end;

begin
  Result := GetProcAddress(FHandle, SymbolName);
  if Result = nil then
    case SymbolError of
        seRaise: raise EDynLibError.Create(ErrStr);
        seReturnNil: ;
        seWarnAndReturnNil: { TODO: Needs Log };
      else
        raise EInternalError.Create('SymbolError=?');
    end;
end;

end.
