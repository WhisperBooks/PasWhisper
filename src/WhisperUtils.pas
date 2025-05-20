unit WhisperUtils;

interface

uses SysUtils, Classes, GgmlTypes, JSon;

type
  { An extremely simple, extremely unreliable, cross-platform + compiler
    MilliSecond Elapsed Timer
  }
  TMilliTimer = class
    strict private
      FTicks: UInt64; // GetTickCount64
      FStart: UInt64; // GetTickCount64
      function GetElapsed: Single;
      function GetTotalElapsed: Single;
    public
      constructor Create;
      procedure Reset;
      property Elapsed: Single Read GetElapsed;
      property TotalElapsed: Single Read GetTotalElapsed;
  end;

function FormatDot(const Fmt: String; const Args: array of const): String;
function Format_JSON(Value: String; Indentation: Integer = 4): String;
function DeviceTypeToString(dt: TGgmlBackendDevType): String;
function  AppPath(): String;
function ExclPathDelim(const s: string): string;
function InclPathDelim(const s: string): string;

{$ifdef MACOS}
type
  CFStringRef = Pointer;
  CFTypeRef = Pointer;
  CFBundleRef = Pointer;
  CFURLRef = Pointer;

function BundlePath: string;
{$ENDIF}

implementation

{$ifdef MACOS}
uses Macapi.CoreFoundation;

var
  BundlePathCached: Boolean;
  BundlePathCache: string;

function BundlePath: string;
{ Based on
  http://wiki.freepascal.org/OS_X_Programming_Tips#How_to_obtain_the_path_to_the_Bundle }
var
  bundle: CFBundleRef;
  pathRef: CFUrlRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
begin
  if not BundlePathCached then
  begin
    bundle := CFBundleGetMainBundle();
    if bundle = nil then
    begin
      BundlePathCache := '';
//      WritelnLog('We cannot detect our macOS AppBundle. Probably the application was run directly (like a Unix application, without being wrapped in a directory like "xxx.app"). Some GUI features (like application menu) will not work without running through AppBundle.');
    end else
    begin
      pathRef := CFBundleCopyBundleUrl(bundle);
      pathCFStr := CFUrlCopyFileSystemPath(pathRef, kCFUrlPOSIXPathStyle);
      CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
      CFRelease(pathRef);
      CFRelease(pathCFStr);
      BundlePathCache := String(pathStr);
      BundlePathCache := InclPathDelim(BundlePathCache);
    end;
    BundlePathCached := true;
  end;
  Result := BundlePathCache;
end;
{$ENDIF}

function InclPathDelim(const s: string): string;
begin
  {$if defined(MSWINDOWS)}
  { On Windows, also accept / as final path delimiter. }
  if (S <> '') and (S[Length(S)] = '/') then Exit(S);
  {$endif}
  Result := IncludeTrailingPathDelimiter(S);
end;

function ExclPathDelim(const s: string): string;
begin
  {$if defined(MSWINDOWS)}
  { On Windows, also accept / as final path delimiter. }
  if (S <> '') and (S[Length(S)] = '/') then Exit(Copy(S, 1, Length(S) - 1));
  {$endif}
  Result := ExcludeTrailingPathDelimiter(S);
end;

function  AppPath(): String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function DeviceTypeToString(dt: TGgmlBackendDevType): String;
begin
  case dt of
    GGML_BACKEND_DEVICE_TYPE_CPU: Result := 'CPU';
    GGML_BACKEND_DEVICE_TYPE_GPU: Result := 'GPU';
    GGML_BACKEND_DEVICE_TYPE_ACCEL: Result := 'Accelerator';
  else
    Result := 'Unknown';
  end;

end;

function GetTickCount64: UInt64; inline;
begin
  Result := TThread.GetTickCount64;
end;

constructor TMilliTimer.Create;
begin
  Reset;
end;

procedure TMilliTimer.Reset;
begin
  FTicks := GetTickCount64;
  FStart := GetTickCount64;
end;

function TMilliTimer.GetElapsed: Single;
begin
  Result := (GetTickCount64 - FTicks) / 1000;
  FTicks := GetTickCount64;
end;

function TMilliTimer.GetTotalElapsed: Single;
begin
  Result := (GetTickCount64 - FStart) / 1000;
  FTicks := GetTickCount64;
end;

function Format_JSON(Value: String; Indentation: Integer = 4): String; inline;
var
  JV: TJSONValue;
begin
  JV := nil;
  try
    try
      // JV := GetJSON(Value);
      JV := TJSONObject.ParseJSONValue(Value);
      Result := JV.Format(Indentation);
    except
      Result := '';;
    end;
  finally
    FreeAndNil(JV);
  end;
end;

function FormatDot(const Fmt: String; const Args: array of const): String;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := TFormatSettings.Create;
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := #0;
  Result := Format(Fmt, Args, FormatSettings);
end;

end.
