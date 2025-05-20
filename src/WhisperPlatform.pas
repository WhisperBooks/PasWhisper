unit WhisperPlatform;

interface

uses
  {$if defined(MACOS)}
	  Macapi.CoreFoundation,
  {$endif}
  SysUtils, WhisperUtils;

{$if defined(MACOS)}
var
  BundlePathCached: Boolean;
  BundlePathCache: string;

{ Main directory of the current macOS bundle, including final slash.
  Empty string if we're not run from a bundle. }
function BundlePath: string;
{$endif}

function InclPathDelim(const s: string): string;

implementation

function InclPathDelim(const s: string): string;
begin
  {$if defined(MSWINDOWS)}
  { On Windows, also accept / as final path delimiter. }
  if (S <> '') and (S[Length(S)] = '/') then Exit(S);
  {$endif}
  Result := IncludeTrailingPathDelimiter(S);
end;

{$if defined(MACOS)}
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
      Raise Exception.Create('We cannot detect our macOS AppBundle. Probably the application was run directly (like a Unix application, without being wrapped in a directory like "xxx.app"). Some GUI features (like application menu) will not work without running through AppBundle.');
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
{$endif}

end.

