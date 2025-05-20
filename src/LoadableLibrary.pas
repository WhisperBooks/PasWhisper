unit LoadableLibrary;

interface

uses Classes, SysUtils, IOUtils, WhisperDynlib, WhisperUtils;

type

  TLibraryLoadResult = (
    DEVICE_ERR,
    DEVICE_OK
  );

  TLoadableLibrary = class
  strict private
    FLibraryName: String;
    FExtensionOverride: String;
    FLibraryFilePath: String;
    FLibraryFileName: String;
    FLibrary: TDynLib;
  public
    constructor Create(const ALibraryName: String; const AExtensionOverride: String = ''; const AFilePath: String = '');
    destructor Destroy; override;
    function Open: Boolean;
    function LoadSymbols: Boolean; virtual;
    function FreeSysbols: Boolean; virtual;
    procedure Close;
  end;

implementation

{ TLoadableLibrary }

procedure TLoadableLibrary.Close;
begin
end;

constructor TLoadableLibrary.Create(const ALibraryName,
  AExtensionOverride: String; const AFilePath: String);
begin
  FLibraryName := ALibraryName;
  FExtensionOverride := AExtensionOverride;
  FLibraryFilePath := AFilePath;
end;

destructor TLoadableLibrary.Destroy;
begin
  FreeSysbols;
  inherited;
end;

function TLoadableLibrary.FreeSysbols: Boolean;
begin
end;

function TLoadableLibrary.LoadSymbols: Boolean;
begin

end;

function TLoadableLibrary.Open: Boolean;
var
  LName: String;
  LLibrary: TDynLib;
begin
  // Prepare to Fail
  LLibrary := Nil;
  Result := True;
  {$IF DEFINED(MSWINDOWS)}
    LName := FLibraryName + '.dll';
  {$ELSE}
    if FExtensionOverride.IsEmpty() then
      {$IF DEFINED(MACOS)}
      LName := 'lib' + FLibraryName + '.dylib'
      {$ELSE}
      LName := 'lib' + FLibraryName + '.so'
      {$ENDIF}
    else
      LName := 'lib' + FLibraryName + '.' + FExtensionOverride;
  {$ENDIF}


  if FileExists(TPath.Combine(FLibraryFilePath, LName)) then
    LLibrary := TDynLib.Load(LName, FLibraryFilePath, true)
  {$IF DEFINED(MACOS)}
  else
    if FileExists(TPath.Combine(BundlePath + '/Contents/MacOS/', LName)) then
      LLibrary := TDynLib.Load(LName, BundlePath + '/Contents/MacOS/', true)
  {$ENDIF}
  ;

  // Success
  if LLibrary <> Nil then
    begin
      FLibrary := LLibrary;
      Result := True;
    end;

end;

end.
