unit ComputeEngine;

interface

uses System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, WhisperLog,
  GGMLTypes, WhisperTypes, WhisperDynLib, GGMLExternal;

type
  TComputeDevice = class
  strict private
    FRefCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Aquire;
    function Release: Boolean;
  end;

  TComputeGGML = class
  strict private
    FRefCount: Integer;
    procedure Aquire;
  public
    constructor Create;
    destructor Destroy; override;
    function BackendLoad(const BackendLibrary: PAnsiChar): PGgmlBackendReg;
    procedure BackendLoadAll;
    procedure BackendLoadAllFromPath(const LibraryPath: PAnsiChar = Nil);
    function BackendTryLoadBest(const BackendDeviceClass: PAnsiChar; const LibraryPath: PAnsiChar = Nil): PGgmlBackendReg;
    function BackendGetDeviceCount: Int32;
    function Release: Boolean;
  end;

  TComputeBackend = class
  strict private
    FDeviceList: TObjectList<TComputeDevice>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDeviceCount: Int32;
    function LoadBest(const ADeviceType: String; const APath: String = ''): PGgmlBackendReg;
  end;

var
  GlobalComputeGGML: TComputeGGML;

implementation

uses Math;

{ TComputeBackend }

constructor TComputeBackend.Create;
begin
  if GlobalComputeGGML = Nil then
    TComputeGGML.Create;
  FDeviceList := TObjectList<TComputeDevice>.Create;
end;

destructor TComputeBackend.Destroy;
begin
  FreeAndNil(FDeviceList);
  if GlobalComputeGGML.Release then
    GlobalComputeGGML.Free;
  inherited;
end;


function TComputeBackend.GetDeviceCount: Int32;
begin
  Result := GlobalComputeGGML.BackendGetDeviceCount;
end;

function TComputeBackend.LoadBest(const ADeviceType, APath: String): PGgmlBackendReg;
var
  Dev: TComputeDevice;
  LPath: String;
  LLib: PGGMLBackendReg;
  DevLoadedOK: Boolean;
begin
  DevLoadedOK := False;
  Dev := TComputeDevice.Create;
  if WhisperGlobalLibraryPath.IsEmpty then
    LPath := APath
  else
    begin
      if APath.IsEmpty then
        LPath := WhisperGlobalLibraryPath
      else
        LPath := APath;
    end;

  SafeMaskFPUExceptions(True);
  try
    Result := Nil;
    DebugLog.Debug('Trying to load device "%s" library from "%s"', [ADeviceType, LPath]);
    LLib := GgmlBackendTryLoadBest(PAnsiChar(Pointer(AnsiString(ADeviceType))), PAnsiChar(Pointer(AnsiString(LPath))));
    if LLib <> Nil then
      begin
        DebugLog.Debug('API Version is %d', [LLib^.ApiVersion]);
        Dev.Aquire;
        DevLoadedOK := True;
      end
    else
      begin
        DebugLog.Debug('Load failed');
        FreeAndNil(Dev);
      end;

  finally
    SafeMaskFPUExceptions(False);
    if DevLoadedOK then
      FDeviceList.Add(Dev);
  end;

end;

{ TComputeGGML }

procedure TComputeGGML.Aquire;
begin
  Inc(FRefCount);
end;

function TComputeGGML.BackendGetDeviceCount: Int32;
begin
  Result :=  GgmlBackendGetDeviceCount;
end;

function TComputeGGML.BackendLoad(
  const BackendLibrary: PAnsiChar): PGgmlBackendReg;
begin
//  GgmlBackendLoad: function (const BackendLibrary: PAnsiChar): PGgmlBackendReg; CDecl;
end;

procedure TComputeGGML.BackendLoadAll;
begin
//  GgmlBackendLoadAll: procedure (); CDecl;
end;

procedure TComputeGGML.BackendLoadAllFromPath(const LibraryPath: PAnsiChar);
begin
//  GgmlBackendLoadAllFromPath: procedure (const LibraryPath: PAnsiChar = Nil); CDecl;
end;

function TComputeGGML.BackendTryLoadBest(const BackendDeviceClass,
  LibraryPath: PAnsiChar): PGgmlBackendReg;
begin
//  GgmlBackendTryLoadBest: function (const BackendDeviceClass: PAnsiChar; const LibraryPath: PAnsiChar = Nil): PGgmlBackendReg; CDecl;
end;

constructor TComputeGGML.Create;
begin
  if GlobalComputeGGML <> Nil then
    begin
      GlobalComputeGGML.Aquire;
      Abort;
    end
  else
    begin
      if GgmlLibrary = Nil then
        begin
          try
            InitializeGgmlLibrary;
            GlobalComputeGGML := Self;
            GlobalComputeGGML.Aquire;
          finally

          end;
        end;
    end;

end;

destructor TComputeGGML.Destroy;
begin
  FinalizeGgmlLibrary;
  inherited;
end;

function TComputeGGML.Release: Boolean;
begin
  Dec(FRefCount);
  if FRefCount < 1 then
     Result := True;
end;

{ TComputeDevice }

procedure TComputeDevice.Aquire;
begin
  Inc(FRefCount);
end;

constructor TComputeDevice.Create;
begin
end;

destructor TComputeDevice.Destroy;
begin

  inherited;
end;

function TComputeDevice.Release: Boolean;
begin
  Dec(FRefCount);
  if FRefCount < 1 then
     Result := True;
end;

end.
