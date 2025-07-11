unit ComputeEngine;

interface

uses System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  GGMLTypes, GGMLExternal;

type
  TComputeDevice = class

  end;

  TComputeGGML = class
    FRefCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function BackendLoad(const BackendLibrary: PAnsiChar): PGgmlBackendReg;
    procedure BackendLoadAll;
    procedure BackendLoadAllFromPath(const LibraryPath: PAnsiChar = Nil);
    function BackendTryLoadBest(const BackendDeviceClass: PAnsiChar; const LibraryPath: PAnsiChar = Nil): PGgmlBackendReg;
    function BackendGetDeviceCount: Int32;
  end;

  TComputeBackend = class
  strict private
    FDeviceList: TObjectList<TComputeDevice>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  GlobalComputeGGML: TComputeGGML;

implementation

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
  GlobalComputeGGML.Free;
  inherited;
end;

{ TComputeGGML }

function TComputeGGML.BackendGetDeviceCount: Int32;
begin
//  GgmlBackendGetDeviceCount: function(): Int32; CDecl;
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
    Abort
  else
    begin
      if GgmlLibrary = Nil then
        begin
          try
            InitializeGgmlLibrary;
            GlobalComputeGGML := Self;
            Inc(FRefCount);
          finally

          end;
        end;
    end;

end;

destructor TComputeGGML.Destroy;
begin
  Dec(FRefCount);
  if FRefCount = 0 then
    FinalizeGgmlLibrary
  else
    Abort;
  inherited;
end;

end.
