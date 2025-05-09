unit BaseDevice;

{$I platform.inc}

interface

uses WhisperDynlib;

type
  // enum ggml_backend_dev_type
  TComputeDeviceType   = (
    // CPU device using system memory
    DEVICE_TYPE_CPU,
    // GPU device using dedicated memory
    DEVICE_TYPE_GPU,
    // accelerator devices intended to be used together with the CPU backend (e.g. BLAS or AMX)
    DEVICE_TYPE_ACCEL
    );

  TDeviceLoadResult = (
    DEVICE_ERR,
    DEVICE_OK
  );

  TComputeDevice = class
    strict private
      FPath: String;
      FLibraryName: String;
      function GetLibraryName(const AName: String): String;
      function UnLoadLibrary: Boolean;
    strict protected
      FDeviceName: String;
      FDeviceDesc: String;
      FMemoryFree: Uint64;
      FMemoryTotal: Uint64;
      FDeviceType: TComputeDeviceType;
      FLoadResult: TDeviceLoadResult;
    public
      constructor Create(const AName: String; const APath: String = ''); virtual;
      destructor Destroy; override;
  End;

  TComputeDeviceCPU = class(TComputeDevice)
    public
      destructor Destroy; override;
  end;

implementation

{ TComputeDevice }

constructor TComputeDevice.Create(const AName: String; const APath: String = '');
var
  LibName: String;
begin
  FDeviceName := AName;
  FPath := APath;
  LibName := GetLibraryName(FDeviceName);
  FLibraryName := LibName;
end;

destructor TComputeDevice.Destroy;
begin
  UnLoadLibrary;
  inherited;
end;

function TComputeDevice.GetLibraryName(const AName: String): String;
var
  LibName: String;
begin
  LibName :=
      {$IF NOT DEFINED(OS_WIN64)}
        'lib' +
      {$ENDIF}
      'ggml-' + AName +
      {$IF DEFINED(OS_WIN64)}
        '.dll'
      {$ELSE}
        {$IF DEFINED(OS_OSX64ARM)}
          '.dylib'
        {$ELSE}
          '.so'
        {$ENDIF}
      {$ENDIF};
  Result := LibName;
end;

function TComputeDevice.UnLoadLibrary: Boolean;
begin
  Result := True;
end;

{ TComputeDeviceCPU }

destructor TComputeDeviceCPU.Destroy;
begin

  inherited;
end;

end.
