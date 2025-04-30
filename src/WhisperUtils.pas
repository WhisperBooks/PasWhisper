unit WhisperUtils;

interface

uses SysUtils, Classes, GgmlTypes
{$ifdef FPC}
  , fpjson, jsonparser
{$else}
  , JSon
{$endif}
;

type
  { An extremely simple, extremely unreliable, cross-platform + compiler
    MilliSecond Elapsed Timer
  }
  TMilliTimer = class
    strict private
      FTicks: {$IFDEF FPC}QWord{$ELSE}UInt64{$ENDIF}; // GetTickCount64
      FStart: {$IFDEF FPC}QWord{$ELSE}UInt64{$ENDIF}; // GetTickCount64
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

implementation

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

{$IFNDEF FPC}
function GetTickCount64: UInt64; inline;
begin
  Result := TThread.GetTickCount64;
end;
{$ENDIF}

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
  JV: {$ifdef FPC}TJSONData{$else}TJSONValue{$endif};
begin
  JV := nil;
  try
    try
      {$ifdef FPC}
      // JV := GetJSON(Value);
      Result := Value; // JV.FormatJSON([], Indentation);
      {$else}
      JV := TJSONObject.ParseJSONValue(Value);
      Result := JV.Format(Indentation);
      {$endif}
    except
      Result := '';;
    end;
  finally
    {$ifndef FPC}
    FreeAndNil(JV);
    {$endif}
  end;
end;

function FormatDot(const Fmt: String; const Args: array of const): String;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := {$ifdef fpc}Default({$endif}TFormatSettings{$ifdef fpc}){$endif}{$ifndef fpc}.Create{$endif};
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := #0;
  Result := Format(Fmt, Args, FormatSettings);
end;

end.
