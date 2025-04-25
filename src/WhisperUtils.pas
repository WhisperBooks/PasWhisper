unit WhisperUtils;

interface

uses SysUtils, Classes;

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

var
  OutLog: TStrings = Nil;
{
procedure WritelnLog(const Category: string; const Message: string); overload;
procedure WritelnLog(const Message: string); overload;
procedure WritelnLog(const Category: string; const MessageBase: string; const Args: array of const); overload;
procedure WritelnLog(const MessageBase: string; const Args: array of const); overload;
procedure WritelnWarning(const Category: string; const Message: string); overload;
procedure WritelnWarning(const Message: string); overload;
procedure WritelnWarning(const Category: string; const MessageBase: string; const Args: array of const); overload;
procedure WritelnWarning(const MessageBase: string; const Args: array of const); overload;
procedure WarningWrite(const Message: string);
}

function FormatDot(const Fmt: String; const Args: array of const): String;
function Format_JSON(Value: String; Indentation: Integer = 4): String; inline;

implementation

{$ifdef FPC}
uses fpjson, jsonparser;
{$else}
uses JSon;
{$endif}
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
      JV := GetJSON(Value);
      Result := JV.FormatJSON([], Indentation);
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

{
procedure WritelnLog(const Category: string; const Message: string);
begin
  if Assigned(OutLog) then
    OutLog.Add(Category + ' : ' + Message);
end;

procedure WritelnLog(const Message: string);
begin
  if Assigned(OutLog) then
    OutLog.Add(Message);
end;

procedure WarningWrite(const Message: string);
begin
  if Assigned(OutLog) then
    OutLog.Add(Message);
end;
procedure WritelnLog(const Category: string; const MessageBase: string; const Args: array of const);
begin
  if Assigned(OutLog) then
    OutLog.Add(Category + ' : ' + FormatDot(MessageBase, Args));
end;

procedure WritelnLog(const MessageBase: string; const Args: array of const);
begin
  if Assigned(OutLog) then
    OutLog.Add(FormatDot(MessageBase, Args));
end;

procedure WritelnWarning(const Category: string; const Message: string);
begin
  if Assigned(OutLog) then
    OutLog.Add(Category + ' : ' + Message);
end;

procedure WritelnWarning(const Message: string);
begin
  if Assigned(OutLog) then
    OutLog.Add(Message);
end;

procedure WritelnWarning(const Category: string; const MessageBase: string; const Args: array of const);
begin
  if Assigned(OutLog) then
    OutLog.Add(Category + ' : ' + FormatDot(MessageBase, Args));
end;

procedure WritelnWarning(const MessageBase: string; const Args: array of const);
begin
  if Assigned(OutLog) then
    OutLog.Add(FormatDot(MessageBase, Args));
end;
}

end.
