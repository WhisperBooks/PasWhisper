unit CheapLog;

interface

uses SysUtils, Classes;

var
  OutLog: TStrings = Nil;

procedure WritelnLog(const Category: string; const Message: string); overload;
procedure WritelnLog(const Message: string); overload;
procedure WritelnLog(const Category: string; const MessageBase: string; const Args: array of const); overload;
procedure WritelnLog(const MessageBase: string; const Args: array of const); overload;
procedure WritelnWarning(const Category: string; const Message: string); overload;
procedure WritelnWarning(const Message: string); overload;
procedure WritelnWarning(const Category: string; const MessageBase: string; const Args: array of const); overload;
procedure WritelnWarning(const MessageBase: string; const Args: array of const); overload;
function FormatDot(const Fmt: String; const Args: array of const): String;
procedure WarningWrite(const Message: string);

function Format_JSON(Value: String; Indentation: Integer = 4): String; inline;

implementation

{$ifndef FPC}
uses JSon;
{$endif}

function Format_JSON(Value: String; Indentation: Integer = 4): String; inline;
{$ifndef FPC}
var
  JV: TJSONValue; // not TJSONObject
  {$endif}
begin
  {$ifndef FPC}
  JV := nil;
  try
    try
      JV := TJSONObject.ParseJSONValue(Value);
         // TJSONObject.ParseJSONValue(Value) as TJSONObject cast fails
      Result := JV.Format(Indentation);
    except
      Result := '';;
    end;
  finally
    FreeAndNil(JV);
  end;
  {$else}
  // FixME : FPC version
  Result := Value;
  {$endif}
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

end.
