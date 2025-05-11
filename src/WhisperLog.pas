unit WhisperLog;

interface
uses
  SysUtils
  {$IFNDEF FPC}
  , log4d;
{$ELSE}
 ;

type
  TLogLogger = class
  public
    constructor Create;
    procedure Debug(const A: String); overload;
    procedure Debug(const Fmt: string; const Args: array of Const); overload;
  end;

{$ENDIF}
var
  DebugLog: TLogLogger;

procedure DebugLogInit(const AFilename: String = 'debug.log');

implementation

{$IFNDEF FPC}
procedure DebugLogInit(const AFilename: String);
var
  FileLogger: TLogFileAppender;
  FileLayout: TLogPatternLayout;
begin
  try

    FileLayout := TLogPatternLayout.Create('[%-5p][%8r] %m%n');

    FileLogger := TLogFileAppender.Create('DebugLogger',AFilename, FileLayout, False);
    TLogBasicConfigurator.Configure(FileLogger);

    // set the log level
    TLogLogger.GetRootLogger.Level := Trace;

    // create a named logger
    DebugLog := TLogLogger.GetLogger('WhisperLog');

    // write log Startup messages
    {
    DebugLog.Fatal('fatal output 3');
    DebugLog.Error('error output 3');
    DebugLog.Warn('warn output 3');
    DebugLog.Info('info output 3');
    DebugLog.Debug('debug output 3');
    DebugLog.Trace('trace %d %s',[312, 'Fred']);
    }
  except
    on E:Exception do
    begin
        Raise E.CreateFmt('%s : %s',[E.Classname, E.Message]);
    end;
  end;
end;
{$ELSE}
constructor TLogLogger.Create;
begin
end;
procedure TLogLogger.Debug(const A: String);
begin

end;

procedure TLogLogger.Debug(const Fmt: string; const Args: array of Const);
begin
end;


procedure DebugLogInit(const AFilename: String);
begin
end;
{$ENDIF}

initialization
  {$IFNDEF FPC}
  TLogLogger.GetRootLogger.Level := Off;
  DebugLog := TLogLogger.GetLogger('NoLog');
  {$ELSE}
  DebugLog := TLogLogger.Create;
  {$ENDIF}
finalization
  DebugLog := Nil;

end.
