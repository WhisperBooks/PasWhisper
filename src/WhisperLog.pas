unit WhisperLog;

interface
uses
  SysUtils, log4d;
{
uses
  System.SysUtils,
  LoggerPro, //LoggerPro core
  LoggerPro.FileAppender;
var
  Log: ILogWriter;
}

var
  DebugLog: TLogLogger;

procedure DebugLogInit(const AFilename: String = 'debug.log');

implementation

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

initialization
  TLogLogger.GetRootLogger.Level := Off;
  DebugLog := TLogLogger.GetLogger('NoLog');

finalization
//  Log := Nil;

end.
