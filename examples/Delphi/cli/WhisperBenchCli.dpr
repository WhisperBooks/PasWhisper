program WhisperBenchCli;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  WhisperCliMain in 'WhisperCliMain.pas',
  whisper in '..\..\..\src\whisper.pas',
  whisperexternal in '..\..\..\src\whisperexternal.pas',
  whispertypes in '..\..\..\src\whispertypes.pas',
  cheaplog in '..\..\..\src\cheaplog.pas',
  dynlib in '..\..\..\src\dynlib.pas',
  ggmlexternal in '..\..\..\src\ggmlexternal.pas';

{$R *.res}
begin
    { TODO -oUser -cConsole Main : Insert code here }
  WhisperMain;
end.
