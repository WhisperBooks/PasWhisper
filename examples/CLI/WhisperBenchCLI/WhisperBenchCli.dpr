program WhisperBenchCli;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  WhisperCliMain in 'src\WhisperCliMain.pas',
  whisper in '..\..\..\src\whisper.pas',
  whisperexternal in '..\..\..\src\whisperexternal.pas',
  whispertypes in '..\..\..\src\whispertypes.pas',
  WhisperUtils in '..\..\..\src\WhisperUtils.pas',
  WhisperDynlib in '..\..\..\src\WhisperDynlib.pas',
  ggmltypes in '..\..\..\src\ggmltypes.pas',
  WhisperPlatform in '..\..\..\src\WhisperPlatform.pas',
  ggmlexternal in '..\..\..\src\ggmlexternal.pas',
  WhisperLog in '..\..\..\src\WhisperLog.pas',
  ComputeDevice in '..\..\..\src\ComputeDevice.pas',
  Log4D in '..\..\..\src\Log4D.pas';

{$R *.res}
begin
    { TODO -oUser -cConsole Main : Insert code here }
  WhisperMain;
end.
