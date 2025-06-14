program WhisperBenchGUI;

uses
  System.StartUpCopy,
  FMX.Forms,
  WhisperBenchMain in 'src\WhisperBenchMain.pas',
  ggmlexternal in '..\..\..\src\ggmlexternal.pas',
  ggmltypes in '..\..\..\src\ggmltypes.pas',
  whisper in '..\..\..\src\whisper.pas',
  WhisperBackend in '..\..\..\src\WhisperBackend.pas',
  WhisperDynlib in '..\..\..\src\WhisperDynlib.pas',
  whisperexternal in '..\..\..\src\whisperexternal.pas',
  WhisperLog in '..\..\..\src\WhisperLog.pas',
  WhisperPlatform in '..\..\..\src\WhisperPlatform.pas',
  whispertypes in '..\..\..\src\whispertypes.pas',
  WhisperUtils in '..\..\..\src\WhisperUtils.pas',
  Settings in 'src\Settings.pas',
  Log4D in '..\..\..\src\Log4D.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
