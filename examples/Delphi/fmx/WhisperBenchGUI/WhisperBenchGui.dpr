program WhisperBenchGui;

uses
  System.StartUpCopy,
  FMX.Forms,
  WhisperBenchMain in 'src\WhisperBenchMain.pas' {Form1},
  WhisperDynlib in '..\..\..\..\src\WhisperDynlib.pas',
  whisperexternal in '..\..\..\..\src\whisperexternal.pas',
  WhisperUtils in '..\..\..\..\src\WhisperUtils.pas',
  whisper in '..\..\..\..\src\whisper.pas',
  ggmltypes in '..\..\..\..\src\ggmltypes.pas',
  whispertypes in '..\..\..\..\src\whispertypes.pas',
  WhisperPlatform in '..\..\..\..\src\WhisperPlatform.pas',
  ggmlexternal in '..\..\..\..\src\ggmlexternal.pas',
  WhisperLog in '..\..\..\..\src\WhisperLog.pas',
  WhisperBackend in '..\..\..\..\src\WhisperBackend.pas',
  ComputeDevice in '..\..\..\..\src\ComputeDevice.pas',
  BaseDevice in '..\..\..\..\src\BaseDevice.pas',
  Log4D in '..\..\..\..\src\Log4D.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
