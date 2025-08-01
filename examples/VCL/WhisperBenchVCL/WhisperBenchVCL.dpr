program WhisperBenchVCL;

uses
  Vcl.Forms,
  WhisperBenchMain in 'src\WhisperBenchMain.pas' {Form1},
  whisper in '..\..\..\src\whisper.pas',
  ComputeEngine in '..\..\..\src\ComputeEngine.pas',
  WhisperDynlib in '..\..\..\src\WhisperDynlib.pas',
  whisperexternal in '..\..\..\src\whisperexternal.pas',
  WhisperLog in '..\..\..\src\WhisperLog.pas',
  WhisperPlatform in '..\..\..\src\WhisperPlatform.pas',
  whispertypes in '..\..\..\src\whispertypes.pas',
  WhisperUtils in '..\..\..\src\WhisperUtils.pas',
  ggmlexternal in '..\..\..\src\ggmlexternal.pas',
  ggmltypes in '..\..\..\src\ggmltypes.pas',
  Log4D in '..\..\..\src\Log4D.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
