program WhisperBenchGui;

uses
  System.StartUpCopy,
  FMX.Forms,
  WhisperBenchMain in 'WhisperBenchMain.pas' {Form1},
  dynlib in 'dynlib.pas',
  whisperexternal in 'whisperexternal.pas',
  cheaplog in 'cheaplog.pas',
  whisper in 'whisper.pas',
  ggmlexternal in 'ggmlexternal.pas',
  whispertypes in 'whispertypes.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
