program WhisperBenchGui;

uses
  System.StartUpCopy,
  FMX.Forms,
  WhisperBenchMain in 'WhisperBenchMain.pas' {Form1},
  dynlib in '..\..\..\..\src\dynlib.pas',
  whisperexternal in '..\..\..\..\src\whisperexternal.pas',
  cheaplog in '..\..\..\..\src\cheaplog.pas',
  whisper in '..\..\..\..\src\whisper.pas',
  ggmlexternal in '..\..\..\..\src\ggmlexternal.pas',
  whispertypes in '..\..\..\..\src\whispertypes.pas',
  platform in '..\..\..\..\src\platform.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
