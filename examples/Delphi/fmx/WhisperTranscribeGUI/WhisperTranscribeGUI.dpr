program WhisperTranscribeGUI;

uses
  System.StartUpCopy,
  FMX.Forms,
  whispertranscribemain in 'whispertranscribemain.pas' {Form1},
  cheaplog in '..\..\..\..\src\cheaplog.pas',
  dynlib in '..\..\..\..\src\dynlib.pas',
  ggmlexternal in '..\..\..\..\src\ggmlexternal.pas',
  platform in '..\..\..\..\src\platform.pas',
  whisper in '..\..\..\..\src\whisper.pas',
  whisperexternal in '..\..\..\..\src\whisperexternal.pas',
  whispertypes in '..\..\..\..\src\whispertypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
