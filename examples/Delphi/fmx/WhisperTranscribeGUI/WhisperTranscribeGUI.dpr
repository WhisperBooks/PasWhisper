program WhisperTranscribeGUI;

uses
  System.StartUpCopy,
  FMX.Forms,
  whispertranscribemain in 'src\whispertranscribemain.pas' {Form1},
  cheaplog in '..\..\..\..\src\cheaplog.pas',
  dynlib in '..\..\..\..\src\dynlib.pas',
  ggmltypes in '..\..\..\..\src\ggmltypes.pas',
  platform in '..\..\..\..\src\platform.pas',
  whisper in '..\..\..\..\src\whisper.pas',
  whisperexternal in '..\..\..\..\src\whisperexternal.pas',
  whispertypes in '..\..\..\..\src\whispertypes.pas',
  ggmlexternal in '..\..\..\..\src\ggmlexternal.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
