program PasWhisper;

uses
  System.StartUpCopy,
  FMX.Forms,
  mainform in 'mainform.pas' {Form1},
  dynlib in 'dynlib.pas',
  whisperexternal in 'whisperexternal.pas',
  cheaplog in 'cheaplog.pas',
  whisputils in 'whisputils.pas',
  whisper in 'whisper.pas',
  ggmlexternal in 'ggmlexternal.pas',
  whispertypes in 'whispertypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
