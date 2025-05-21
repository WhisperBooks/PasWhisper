program WhisperTranscribeGUI;

uses
  System.StartUpCopy,
  FMX.Forms,
  WhisperTranscribeMain in 'src\WhisperTranscribeMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
