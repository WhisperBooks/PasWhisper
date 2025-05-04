program WhisperStreamGUI;

uses
  System.StartUpCopy,
  FMX.Forms,
  whisperstreammain in 'src\whisperstreammain.pas' {Form1},
  WhisperUtils in '..\..\..\..\src\WhisperUtils.pas',
  WhisperDynlib in '..\..\..\..\src\WhisperDynlib.pas',
  ggmltypes in '..\..\..\..\src\ggmltypes.pas',
  WhisperPlatform in '..\..\..\..\src\WhisperPlatform.pas',
  whisper in '..\..\..\..\src\whisper.pas',
  whisperexternal in '..\..\..\..\src\whisperexternal.pas',
  whispertypes in '..\..\..\..\src\whispertypes.pas',
  ggmlexternal in '..\..\..\..\src\ggmlexternal.pas',
  WhisperLog in '..\..\..\..\src\WhisperLog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
