program WhisperCli;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Whisper, WhisperTypes;

var
  FWhisper: TWhisper;
  Cparams: TWhisperContextParams;
  zz: TWhisperModel;
  AModel: String;

  begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    FWhisper := TWhisper.Create;
    cparams :=  default(TWhisperContextParams);
    AModel := 'C:\models\ggml-base.en.bin';

    zz := FWhisper.Test(AModel);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
