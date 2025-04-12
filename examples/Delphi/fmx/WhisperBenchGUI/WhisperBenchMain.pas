unit WhisperBenchMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Whisper, CheapLog, GgmlExternal,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Layouts;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Memo1: TMemo;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  Threads: Int32 = 4;
  MaxBenchToken = 512;

implementation

{$R *.fmx}

uses WhisperTypes, IOUtils;

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  Whisp: TWhisper;
  NMels: Int32;
  Tokens: array [0..MaxBenchToken-1] of TWhisperToken;
  Timings: PWhisperTimings;
  ModelFile: String;
begin
  Whisp := TWhisper.Create;
  try
  {$IF (OS_PLATFORM_TYPE = 'WIN64')}
    ModelFile := 'C:\models\ggml-base.en.bin';
  {$ELSEIF (OS_PLATFORM_TYPE = 'LINUX64')}
    ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
  {$ELSEIF (OS_PLATFORM_TYPE = 'OSXARM64')}
    ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
  {$ELSEIF (OS_PLATFORM_TYPE = 'OSX64')}
    ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
  {$ELSE}
    Unsupported Platform
  {$ENDIF}
    if Whisp.LoadModel(ModelFile) then
      begin
        NMels := Whisp.ModelNmels;
        if Whisp.SetMel(Nil, 0, NMels) <> WHISPER_SUCCESS then
          Exit;

        for I := 0 to MaxBenchToken - 1 do
            Tokens[I] := 0;

        // Heat
        if Whisp.Encode(0, Threads) <> WHISPER_SUCCESS then
          Exit;
        if Whisp.Decode(@Tokens, 256, 0, Threads) <> WHISPER_SUCCESS then
          Exit;
        if Whisp.Decode(@Tokens, 1, 256, Threads) <> WHISPER_SUCCESS then
          Exit;

        Whisp.ResetTimings;

        // Run
        if Whisp.Encode(0, Threads) <> 0 then
          Exit;

        for I := 0 to 255 do
          begin
            if Whisp.Decode(@Tokens, 1, I, Threads) <> WHISPER_SUCCESS then
              Exit;
          end;

        for I := 0 to 63 do
          begin
            if Whisp.Decode(@Tokens, 5, 0, Threads) <> WHISPER_SUCCESS then
              Exit;
          end;

        for I := 0 to 15 do
          begin
            if Whisp.Decode(@Tokens, 256, 0, Threads) <> WHISPER_SUCCESS then
              Exit;
          end;

        Timings := Whisp.GetTimings;

        // Log.d('Hello');
        WriteLnLog('Whisper NMels               : %d',[Nmels]);
        WriteLnLog('Whisper Sample ms           : %3.8f',[Timings^.SampleMs]);
        WriteLnLog('Whisper Encode ms           : %3.8f',[Timings^.EncodeMs]);
        WriteLnLog('Whisper Decode ms           : %3.8f',[Timings^.DecodeMs]);
        WriteLnLog('Whisper Batch ms            : %3.8f',[Timings^.BatchdMs]);
        WriteLnLog('Whisper Prompt ms           : %3.8f',[Timings^.PromptMs]);
      end;
  finally
    Whisp.Free;
  end;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 // FreeAndNil(FWhisper);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OutLog := Memo1.Lines;
end;

end.
