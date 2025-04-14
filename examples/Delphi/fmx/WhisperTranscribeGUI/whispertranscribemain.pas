unit whispertranscribemain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Whisper, CheapLog, GgmlExternal,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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

uses WhisperTypes, IOUtils, Diagnostics;

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  Whisp: TWhisper;
  NMels: Int32;
  Tokens: array [0..MaxBenchToken-1] of TWhisperToken;
  Timings: PWhisperTimings;
  ModelFile: String;
  sw: TStopWatch;
  Perf: Array[0..7] of Int64;
begin
  Whisp := TWhisper.Create;
  sw := TStopWatch.StartNew;
  try
    Perf[0] := sw.ElapsedMilliseconds; // Start
    Whisp.LoadBackends;
    Perf[1] := sw.ElapsedMilliseconds; // Loaded Backends

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

        Perf[2] := sw.ElapsedMilliseconds; // Loaded Model

        // Heat
        if Whisp.Encode(0, Threads) <> WHISPER_SUCCESS then
          Exit;
        if Whisp.Decode(@Tokens, 256, 0, Threads) <> WHISPER_SUCCESS then
          Exit;
        if Whisp.Decode(@Tokens, 1, 256, Threads) <> WHISPER_SUCCESS then
          Exit;

        Whisp.ResetTimings;

        Perf[3] := sw.ElapsedMilliseconds; // Done Heat

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

        Perf[4] := sw.ElapsedMilliseconds; // Done Run

        Timings := Whisp.GetTimings;

        // Log.d('Hello');
        Memo1.Lines.Clear;
        WriteLnLog('Whisper NMels               : %d',[Nmels]);
        WriteLnLog('Whisper Sample ms           : %3.8f',[Timings^.SampleMs]);
        WriteLnLog('Whisper Encode ms           : %3.8f',[Timings^.EncodeMs]);
        WriteLnLog('Whisper Decode ms           : %3.8f',[Timings^.DecodeMs]);
        WriteLnLog('Whisper Batch ms            : %3.8f',[Timings^.BatchdMs]);
        WriteLnLog('Whisper Prompt ms           : %3.8f',[Timings^.PromptMs]);
        WriteLnLog('');
        WriteLnLog('Whisper Load Backends       : %8.3f',[(Perf[1] - Perf[0])/1000]);
        WriteLnLog('Whisper Load Model          : %8.3f',[(Perf[2] - Perf[1])/1000]);
        WriteLnLog('Whisper Load Heat           : %8.3f',[(Perf[3] - Perf[2])/1000]);
        WriteLnLog('Whisper Load Run            : %8.3f',[(Perf[4] - Perf[3])/1000]);
        WriteLnLog('Whisper Total Runtime       : %8.3f',[(Perf[4] - Perf[0])/1000]);

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
