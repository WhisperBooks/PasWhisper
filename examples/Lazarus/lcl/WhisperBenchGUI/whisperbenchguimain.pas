unit WhisperBenchGUIMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Whisper, WhisperTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    BackendsLoaded: Boolean;
    procedure RunBench;
  public

  end;

var
  Form1: TForm1;

const
  Threads: Int32 = 4;
  MaxBenchToken = 256;

implementation

{$R *.lfm}

{ TForm1 }

uses WhisperUtils;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Enabled := False;
  Memo1.Lines.Clear;
  Application.ProcessMessages;
  RunBench;
  Button1.Enabled := True;
end;

procedure TForm1.RunBench;
var
  Info: String;
  I: Integer;
  Whisp: TWhisper;
  NMels: Int32;
  Tokens: array [0..MaxBenchToken-1] of TWhisperToken;
  Timings: PWhisperTimings;
  ModelFile: String;
  sw: TMilliTimer;
  Perf: Array[0..7] of Single; // A few spare just in case
begin
  Whisp := TWhisper.Create;
  try
    sw := TMilliTimer.Create;
    try
      if not BackendsLoaded then
        begin
  //        Whisp.LoadBackends;
          Whisp.LoadBestBackend('cuda');
          Whisp.LoadBestBackend('blas');
          Whisp.LoadBestBackend('cpu-sandybridge');
          BackendsLoaded := True;
        end;
      Perf[0] := sw.Elapsed; // Loaded Backends


      {$IF OS_PLATFORM_TYPE = 'WIN64'}
        ModelFile := 'd:\models\ggml-base.en.bin';
      {$ELSEIF OS_PLATFORM_TYPE = 'LINUX64'}
        ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
      {$ELSEIF OS_PLATFORM_TYPE = 'OSXARM64'}
        ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
      {$ELSEIF OS_PLATFORM_TYPE = 'OSX64')}
        ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
      {$ELSE}
        ModelFile := 'd:\models\ggml-base.en.bin';
      {$ENDIF}
      if Whisp.LoadModel(ModelFile, not Checkbox1.Checked) then
        begin
          NMels := Whisp.ModelNmels;
          if Whisp.SetMel(Nil, 0, NMels) <> WHISPER_SUCCESS then
            Exit;

          for I := 0 to MaxBenchToken - 1 do
              Tokens[I] := 0;

          Perf[1] := sw.Elapsed; // Loaded Model

          // Heat
          if Whisp.Encode(0, Threads) <> WHISPER_SUCCESS then
            Exit;
          if Whisp.Decode(@Tokens, 256, 0, Threads) <> WHISPER_SUCCESS then
            Exit;
          if Whisp.Decode(@Tokens, 1, 256, Threads) <> WHISPER_SUCCESS then
            Exit;

          Whisp.ResetTimings;

          Perf[2] := sw.Elapsed; // Done Heat

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

          Perf[3] := sw.Elapsed; // Done Run
          Perf[4] := sw.TotalElapsed; // Done Run

          Timings := Whisp.GetTimings;

          // Log.d('Hello');
          Memo1.Lines.Add(FormatDot('Whisper NMels               : %d',[Nmels]));
          if(Timings <> Nil) then
            begin
              Memo1.Lines.Add(FormatDot('Whisper Sample ms           : %3.8f',[Timings^.SampleMs]));
              Memo1.Lines.Add(FormatDot('Whisper Encode ms           : %3.8f',[Timings^.EncodeMs]));
              Memo1.Lines.Add(FormatDot('Whisper Decode ms           : %3.8f',[Timings^.DecodeMs]));
              Memo1.Lines.Add(FormatDot('Whisper Batch ms            : %3.8f',[Timings^.BatchdMs]));
              Memo1.Lines.Add(FormatDot('Whisper Prompt ms           : %3.8f',[Timings^.PromptMs]));
            end;
          Memo1.Lines.Add('');
          Memo1.Lines.Add(FormatDot('Whisper Load Backends       : %8.3f',[Perf[0]]));
          Memo1.Lines.Add(FormatDot('Whisper Load Model          : %8.3f',[Perf[1]]));
          Memo1.Lines.Add(FormatDot('Whisper Load Heat           : %8.3f',[Perf[2]]));
          Memo1.Lines.Add(FormatDot('Whisper Load Run            : %8.3f',[Perf[3]]));
          Memo1.Lines.Add(FormatDot('Whisper Total Runtime       : %8.3f',[Perf[4]]));
          Memo1.Lines.Add('');

          Info := Format_JSON(Whisp.GetSystemInfoJson);
          Memo1.Lines.Add(Format('Info : %s',[Info]));

        end;
    finally
      sw.Free;
    end;
  finally
    Whisp.Free;
  end;

end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

