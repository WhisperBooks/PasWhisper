unit mainform;

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

uses WhisperTypes;

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  Whisp: TWhisper;
  NMels: Int32;
  Tokens: array [0..MaxBenchToken-1] of TWhisperToken;
  Timings: PWhisperTimings;
begin
  Whisp := TWhisper.Create;

  if Whisp.LoadModel('C:\models\ggml-base.en.bin') then
    begin
      NMels := Whisp.ModelNmels;
      if Whisp.SetMel(Nil, 0, NMels) <> 0 then
        Exit;

      for I := 0 to MaxBenchToken - 1 do
          Tokens[I] := 0;

      // Heat
      if Whisp.Encode(0, Threads) <> 0 then
        Exit;
      if Whisp.Decode(@Tokens, 256, 0, Threads) <> 0 then
        Exit;
      if Whisp.Decode(@Tokens, 1, 256, Threads) <> 0 then
        Exit;

      Whisp.ResetTimings;

      // Run
      if Whisp.Encode(0, Threads) <> 0 then
        Exit;

      for I := 0 to 255 do
        begin
          if Whisp.Decode(@Tokens, 1, I, Threads) <> 0 then
            Exit;
        end;

      for I := 0 to 63 do
        begin
          if Whisp.Decode(@Tokens, 5, 0, Threads) <> 0 then
            Exit;
        end;

      for I := 0 to 15 do
        begin
          if Whisp.Decode(@Tokens, 256, 0, Threads) <> 0 then
            Exit;
        end;

      Timings := Whisp.GetTimings;

      WriteLnLog('Whisper NMels               : %d',[Nmels]);
      WriteLnLog('Whisper Sample ms           : %3.8f',[Timings^.SampleMs]);
      WriteLnLog('Whisper Encode ms           : %3.8f',[Timings^.EncodeMs]);
      WriteLnLog('Whisper Decode ms           : %3.8f',[Timings^.DecodeMs]);
      WriteLnLog('Whisper Batch ms            : %3.8f',[Timings^.BatchdMs]);
      WriteLnLog('Whisper Prompt ms           : %3.8f',[Timings^.PromptMs]);
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
