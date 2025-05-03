unit WhisperBenchGUIMain;

{$mode objfpc}{$H+}
{$I platform.inc}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

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
    PromptCount: Integer;
    BatchCount: Integer;
    BatchSize: Integer;
    TokenCount: Integer;
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

uses
  WhisperTypes, GgmlTypes, GgmlExternal,
  WhisperExternal, Whisper, WhisperUtils;

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
  Tokens: TWhisperTokenArray;
  Timings: PWhisperActivity;
  ModelFile: String;
  sw: TMilliTimer;
  Perf: Array[0..7] of Single; // A few spare just in case
  dev: TBackendDevice;
//  GgmlBackendCount: int64;
  WhisperBackendCount: int64;
begin
  SetLength(Tokens, TokenCount);
  Whisp := TWhisper.Create;
  try
    sw := TMilliTimer.Create;
    try
      if not BackendsLoaded then
        begin
  //        Whisp.LoadBackends;
          Whisp.LoadBestBackend('cuda');
          Whisp.LoadBestBackend('blas');
          Whisp.LoadBestBackend('cpu');
          BackendsLoaded := True;
        end;
      Perf[0] := sw.Elapsed; // Loaded Backends

      {$IF DEFINED(OS_WIN64)}
        ModelFile := 'd:\models\ggml-base.en.bin';
      {$ELSEIF DEFINED(OS_LINUX64)}
        ModelFile := GetUserDir() + '/models/ggml-base.en.bin';
      {$ELSEIF DEFINED(OS_LINUX64ARM)}
        ModelFile := GetUserDir() + '/models/ggml-base.en.bin';
      {$ELSEIF DEFINED(OS_OSX64ARM)}
        ModelFile := GetUserDir() + '/models/ggml-base.en.bin';
      {$ELSEIF DEFINED(OS_OSX64)}
        ModelFile := GetUserDir() + '/models/ggml-base.en.bin';
      {$ELSE}
        Unsupported Platform
      {$ENDIF}

      if Whisp.LoadModel(ModelFile, not Checkbox1.Checked) then
        WhisperBackendCount := Whisp.GetBackendCount;

        if WhisperBackendCount < 1 then
          begin
            Memo1.Lines.Add(Format('Insufficient Devices',[WhisperBackendCount]));
            Exit;
          end;
        Memo1.Lines.Add(Format('Preferred Device (of %d)',[WhisperBackendCount]));

        dev := Whisp.GetPreferredBackend;
        Memo1.Lines.Add(Format('Device      : %s',[dev.devName]));
        Memo1.Lines.Add(Format('Description : %s',[dev.devDesc]));
        Memo1.Lines.Add(Format('Type        : %s',[DeviceTypeToString(dev.devType)]));
        if dev.devType = GGML_BACKEND_DEVICE_TYPE_GPU then
          begin
            Memo1.Lines.Add(Format('MemoryFree  : %d',[dev.memoryFree]));
            Memo1.Lines.Add(Format('MemoryTotal : %d',[dev.memoryTotal]));
          end;
        Memo1.Lines.Add('');

        if WhisperBackendCount > 1 then
          begin
            Memo1.Lines.Add('Backup Device(s)');
            for I := 1 to WhisperBackendCount - 1 do
              begin
                dev := Whisp.GetIndexedBackend(I);
                Memo1.Lines.Add(Format('Device      : %s',[dev.devName]));
                Memo1.Lines.Add(Format('Description : %s',[dev.devDesc]));
                Memo1.Lines.Add(Format('Type        : %s',[DeviceTypeToString(dev.devType)]));
                if dev.devType = GGML_BACKEND_DEVICE_TYPE_GPU then
                  begin
                    Memo1.Lines.Add(Format('MemoryFree  : %d',[dev.memoryFree]));
                    Memo1.Lines.Add(Format('MemoryTotal : %d',[dev.memoryTotal]));
                  end;
                Memo1.Lines.Add('');
              end;
          end;


          NMels := Whisp.ModelNmels;
          if Whisp.SetMel(Nil, 0, NMels) <> WHISPER_SUCCESS then
            Exit;

          for I := 0 to MaxBenchToken - 1 do
              Tokens[I] := 0;

          Perf[1] := sw.Elapsed; // Loaded Model

          // Heat
          if Whisp.Encode(0, Threads) <> WHISPER_SUCCESS then
            Exit;
          if Whisp.Decode(Tokens, TokenCount, 0, Threads) <> WHISPER_SUCCESS then
            Exit;
          if Whisp.Decode(Tokens, 1, TokenCount, Threads) <> WHISPER_SUCCESS then
            Exit;

          Whisp.ResetTimings;

          Perf[2] := sw.Elapsed; // Done Heat

          // Run
          if Whisp.Encode(0, Threads) <> 0 then
            Exit;

          for I := 0 to TokenCount - 1 do
            begin
              if Whisp.Decode(Tokens, 1, I, Threads) <> WHISPER_SUCCESS then
                Exit;
            end;

          for I := 0 to BatchCount - 1 do
            begin
              if Whisp.Decode(Tokens, BatchSize, 0, Threads) <> WHISPER_SUCCESS then
                Exit;
            end;

          for I := 0 to PromptCount - 1 do
            begin
              if Whisp.Decode(Tokens, TokenCount, 0, Threads) <> WHISPER_SUCCESS then
                Exit;
            end;

          Perf[3] := sw.Elapsed; // Done Run
          Perf[4] := sw.TotalElapsed; // Done Run

          Memo1.Lines.Add(FormatDot('Whisper NMels               : %d',[Nmels]));
          Timings := Whisp.GetActivity;

          Memo1.Lines.Add(Format('Whisper NMels               : %d',[Nmels]));
          if Timings <> Nil then
            begin
              Memo1.Lines.Add(Format('Whisper Sample ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NSample, Timings^.SampleMs * Timings^.NSample, Timings^.SampleMs]));
              Memo1.Lines.Add(Format('Whisper Encode ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NEncode, Timings^.EncodeMs * Timings^.NEncode, Timings^.EncodeMs]));
              Memo1.Lines.Add(Format('Whisper Decode ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NDecode, Timings^.DecodeMs * Timings^.NDecode, Timings^.DecodeMs]));
              Memo1.Lines.Add(Format('Whisper Batch ms  x %6d     : %12.4f tot / %12.4f per',[Timings^.NBatchd, Timings^.BatchdMs * Timings^.NBatchd, Timings^.BatchdMs]));
              Memo1.Lines.Add(Format('Whisper Prompt ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NPrompt, Timings^.PromptMs * Timings^.NPrompt, Timings^.PromptMs]));
            end;
          WriteLn(stderr, '');
          Memo1.Lines.Add(FormatDot('Whisper Load Backends       : %8.3f',[Perf[0]]));
          Memo1.Lines.Add(FormatDot('Whisper Load Model          : %8.3f',[Perf[1]]));
          Memo1.Lines.Add(FormatDot('Whisper Load Heat           : %8.3f',[Perf[2]]));
          Memo1.Lines.Add(FormatDot('Whisper Load Run            : %8.3f',[Perf[3]]));
          Memo1.Lines.Add(FormatDot('Whisper Total Runtime       : %8.3f',[Perf[4]]));
          Memo1.Lines.Add('');

          Info := Format_JSON(Whisp.GetSystemInfoJson);
          Memo1.Lines.Add(Format('Info : %s',[Info]));
     finally
      sw.Free;
    end;
  finally
    Whisp.Free;
    SetLength(Tokens, 0);
  end;

end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetMultiByteConversionCodePage(CP_UTF8);
  TokenCount := 256;
  BatchCount := 64;
  BatchSize := 5;
  PromptCount := 16;
end;

end.

