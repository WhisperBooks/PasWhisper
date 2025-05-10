unit WhisperBenchMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Menus;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    BackendsLoaded: Boolean;
    PromptCount: Integer;
    BatchCount: Integer;
    BatchSize: Integer;
    TokenCount: Integer;
    procedure RunBench;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  Threads: Integer = 4;
  MaxToken = 256;
  Appname = 'WhisperBenchGUI';

implementation

{$R *.fmx}

uses
  WhisperLog,
  BaseDevice,
  WhisperTypes, GgmlTypes, IOUtils, GgmlExternal,
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
  GgmlBackendCount: Integer;
  WhisperBackendCount: Integer;
begin
  // LogTest();
  SetLength(Tokens, TokenCount);

  for I := 0 to TokenCount - 1 do
      Tokens[I] := 0;

  Whisp := TWhisper.Create;
  try
    sw := TMilliTimer.Create;
    try
      if not BackendsLoaded then
        begin
//          Whisp.LoadBackends;

          Whisp.LoadBestBackend('cpu');
          if Checkbox1.IsChecked then
            Whisp.LoadBestBackend('blas');
          Whisp.LoadBestBackend('rpc');
          if Checkbox2.IsChecked then
            begin
              if Checkbox3.IsChecked then
                Whisp.LoadBestBackend('cuda');
              if Checkbox4.IsChecked then
                Whisp.LoadBestBackend('vulkan');
            end
          else
            begin
              if Checkbox4.IsChecked then
                Whisp.LoadBestBackend('vulkan');
              if Checkbox3.IsChecked then
                Whisp.LoadBestBackend('cuda');
            end;

          BackendsLoaded := True;
        end;
      Perf[0] := sw.Elapsed; // Loaded Backends

    {$IF DEFINED(MSWINDOWS)}
      ModelFile := 'D:\models\ggml-base.en.bin';
    {$ELSEIF DEFINED(OS_WIN64)}
      ModelFile := 'D:\models\ggml-base.en.bin';
    {$ELSEIF DEFINED(WIN32)}
      ModelFile := 'D:\models\ggml-base.en.bin';
    {$ELSEIF DEFINED(LINUX64)}
      ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
    {$ELSEIF DEFINED(OS_OSX64ARM)}
      ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
    {$ELSEIF DEFINED(OSX64)}
      ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
    {$ELSE}
      Unsupported Platform
    {$ENDIF}
      GgmlBackendCount := GgmlBackendGetDeviceCount();
      Memo1.Lines.Add(Format('Available Backend Devices : %d',[GgmlBackendCount]));
      Memo1.Lines.Add('');

      if Whisp.LoadModel(ModelFile) then
        begin
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

          Timings := Whisp.GetActivity;

          // Log.d('Hello');
          Memo1.Lines.Add(FormatDot('Whisper NMels               : %d',[Nmels]));
          if(Timings <> Nil) then
            begin
              Memo1.Lines.Add(FormatDot('Whisper Sample ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NSample, Timings^.SampleMs * Timings^.NSample, Timings^.SampleMs]));
              Memo1.Lines.Add(FormatDot('Whisper Encode ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NEncode, Timings^.EncodeMs * Timings^.NEncode, Timings^.EncodeMs]));
              Memo1.Lines.Add(FormatDot('Whisper Decode ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NDecode, Timings^.DecodeMs * Timings^.NDecode, Timings^.DecodeMs]));
              Memo1.Lines.Add(FormatDot('Whisper Batch  ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NBatchd, Timings^.BatchdMs * Timings^.NBatchd, Timings^.BatchdMs]));
              Memo1.Lines.Add(FormatDot('Whisper Prompt ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NPrompt, Timings^.PromptMs * Timings^.NPrompt, Timings^.PromptMs]));
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
    SetLength(Tokens, 0);
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  SetWhisperLibraryPath('C:\src\Whisper\lib\windows\x64\');
  SetMultiByteConversionCodePage(CP_UTF8);
  DebugLogInit('Whisper.log');
  DebugLog.Info('Start');
  TokenCount := 256;
  BatchCount := 64;
  BatchSize := 5;
  PromptCount := 16;
  Caption := AppName;
  Width := 640;
  Height := 960;
  Button1.Text := 'Benchmark';
  CheckBox1.Text := 'Blas';
  CheckBox2.Text := 'Cuda First';
  CheckBox3.Text := 'Cuda';
  CheckBox4.Text := 'Vulkan';
  CheckBox3.isChecked := True;
  Memo1.Lines.Add('Whisper path is ' + WhisperGlobalLibraryPath);
  Memo1.Lines.Add('GGML path is ' + GGMLGlobalLibraryPath);

end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  DebugLog.Info('Stop');
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Caption := AppName + ' (' + IntToStr(Width) + ' x ' + IntToStr(Height) + ')';
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add(Format('TGgmlBackend       : %d',[SizeOf(TGgmlBackend)]));
  Memo1.Lines.Add(Format('TGgmlBackendDevice : %d',[SizeOf(TGgmlBackendDevice)]));
  Memo1.Lines.Add(Format('IGgmlBackendDevice : %d',[SizeOf(IGgmlBackendDevice)]));
end;

end.
