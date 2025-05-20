unit WhisperBenchMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Menus,
  Whisper, Settings;

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
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MenuItem3Click(Sender: TObject);
    procedure CheckBoxChange(Sender: TObject);
  private
    { Private declarations }
    Whisp: TWhisper;
    Settings: TSettings;
    BackendsLoaded: Boolean;
    PromptCount: Integer;
    BatchCount: Integer;
    BatchSize: Integer;
    TokenCount: Integer;
    procedure RunBench;
    procedure SelectModel;
    function NoModel: Boolean;
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
  WhisperExternal, WhisperUtils;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Enabled := False;
  Memo1.Lines.Clear;
  Application.ProcessMessages;
  RunBench;
  Button1.Enabled := True;
end;

function TForm1.NoModel: Boolean;
begin
  Result := False;
  if Settings.LastUsedModel.IsEmpty() then
    Result := True;
  if Settings.ModelDirectory.IsEmpty() then
    Result := True;
end;

procedure TForm1.RunBench;
var
  Info: String;
  I: Integer;
  NMels: Int32;
  Tokens: TWhisperTokenArray;
  Timings: PWhisperActivity;
  sw: TMilliTimer;
  Perf: Array[0..7] of Single; // A few spare just in case
  dev: TBackendDevice;
  Model: String;
  GgmlBackendCount: Integer;
  WhisperBackendCount: Integer;
begin
  if (NoModel) then
    SelectModel;

  if (NoModel) then
    Exit;

  SetLength(Tokens, TokenCount);

  for I := 0 to TokenCount - 1 do
      Tokens[I] := 0;

  try
    sw := TMilliTimer.Create;
    try
      if not BackendsLoaded then
        begin
//          Whisp.LoadBackends;

          Whisp.LoadBestBackend('cpu');
          {$IFDEF MACOS}
          if Checkbox1.IsChecked then
            Whisp.LoadBestBackend('blas');
          if Checkbox2.IsChecked then
            Whisp.LoadBestBackend('metal');
          if Checkbox3.IsChecked then
            Whisp.LoadBestBackend('rpc');
          {$ELSE}
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
          {$ENDIF}
          BackendsLoaded := True;
        end;
      Perf[0] := sw.Elapsed; // Loaded Backends



      GgmlBackendCount := GgmlBackendGetDeviceCount();
      Memo1.Lines.Add(Format('Available Backend Devices : %d',[GgmlBackendCount]));
      Memo1.Lines.Add('');

      Model := TPath.Combine(Settings.ModelDirectory, Settings.LastUsedModel);
      Memo1.Lines.Add('Using Model : ' + Model);
      Memo1.Lines.Add('');

      if not Whisp.IsModelLoaded then
        Whisp.LoadModel(Model, {$IFDEF MACOS}Checkbox4.IsChecked{$ELSE}True{$ENDIF});

      if Whisp.IsModelLoaded then
        begin
          Perf[1] := sw.Elapsed; // Loaded Model

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

          Whisp.ResetTimings;

          if Whisp.SetMel(Nil, 0, NMels) <> WHISPER_SUCCESS then
            Exit;

          Perf[2] := sw.Elapsed; // Loaded Model

          // Heat
          if Whisp.Encode(0, Threads) <> WHISPER_SUCCESS then
            Exit;
          if Whisp.Decode(Tokens, TokenCount, 0, Threads) <> WHISPER_SUCCESS then
            Exit;
          if Whisp.Decode(Tokens, 1, TokenCount, Threads) <> WHISPER_SUCCESS then
            Exit;

          Whisp.ResetTimings;

          Perf[3] := sw.Elapsed; // Done Heat

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

          Perf[4] := sw.Elapsed; // Done Run
          Perf[5] := sw.TotalElapsed; // Done Run

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
          Memo1.Lines.Add(FormatDot('Whisper Load Heat           : %8.3f',[Perf[3]]));
          Memo1.Lines.Add(FormatDot('Whisper Load Run            : %8.3f',[Perf[4]]));
          Memo1.Lines.Add(FormatDot('Whisper Total Runtime       : %8.3f',[Perf[5]]));
          Memo1.Lines.Add('');

          Info := Format_JSON(Whisp.GetSystemInfoJson);
          Memo1.Lines.Add(Format('Info : %s',[Info]));

        end;
    finally
      sw.Free;
    end;
  finally
    SetLength(Tokens, 0);
  end;

end;

procedure TForm1.CheckBoxChange(Sender: TObject);
var
  CBX: TCheckBox;
begin
  if Sender is TCheckBox then
    begin
      CBX := Sender as TCheckBox;
      Settings.GenOpt[CBX.Tag] := CBX.IsChecked;
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Settings.GenOpt[0] := CheckBox1.IsChecked;
  Settings.GenOpt[1] := CheckBox2.IsChecked;
  Settings.GenOpt[2] := CheckBox3.IsChecked;
  Settings.GenOpt[3] := CheckBox4.IsChecked;
  Settings.Save;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Settings := TSettings.Create;
  {$IFDEF MSWINDOWS}
  SetWhisperLibraryPath('C:\\src\\Whisper\\lib\\windows\\x64\\');
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);
  DebugLogInit(TPath.Combine(Settings.AppHome, 'Whisper.log'));
  DebugLog.Info('Start');
  TokenCount := 256;
  BatchCount := 64;
  BatchSize := 5;
  PromptCount := 16;
  Caption := AppName;
  Width := 640;
  Height := 960;
  Button1.Text := 'Benchmark';
  {$IFDEF MACOS}
  CheckBox1.Text := 'Blas';
  CheckBox2.Text := 'Metal';
  CheckBox3.Text := 'RPC';
  CheckBox4.Text := 'State';
  {$ELSE}
  CheckBox1.Text := 'Blas';
  CheckBox2.Text := 'Cuda First';
  CheckBox3.Text := 'Cuda';
  CheckBox4.Text := 'Vulkan';
  {$ENDIF}
  CheckBox1.IsChecked := Settings.GenOpt[0];
  CheckBox2.IsChecked := Settings.GenOpt[1];
  CheckBox3.IsChecked := Settings.GenOpt[2];
  CheckBox4.IsChecked := Settings.GenOpt[3];

  Memo1.Lines.Add('Whisper path is ' + WhisperGlobalLibraryPath);
  Memo1.Lines.Add('Settings path is ' + Settings.AppHome);
  Memo1.Lines.Add(Format('Model is %s - %s',[Settings.ModelDirectory, Settings.LastUsedModel]));
  Whisp := TWhisper.Create;

end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  DebugLog.Info('Stop');
  FreeAndNil(Settings);
//  FreeAndNil(Whisp);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Caption := AppName + ' (' + IntToStr(Width) + ' x ' + IntToStr(Height) + ')';
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var
  ModelDirectory: String;
begin
  ModelDirectory := TPath.GetDocumentsPath();
  SelectDirectory('Select Model Dirctory', ModelDirectory, ModelDirectory);
  Memo1.Lines.Add('Model path is ' + ModelDirectory);
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  SelectModel;
end;

procedure TForm1.SelectModel;
var
  FP, FN: String;
begin
  if Settings.ModelDirectory <> '' then
    OpenDialog1.InitialDir := Settings.ModelDirectory;

  OpenDialog1.Filter := 'Model Files|*.bin';
  if OpenDialog1.Execute then
    begin
      // check 4cc = lmgg then do this
      FP := ExtractFilePath(OpenDialog1.Filename);
      FN := ExtractFileName(OpenDialog1.Filename);

      Memo1.Lines.Add(Format('Model is %s - %s',[FP, FN]));
      Settings.LastUsedModel := FN;
      Settings.ModelDirectory := FP;
      Settings.Save;
      Memo1.Lines.Add(Format('Settings Saved',[FP, FN]));
    end;
end;

end.
