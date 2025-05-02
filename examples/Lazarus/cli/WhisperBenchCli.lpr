program WhisperBenchCli;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Crt,
  { you can add units after this }
  WhisperTypes, GgmlTypes, GgmlExternal,
  WhisperExternal, Whisper, WhisperUtils;

{$I platform.inc}
type

  { WhisperCli }

  WhisperCli = class(TCustomApplication)
  private
    BackendsLoaded: Boolean;
    PromptCount: Integer;
    BatchCount: Integer;
    BatchSize: Integer;
    TokenCount: Integer;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure DoWhisper;
  end;

const
  Threads: Integer = 4;
  MaxToken = 256;

{ WhisperCli }
procedure WhisperCli.DoWhisper;
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
  GgmlBackendCount: Int64;
  WhisperBackendCount: Int64;
begin
  SetLength(Tokens, TokenCount);

  for I := 0 to TokenCount - 1 do
      Tokens[I] := 0;

  Whisp := TWhisper.Create;
  try
    sw := TMilliTimer.Create;
    try
      if not BackendsLoaded then
        begin
  //        Whisp.LoadBackends;
          Whisp.LoadBestBackend('cpu');
          Whisp.LoadBestBackend('blas');
          Whisp.LoadBestBackend('rpc');
  //        Whisp.LoadBestBackend('vulkan');
  //        Whisp.LoadBestBackend('cuda');

          BackendsLoaded := True;
        end;
      Perf[0] := sw.Elapsed; // Loaded Backends

    {$IF DEFINED(OS_WIN64)}
      ModelFile := 'D:\models\ggml-base.en.bin';
    {$ELSEIF DEFINED(OS_LINUX64)}
      ModelFile := {$ifdef fpc}GetUserDir(){$else}TPath.GetHomePath()+ '/' {$endif} + 'models/ggml-base.en.bin';
    {$ELSEIF DEFINED(OS_LINUX64ARM)}
      ModelFile := {$ifdef fpc}GetUserDir(){$else}TPath.GetHomePath()+ '/' {$endif} + 'models/ggml-base.en.bin';
    {$ELSEIF DEFINED(OS_OSX64ARM)}
      ModelFile := {$ifdef fpc}GetUserDir(){$else}TPath.GetHomePath()+ '/' {$endif} + 'models/ggml-base.en.bin';
    {$ELSEIF DEFINED(OS_OSX64)}
      ModelFile := {$ifdef fpc}GetUserDir(){$else}TPath.GetHomePath()+ '/' {$endif} + 'models/ggml-base.en.bin';
    {$ELSE}
      Unsupported Platform
    {$ENDIF}
      GgmlBackendCount := GgmlBackendGetDeviceCount();
      WriteLn(stderr, Format('Available Backend Devices : %d',[GgmlBackendCount]));
      WriteLn(stderr, '');

      if Whisp.LoadModel(ModelFile) then
        begin
          WhisperBackendCount := Whisp.GetBackendCount;

          if WhisperBackendCount < 1 then
            begin
              WriteLn(stderr, Format('Insufficient Devices',[WhisperBackendCount]));
              Exit;
            end;
          WriteLn(stderr, Format('Preferred Device (of %d)',[WhisperBackendCount]));

          dev := Whisp.GetPreferredBackend;
          WriteLn(stderr, Format('Device      : %s',[dev.devName]));
          WriteLn(stderr, Format('Description : %s',[dev.devDesc]));
          WriteLn(stderr, '');

          if WhisperBackendCount > 1 then
            begin
              WriteLn(stderr, 'Backup Device(s)');
              for I := 1 to WhisperBackendCount - 1 do
                begin
                  dev := Whisp.GetIndexedBackend(I);
                  WriteLn(stderr, Format('Device      : %s',[dev.devName]));
                  WriteLn(stderr, Format('Description : %s',[dev.devDesc]));
                  WriteLn(stderr, '');
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

          WriteLn(stderr, Format('Whisper NMels               : %d',[Nmels]));
          if Timings <> Nil then
            begin
              Writeln(stderr, Format('Whisper Sample ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NSample, Timings^.SampleMs * Timings^.NSample, Timings^.SampleMs]));
              Writeln(stderr, Format('Whisper Encode ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NEncode, Timings^.EncodeMs * Timings^.NEncode, Timings^.EncodeMs]));
              Writeln(stderr, Format('Whisper Decode ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NDecode, Timings^.DecodeMs * Timings^.NDecode, Timings^.DecodeMs]));
              Writeln(stderr, Format('Whisper Batch ms  x %6d     : %12.4f tot / %12.4f per',[Timings^.NBatchd, Timings^.BatchdMs * Timings^.NBatchd, Timings^.BatchdMs]));
              Writeln(stderr, Format('Whisper Prompt ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NPrompt, Timings^.PromptMs * Timings^.NPrompt, Timings^.PromptMs]));
            end;
          WriteLn(stderr, '');
          WriteLn(stderr, FormatDot('Whisper Load Backends       : %8.3f',[Perf[0]]));
          WriteLn(stderr, FormatDot('Whisper Load Model          : %8.3f',[Perf[1]]));
          WriteLn(stderr, FormatDot('Whisper Load Heat           : %8.3f',[Perf[2]]));
          WriteLn(stderr, FormatDot('Whisper Load Run            : %8.3f',[Perf[3]]));
          WriteLn(stderr, FormatDot('Whisper Total Runtime       : %8.3f',[Perf[4]]));
          WriteLn(stderr, '');

          Info := Format_JSON(Whisp.GetSystemInfoJson);
          WriteLn(stderr, Format('Info : %s',[Info]));

        end;
    finally
      sw.Free;
    end;
  finally
    Whisp.Free;
    SetLength(Tokens, 0);
  end;

end;

procedure WhisperCli.DoRun;
var
  ErrorMsg: String;
  ch: Char;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  repeat
    TokenCount := 256;
    BatchCount := 64;
    BatchSize := 5;
    PromptCount := 16;

    DoWhisper;
    writeln(stderr, '');
    writeln(stderr, 'Press X to Quit');
    writeln(stderr, '');
    ch := ReadKey;
  until (ch = #88) or (ch = #120);
  // stop program loop
  Terminate;
end;

constructor WhisperCli.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  SetMultiByteConversionCodePage(CP_UTF8);
  InitializeGgmlLibrary;
  InitializeWhisperLibrary;
end;

destructor WhisperCli.Destroy;
begin
  FinalizeWhisperLibrary;
  FinalizeGgmlLibrary;
  inherited Destroy;
end;

procedure WhisperCli.WriteHelp;
begin
  { add your help code here }
  writeln(stderr, 'Usage: ', ExeName, ' -h');
end;

var
  Application: WhisperCli;
begin
  Application:=WhisperCli.Create(nil);
  Application.Title:='WhisperBenchCli';
  Application.Run;
  Application.Free;
end.

