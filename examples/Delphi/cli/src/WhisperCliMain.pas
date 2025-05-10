unit WhisperCliMain;

interface

uses
 SysUtils, IOUtils, Classes,
 WhisperLog,
 WhisperTypes, GgmlTypes, GgmlExternal,
 WhisperExternal, Whisper, WhisperUtils;

type
  TWhisperCli = class
    strict private
      BackendsLoaded: Boolean;
      procedure DoWhisper;
    public
      constructor Create;
      Destructor Destroy; override;
      procedure MainLoop;
  end;

var
  WhisperCli: TWhisperCli;

const
  MaxBenchToken = 256;
  Threads = 4;

procedure WhisperMain;

implementation

procedure WhisperMain;
begin
  DebugLogInit('Whisper.log');
//  SetWhisperLibraryPath('C:\src\Whisper\lib\windows\x64');
  SetWhisperLibraryPath('..\..\..\..\..\lib\windows\x64');
  WhisperCli := TWhisperCli.Create;
  WhisperCli.MainLoop;
end;

constructor TWhisperCli.Create;
begin
  WriteLn('Whisper path is ' + WhisperGlobalLibraryPath);
  SetMultiByteConversionCodePage(CP_UTF8);
end;

procedure TWhisperCli.MainLoop;
var
  s: String;
begin
  repeat
    DoWhisper;
    writeln('');
    writeln('Press X to Quit');
    writeln('');
    ReadLn(s);
  until (s = 'x') or (s = 'X');
end;

destructor TWhisperCli.Destroy;
begin
 inherited;
end;

procedure TWhisperCli.DoWhisper;
var
  I: Integer;
  Info: String;
  Whisp: TWhisper;
  NMels: Int32;
  Tokens: array [0..MaxBenchToken-1] of TWhisperToken;
  Timings: PWhisperActivity;
  ModelFile: String;
  sw: TMilliTimer;
  Perf: Array[0..7] of Single; // A few spare just in case
begin
//  LogTest();
  sw := TMilliTimer.Create;
  try
    Whisp := TWhisper.Create;
    try
      if not BackendsLoaded then
        begin
          if(paramcount() = 4) then
            begin
              Whisp.LoadBackends;
            end
          else
            begin
              if(paramcount() = 1) then
                Whisp.LoadBestBackend('blas');
              if(paramcount() = 2) then
                Whisp.LoadBestBackend('vulkan');
              if(paramcount() = 3) then
                Whisp.LoadBestBackend('cuda');
              Whisp.LoadBestBackend('cpu');
            end;
          BackendsLoaded := True;
        end;

      WriteLn(Format('Drivers = %d',[ GgmlBackendGetDeviceCount()]));

      Perf[0] := sw.Elapsed; // Loaded Backends

    {$IF (OS_PLATFORM_TYPE = 'WIN64')}
      ModelFile := 'D:\models\ggml-base.en.bin';
    {$ELSEIF (OS_PLATFORM_TYPE = 'LINUX64')}
      ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
    {$ELSEIF (OS_PLATFORM_TYPE = 'OSX64ARM')}
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

          Timings := Whisp.GetActivity;

          WriteLn(Format('Whisper NMels               : %d',[Nmels]));
          if Timings <> Nil then
            begin
              Writeln(Format('Whisper Sample ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NSample, Timings^.SampleMs * Timings^.NSample, Timings^.SampleMs]));
              Writeln(Format('Whisper Encode ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NEncode, Timings^.EncodeMs * Timings^.NEncode, Timings^.EncodeMs]));
              Writeln(Format('Whisper Decode ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NDecode, Timings^.DecodeMs * Timings^.NDecode, Timings^.DecodeMs]));
              Writeln(Format('Whisper Batch ms  x %6d     : %12.4f tot / %12.4f per',[Timings^.NBatchd, Timings^.BatchdMs * Timings^.NBatchd, Timings^.BatchdMs]));
              Writeln(Format('Whisper Prompt ms x %6d     : %12.4f tot / %12.4f per',[Timings^.NPrompt, Timings^.PromptMs * Timings^.NPrompt, Timings^.PromptMs]));
            end;
          WriteLn('');
          WriteLn(FormatDot('Whisper Load Backends       : %8.3f',[Perf[0]]));
          WriteLn(FormatDot('Whisper Load Model          : %8.3f',[Perf[1]]));
          WriteLn(FormatDot('Whisper Load Heat           : %8.3f',[Perf[2]]));
          WriteLn(FormatDot('Whisper Load Run            : %8.3f',[Perf[3]]));
          WriteLn(FormatDot('Whisper Total Runtime       : %8.3f',[Perf[4]]));
          WriteLn('');

          Info := Format_JSON(Whisp.GetSystemInfoJson);
          WriteLn(Format('Info : %s',[Info]));
          Perf[5] := sw.Elapsed; // Timer before Destruction of TWhisper
        end;
    finally
      Whisp.Free;
    end;
  finally
    Perf[6] := sw.Elapsed; // Time for TWhisper Destruction
    WriteLn('');
    WriteLn(FormatDot('Whisper Destruction Time    : %8.3f',[Perf[6]]));
    WriteLn('');
    sw.Free;
  end;
end;

end.
