unit WhisperCliMain;

interface
  uses SysUtils, Classes, Whisper, WhisperTypes, GgmlExternal, WhisperUtils;

type
  TWhisperCli = class
    strict private
      BackendsLoaded: Boolean;
      procedure DoWhisper;
    public
      constructor Create;
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
  WhisperCli := TWhisperCli.Create;
  WhisperCli.MainLoop;
end;

constructor TWhisperCli.Create;
begin
  // Just an empty constructor
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
procedure TWhisperCli.DoWhisper;
var
  I: Integer;
  Info: String;
  Whisp: TWhisper;
  NMels: Int32;
  Tokens: array [0..MaxBenchToken-1] of TWhisperToken;
  Timings: PWhisperTimings;
  ModelFile: String;
  sw: TMilliTimer;
  Perf: Array[0..7] of Single; // A few spare just in case
begin
  sw := TMilliTimer.Create;
  try
    Whisp := TWhisper.Create;
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

    {$IF (OS_PLATFORM_TYPE = 'WIN64')}
      ModelFile := 'D:\models\ggml-base.en.bin';
    {$ELSEIF (OS_PLATFORM_TYPE = 'LINUX64')}
      ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
    {$ELSEIF (OS_PLATFORM_TYPE = 'OSXARM64')}
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

          Timings := Whisp.GetTimings;

          WriteLn(Format('Whisper NMels               : %d',[Nmels]));
          if Timings <> Nil then
            begin
              WriteLn(Format('Whisper Sample ms           : %3.8f',[Timings^.SampleMs]));
              WriteLn(Format('Whisper Encode ms           : %3.8f',[Timings^.EncodeMs]));
              WriteLn(Format('Whisper Decode ms           : %3.8f',[Timings^.DecodeMs]));
              WriteLn(Format('Whisper Batch ms            : %3.8f',[Timings^.BatchdMs]));
              WriteLn(Format('Whisper Prompt ms           : %3.8f',[Timings^.PromptMs]));
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
