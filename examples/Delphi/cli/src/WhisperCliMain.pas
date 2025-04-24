unit WhisperCliMain;

interface
  uses SysUtils, Whisper, WhisperTypes, GgmlExternal;

procedure WhisperMain;

const
  MaxBenchToken = 256;
  Threads = 4;

implementation

procedure WhisperMain;
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
    Whisp.LoadBackends;

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

        WriteLn(Format('Whisper NMels               : %d',[Nmels]));
        if Timings <> Nil then
          begin
            WriteLn(Format('Whisper Sample ms           : %3.8f',[Timings^.SampleMs]));
            WriteLn(Format('Whisper Encode ms           : %3.8f',[Timings^.EncodeMs]));
            WriteLn(Format('Whisper Decode ms           : %3.8f',[Timings^.DecodeMs]));
            WriteLn(Format('Whisper Batch ms            : %3.8f',[Timings^.BatchdMs]));
            WriteLn(Format('Whisper Prompt ms           : %3.8f',[Timings^.PromptMs]));
          end;
      end;
  finally
    Whisp.Free;
  end;
end;

end.
