program WhisperBenchCli;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Crt, Whisper, WhisperTypes, GGMLExternal
  { you can add units after this };
{$I platform.inc}
type

  { WhisperCli }

  WhisperCli = class(TCustomApplication)
  private
    BackendsLoaded: Boolean;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure DoWhisper;
  end;

const
  MaxBenchToken = 256;
  Threads = 4;

{ WhisperCli }
procedure WhisperCli.DoWhisper;
var
  Info: String;
  I: Integer;
  Whisp: TWhisper;
  NMels: Int32;
  Tokens: array [0..MaxBenchToken-1] of TWhisperToken;
  Timings: PWhisperTimings;
  ModelFile: String;
begin
  Whisp := TWhisper.Create;
  try
    if not BackendsLoaded then
      begin
        Whisp.LoadBestBackend('cuda');
        Whisp.LoadBestBackend('blas');
        Whisp.LoadBestBackend('cpu-sandybridge');
        BackendsLoaded := True;
      end;
  {$IF (OS_PLATFORM_TYPE = 'WIN64')}
  ModelFile := 'd:\models\ggml-base.en.bin';
  {$ELSEIF (OS_PLATFORM_TYPE = 'LINUX64')}
    ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
  {$ELSEIF (OS_PLATFORM_TYPE = 'OSXARM64')}
    ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
  {$ELSEIF (OS_PLATFORM_TYPE = 'OSX64')}
    ModelFile := TPath.GetHomePath() + '/models/ggml-base.en.bin';
  {$ELSE}
    ModelFile := 'd:\models\ggml-base.en.bin';
  {$ENDIF}
    if Whisp.LoadModel(ModelFile, True) then
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

        WriteLn(stderr, Format('Whisper NMels               : %d',[Nmels]));
        if Timings <> Nil then
          begin
            WriteLn(stderr, Format('Whisper Sample ms           : %3.8f',[Timings^.SampleMs]));
            WriteLn(stderr, Format('Whisper Encode ms           : %3.8f',[Timings^.EncodeMs]));
            WriteLn(stderr, Format('Whisper Decode ms           : %3.8f',[Timings^.DecodeMs]));
            WriteLn(stderr, Format('Whisper Batch ms            : %3.8f',[Timings^.BatchdMs]));
            WriteLn(stderr, Format('Whisper Prompt ms           : %3.8f',[Timings^.PromptMs]));
          end;
      Info := Whisp.GetSystemInfoJson;
      WriteLn(stderr, Format('Sysinfo : %s',[Info]));

      end;
  finally
    Whisp.Free;
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
end;

destructor WhisperCli.Destroy;
begin
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

