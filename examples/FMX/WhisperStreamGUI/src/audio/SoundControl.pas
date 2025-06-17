unit SoundControl;

interface

uses
  System.SysUtils, System.Types, System.Classes, FMX.Types,
  System.Generics.Collections,
  sdl2;

type
  TAudioSDLDevice = class
  strict private
    FDevIndex: Integer;
    FDevName: String;
  public
    property DevIndex: Integer read FDevIndex write FDevIndex;
    property DevName: String read FDevName write FDevName;
  end;

  TAudioSDL = Class
  strict private
    FDevIn: TObjectList<TAudioSDLDevice>;
    FDevInSelected: Integer;
    FDevOut: TObjectList<TAudioSDLDevice>;
    FDevOutSelected: Integer;
    function Init: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property DevIn: TObjectList<TAudioSDLDevice> read FDevIn;
    property DevOut: TObjectList<TAudioSDLDevice> read FDevOut;
    property DevInSelected: Integer read FDevInSelected;
    property DevOutSelected: Integer read FDevOutSelected;
  End;

  ESDL2Error = class(Exception);

const
  SDL_AUDIO_ISCAPTURE = 1;
  SDL_AUDIO_ISOUTPUT = 0;

var
  AudioSDL: TAudioSDL;

implementation

{ TAudioSDL }

constructor TAudioSDL.Create;
begin
  if AudioSDL <> Nil then
    Abort
  else
    begin
      FDevIn := TObjectList<TAudioSDLDevice>.Create;
      FDevOut := TObjectList<TAudioSDLDevice>.Create;
      FDevInSelected := -1;
      FDevOutSelected := -1;
      if Init then
        AudioSDL := Self;
    end;
end;

destructor TAudioSDL.Destroy;
begin
  if AudioSDL = Self then
    begin
      DevIn.Free;
      DevOut.Free;
      AudioSDL := Nil;
    end;

  inherited Destroy;
end;

procedure FreeGlobalObjects;
begin
  if AudioSDL <> nil then
    AudioSDL.Free;
end;

function TAudioSDL.Init: Boolean;
var
  Flag: TSDL_Init;
  NumDev: Integer;
  Device: TAudioSDLDevice;
  I: Integer;
  {
  Chosen: PAnsiString;
  AudioSpec: PSDL_AudioSpec;
  }
begin
  Result := False;
  Flag := SDL_INIT_AUDIO;
  begin
    try
      if SDL_Init(Flag) <> 0 then
        raise ESDL2Error.Create('SDL_Init failed: Flag = ' + IntToStr(Flag));
      if Flag =  SDL_INIT_AUDIO then
        begin
          NumDev := SDL_GetNumAudioDevices(SDL_AUDIO_ISCAPTURE);
          if(NumDev > 0) then
            begin
//              WriteLn(Format('Found %d audio input devices',[NumDev]));
              for I := 0 to NumDev - 1 do
                begin
                  Device := TAudioSDLDevice.Create;
                  Device.DevIndex := I;
                  Device.DevName := String(SDL_GetAudioDeviceName(I, SDL_AUDIO_ISCAPTURE));
                  FDevIn.Add(Device);
                end;
            end;

          NumDev := SDL_GetNumAudioDevices(SDL_AUDIO_ISOUTPUT);
          if(NumDev > 0) then
            begin
              for I := 0 to NumDev - 1 do
                begin
                  Device := TAudioSDLDevice.Create;
                  Device.DevIndex := I;
                  Device.DevName := String(SDL_GetAudioDeviceName(I, SDL_AUDIO_ISCAPTURE));
                  FDevOut.Add(Device);
                end;
            end;
            {
            if SDL_GetDefaultAudioInfo(@Chosen, @AudioSpec, SDL_AUDIO_ISCAPTURE) = 0 then
              begin
                SDL_Free(@Chosen);
              end;

            if SDL_GetDefaultAudioInfo(@Chosen, @AudioSpec, SDL_AUDIO_ISOUTPUT) = 0 then
              begin
                SDL_Free(@Chosen);
              end;
            }
            Result := True;
        end;
    except
      on E: ESDL2Error do
      try
        SDL_Quit;
      except
        raise;
      end;
    end;
    SDL_Quit;
  end;
end;

begin
  AddExitProc(FreeGlobalObjects);
end.
