unit Settings;

// {$DEFINE CLEANSTART}

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes,
  System.Variants{, FMX.Forms, FMX.Dialogs, StyleModel};

const
  GenOptCount = 4;

type

  TGenOpts = Array[0..GenOptCount - 1] of Boolean;

  TSettingsRec = record
    WipeOnStart: Boolean;
    LastUsedModel: String;
    ModelDirectory: String;
    GenOpt: TGenOpts;
    AppHome: String;
    SettingsHome: String;
    WhisperVersion: String;
  end;

  TSettings = Class(TObject)
  strict private
    FSettings: TSettingsRec;
    function GetAppHome: String;
    function GetLastUsedModel: String;
    procedure SetLastUsedModel(AValue: String);
    function GetModelDirectory: String;
    procedure SetModelDirectory(AValue: String);
    function GetGenOpt(Index: Cardinal): Boolean;
    procedure SetGenOpt(Index: Cardinal; AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; Override;
    procedure Initialise;
    procedure Load;
    procedure MakeDefaultValues;
    procedure Save;
    property AppHome: String read GetAppHome;
    property LastUsedModel: String read GetLastUsedModel write SetLastUsedModel;
    property ModelDirectory: String read GetModelDirectory write SetModelDirectory;
    property GenOpt[Index: Cardinal]: Boolean read GetGenOpt write SetGenOpt;
  end;

var
  InstallRequired: Boolean;
  VersionUpdate: Boolean;


  SystemActive: Boolean;

  FrameCount: Integer;
  EnableGPU: Boolean;
  AllowGPU: Boolean;

const
  appname: String = 'WhisperBenchGUI';
  pypath: String = 'python';
  pyver: String = '3.9';
  pyexe: String = 'python.exe';
  pyshim: String = 'pysrc';
  pycode: String = 'SystemCode.py';
  appver: String = '1.0.2';

  APIBase: String = 'https://peardox.com/Lartis/';


implementation

uses
  JSON.Serializers;

{ TSettings }

constructor TSettings.Create;
begin
  MakeDefaultValues;
  if FileExists(IncludeTrailingPathDelimiter(FSettings.SettingsHome) + 'Settings.json') then
    begin
      Load;
      // If AppHome has been removed (e.g. USB) then it won't exist any more
      // so reset it back to default in that situation
    end
  else
    Initialise;
end;

destructor TSettings.Destroy;
begin
  inherited;
end;

function TSettings.GetAppHome: String;
begin
  Result := FSettings.AppHome;
end;

function TSettings.GetGenOpt(Index: Cardinal): Boolean;
begin
  Result := FSettings.GenOpt[Index];
end;

function TSettings.GetLastUsedModel: String;
begin
  Result := FSettings.LastUsedModel;
end;

function TSettings.GetModelDirectory: String;
begin
  Result := FSettings.ModelDirectory;
end;

procedure TSettings.Initialise;
  begin
  if not DirectoryExists(FSettings.AppHome) and not FSettings.AppHome.IsEmpty() then
    begin
      ForceDirectories(FSettings.AppHome);
      InstallRequired := True;
    end;

  FSettings.WhisperVersion := appver;
  Save;
end;

procedure TSettings.Load;
var
  lSerializer: TJsonSerializer;
  JsonText: String;
  LSettings: TSettingsRec;
begin
  if FSettings.SettingsHome.IsEmpty() then
    Exit;

  try
    JsonText := TFile.ReadAllText(IncludeTrailingPathDelimiter(FSettings.SettingsHome) + 'Settings.json');
  except
     on E : Exception do
       Raise Exception.Create('LoadSystemSettings - Exception : Class = ' +
        E.ClassName + ', Message = ' + E.Message);
  end;
  if JsonText = '' then
    begin
      // What to do it you can't load?
    end
  else
    begin
      lSerializer := TJsonSerializer.Create;
      LSerializer.MaxDepth := 2;
      try
        try
          FSettings := lSerializer.Deserialize<TSettingsRec>(JsonText);
          if FSettings.WhisperVersion <> appver then
            VersionUpdate := True;
        except
         on E : EJsonSerializationException do
           LSettings := Default(TSettingsRec);
         on E : Exception do   // EJsonSerializationException
           Raise Exception.Create('LoadSystemSettings - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
        end;
      finally
        FreeAndNil(lSerializer);
      end;
    end;
end;

procedure TSettings.MakeDefaultValues;
var
  RealHome: String;
begin
  InstallRequired := False;
  VersionUpdate := False;
  EnableGPU := False;
  {$IF DEFINED(MACOS)}
  FSettings.GenOpt[0] := True;
  FSettings.GenOpt[1] := True;
  FSettings.GenOpt[2] := False;
  FSettings.GenOpt[3] := True;
  {$ELSE}
  FSettings.GenOpt[0] := False;
  FSettings.GenOpt[1] := False;
  FSettings.GenOpt[2] := True;
  FSettings.GenOpt[3] := False;
  {$ENDIF}

  AllowGPU := EnableGPU;

  {$IF DEFINED(MACOS)}
  RealHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath) + appname;
  {$ELSEIF DEFINED(LINUX)}
  RealHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + '.' + appname;
  {$ELSE}
  RealHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + appname;
  {$ENDIF}
  // System agnostic path for data files + Python

  FSettings.AppHome := RealHome;
  FSettings.SettingsHome := RealHome;
end;

procedure TSettings.Save;
var
  lSerializer: TJsonSerializer;
  JsonText: String;
begin
  if FSettings.SettingsHome.IsEmpty() then
    Exit;

  lSerializer := TJsonSerializer.Create;
  lSerializer.MaxDepth := 2;
  try
    try
      JsonText := lSerializer.Serialize<TSettingsRec>(FSettings);
      try
        TFile.WriteAllText(IncludeTrailingPathDelimiter(FSettings.SettingsHome) + 'Settings.json', JsonText);
      except
         on E : Exception do
           Raise Exception.Create('SaveSystemSettings - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
      end;
    except
     on E : Exception do
     begin
       Raise Exception.Create('SaveSystemSettings - Exception : Class = ' +
        E.ClassName + ', Message = ' + E.Message);
     end;
    end;
  finally
    FreeAndNil(lSerializer);
  end;
end;

procedure TSettings.SetGenOpt(Index: Cardinal; AValue: Boolean);
begin
  FSettings.GenOpt[Index] := AValue;
end;

procedure TSettings.SetLastUsedModel(AValue: String);
begin
  if AValue <> '' then
    FSettings.LastUsedModel := AValue;
end;

procedure TSettings.SetModelDirectory(AValue: String);
begin
  if AValue <> '' then
    FSettings.ModelDirectory := AValue;
end;

end.
