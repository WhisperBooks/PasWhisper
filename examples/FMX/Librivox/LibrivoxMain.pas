unit LibrivoxMain;

interface

Uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Diagnostics,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  CEDownloader;

type
  TGrabForm = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Layout3: TLayout;
    ProgressBar1: TProgressBar;
    procedure OnDownloadComplete(Sender: TObject);
    procedure OnAsynchData(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ErrorPop(Sender: TObject; const Code: Integer; const Msg: String);
  private
    { Private declarations }
    RJSON: TArchiveOrg;
    RModel: THuggingFace;
  public
    { Public declarations }
  end;

var
  GrabForm: TGrabForm;

implementation

{$R *.fmx}

{ TGrabForm }

procedure TGrabForm.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.WordWrap := True;
  ProgressBar1.Value := 0;

  RJSON := TArchiveOrg.Create(Self);
  RJSON.NumItems := 100;
  RJSON.OnAsynchCompltete := OnDownloadComplete;
  RJSON.OnAsynchData := OnAsynchData;
  RJSON.Grab;
end;

procedure TGrabForm.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.WordWrap := True;
  ProgressBar1.Value := 0;

  RModel := THuggingFace.Create(Self);
  RModel.Model := 'medium.en';
  RModel.ModelType := GGML; // OpenVino;
  RModel.OnAsynchCompltete := OnDownloadComplete;
  RModel.OnAsynchData := OnAsynchData;
  RModel.OnError := ErrorPop;
  RModel.Grab;
end;

procedure TGrabForm.OnAsynchData(Sender: TObject);
begin
  if not(Sender is TRemoteData) then
    Exit;
  if TRemoteData(Sender).Progress.ReadBytes >= TRemoteData(Sender).Progress.TotalBytes then
    ProgressBar1.Value := 0
  else
    ProgressBar1.Value := (TRemoteData(Sender).Progress.ReadBytes / TRemoteData(Sender).Progress.TotalBytes) * ProgressBar1.Max;

end;

procedure TGrabForm.ErrorPop(Sender: TObject; const Code: Integer;
  const Msg: String);
begin
  ShowMessage(Format('ErrorPop : %d - %s', [Code, Msg]));
end;

procedure TGrabForm.OnDownloadComplete(Sender: TObject);
var
  S: TStream;
  Elapsed: Int64;
  Secs, Bps: Single;
begin
  if not(Sender is TRemoteData) then
    Exit;
  if TRemoteData(Sender).Invalid then
    Exit;
  Elapsed := TRemoteData(Sender).GetTime;
  Secs :=  (Elapsed / 1000);
  Bps := (TRemoteData(Sender).ContentLength / (1024 * 1024)) / Secs;

  S := Nil;
  Memo1.Lines.Add(Format('Date : Type = %s, Length = %d, Raw = %d, Time = %f, MBPS = %f',[TRemoteData(Sender).ContentType, TRemoteData(Sender).ContentLength, Length(TRemoteData(Sender).RawBytes), Secs, Bps]));
  try
    S := TFileStream.Create(TRemoteData(Sender).FileName, fmCreate);
    S.Position := 0;
    S.Write(TRemoteData(Sender).RawBytes, Length(TRemoteData(Sender).RawBytes));
  finally
    if S <> Nil then
      S.Free;
    if Assigned(TRemoteData(Sender).OnAfterDownload) then
      TRemoteData(Sender).OnAfterDownload(Self);
    FreeAndNil(Sender);
  end;
end;

end.
