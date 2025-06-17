unit LibrivoxMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Diagnostics,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  REST.Client, REST.Types,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Layouts;

type
  TNotifyMessageEvent = procedure(Sender: TObject; const Code: Integer; const Msg: String) of Object;

  TDownloadProgress = record
    ReadCount: Int64;
    ReadBytes: Int64;
    TotalBytes: Int64;
  end;

  TRemoteData = class
  strict private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FPrecisionTimer: TStopWatch;
    FFile: String;
    FAquireTime: Int64;
    FIsAsynch: Boolean;
    FOnAsynchCompltete: TNotifyEvent;
    FOnAsynchData: TNotifyEvent;
    FOnError: TNotifyMessageEvent;
    FProgress: TDownloadProgress;
    FInvalid: Boolean;
    FAbort: Boolean;
    procedure SetURL(const AValue: String);
    procedure SetResource(const AValue: String);
    function GetContent: String;
    function GetContentType: String;
    function GetContentLength: Integer;
    function GetRawBytes: TBytes;
    procedure AynchComplete;
    procedure AynchCompleteWithErrors(O: TObject);
    procedure ClientReceiveData(const Sender: TObject; AContentLength,
      AReadCount: Int64; var AAbort: Boolean);
    procedure ClientHTTPProtocolError(Sender: TCustomRESTClient);
    procedure RequestHTTPProtocolError(Sender: TCustomRESTRequest);
  protected
    FAsynchThread: TRESTExecutionThread;
    FRESTParams: TRESTRequestParameterList;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Grab: Boolean; virtual;
    function GetTime: Int64;
    property URL: String write SetURL;
    property Resource: String write SetResource;
    property Content: String read GetContent;
    property ContentType: String read GetContentType;
    property ContentLength: Integer read GetContentLength;
    property RawBytes: TBytes read GetRawBytes;
    property IsAsynch: Boolean read FIsAsynch write FIsAsynch;
    property OnAsynchCompltete: TNotifyEvent read FOnAsynchCompltete write FOnAsynchCompltete;
    property OnAsynchData: TNotifyEvent read FOnAsynchData write FOnAsynchData;
    property Progress: TDownloadProgress read FProgress write FProgress;
    property OnError: TNotifyMessageEvent read FOnError write FOnError;
    property Abort: Boolean read FAbort write FAbort;
    property Invalid: Boolean read FInvalid;
    property AquireTime: Int64 read FAquireTime;
    property FileName: String read FFile write FFile;
  end;

  TArchiveOrg = class(TRemoteData)
  strict private
    FNumItems: Integer;
  public
    function Grab: Boolean; override;
    property NumItems: Integer read FNumItems write FNumItems;
  end;

  TModelType = (GGML, OpenVino, CoreML);

  THuggingFace = class(TRemoteData)
  strict private
    FNumItems: Integer;
    FModel: String;
    FModelType: TModelType;
  public
    function Grab: Boolean; override;
    property NumItems: Integer read FNumItems write FNumItems;
    property Model: String read FModel write FModel;
    property ModelType: TModelType read FModelType write FModelType;
  end;

  TGrabForm = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Layout3: TLayout;
    ProgressBar1: TProgressBar;
    procedure OnDownloadComplete(Sender: TObject);
    procedure DoAsynchData(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  RJSON := TArchiveOrg.Create(Self);
  RJSON.NumItems := 10;

  RJSON.IsAsynch := True;
  RJSON.OnAsynchCompltete := OnDownloadComplete;
  RJSON.Grab;
end;

procedure TRemoteData.AynchComplete;
begin
  if Assigned(FOnAsynchCompltete) then
    FOnAsynchCompltete(Self);
end;

procedure TRemoteData.AynchCompleteWithErrors(O: TObject);
begin
  raise Exception.Create('GrabJSON : Exception : Class = ' + O.ClassName);
end;

procedure TRemoteData.ClientReceiveData(const Sender: TObject;
  AContentLength, AReadCount: Int64; var AAbort: Boolean);
begin
  FProgress.ReadCount := FProgress.ReadCount + 1;
  FProgress.ReadBytes := AReadCount;
  FProgress.TotalBytes := AContentLength;
  if Assigned(FOnAsynchData) then
    FOnAsynchData(Self);
end;

procedure TRemoteData.ClientHTTPProtocolError(Sender: TCustomRESTClient);
begin
  raise Exception.Create('GrabJSON : Exception : Class = ');
end;


constructor TRemoteData.Create(AOwner: TComponent);
begin
  FIsAsynch := True;

  FPrecisionTimer := TStopWatch.StartNew;

  FRESTParams := TRESTRequestParameterList.Create(Nil);
  FRESTClient := TRESTClient.Create(AOwner);
  FRESTRequest := TRESTRequest.Create(FRESTClient);
  FRESTResponse := TRESTResponse.Create(FRESTClient);

  FRestClient.OnReceiveData := ClientReceiveData;
  FRestClient.OnHTTPProtocolError := ClientHTTPProtocolError;
  FRestClient.RaiseExceptionOn500 := True;
  FRestClient.SynchronizedEvents := True;

  FRESTRequest.Client := FRestClient;
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.OnHTTPProtocolError := RequestHTTPProtocolError;
  FRESTRequest.SynchronizedEvents := True;

  FProgress := Default(TDownloadProgress);
end;

destructor TRemoteData.Destroy;
begin
  FRESTRequest.Free;
  FRESTResponse.Free;
  FRESTParams.Free;
  FRESTClient.Free;
  inherited;
end;

function TArchiveOrg.Grab: Boolean;
begin
  URL := 'https://archive.org';
  Resource := 'advancedsearch.php';
  Filename := 'librivox.json';

  FRestParams.AddItem('q','collection:"librivoxaudio"');
  FRestParams.AddItem('fl','avg_rating;creator;date;description;downloads;format;genre;identifier;item_size;language;licenseurl;mediatype;name;noindex;num_reviews;oai_updatedate;publicdate;subject;title', pkGETorPOST, [poPHPArray]);
  FRestParams.AddItem('rows',IntToStr(NumItems));
  FRestParams.AddItem('page','1');
  FRestParams.AddItem('output','json');
  Result := inherited;
end;

function TRemoteData.GetContent: String;
begin
  Result := FRestResponse.Content;
end;

function TRemoteData.GetContentLength: Integer;
begin
  Result := FRestResponse.ContentLength;
end;

function TRemoteData.GetContentType: String;
begin
  Result := FRestResponse.ContentType;
end;

function TRemoteData.GetRawBytes: TBytes;
begin
  Result := FRestResponse.RawBytes;
end;

function TRemoteData.GetTime: Int64;
begin
  FPrecisionTimer.Stop;
  FAquireTime := FPrecisionTimer.ElapsedMilliseconds;
  Result := FAquireTime;
end;

function TRemoteData.Grab: Boolean;
begin
  try
    FRESTRequest.Method := rmGet;
    FRESTRequest.Params := FRESTParams;
    FRESTRequest.AcceptEncoding := 'gzip, deflate';
    try
      if FIsAsynch then
        FAsynchThread := FRESTRequest.ExecuteAsync(AynchComplete, True, True, AynchCompleteWithErrors)
      else
        begin
          FRESTRequest.Execute;
          case FRestResponse.StatusCode of
            200 :
              begin
              end;
            404 :
              begin
              end;
            else
              raise Exception.Create(IntToStr(FRestResponse.StatusCode) + ' : ' + FRestClient.BaseURL);
          end;
      end;
    except
      on E : Exception do
        raise Exception.Create('Grab : ' + E.ClassName + ', Message = ' + E.Message);
      end;
  finally
//    AUrl := RestRequest.GetFullRequestURL(True);
  end;

  Result := True;
end;

procedure TRemoteData.RequestHTTPProtocolError(Sender: TCustomRESTRequest);
begin
  if Assigned(OnError) then
    begin
      FInvalid := True;
      FOnError(Self, Sender.Response.StatusCode, Sender.Response.StatusText);
    end;
end;

procedure TRemoteData.SetResource(const AValue: String);
begin
  FRestRequest.Resource := AValue;
end;

procedure TRemoteData.SetURL(const AValue: String);
begin
  FRestClient.BaseURL := AValue;
end;

procedure TGrabForm.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  ProgressBar1.Value := 0;
  Memo1.WordWrap := True;
  RModel := THuggingFace.Create(Self);
//  RModel.Model := 'non-existant-test';
  RModel.Model := 'medium.en';
  RModel.ModelType := OpenVino; // GGML;
  RModel.OnAsynchCompltete := OnDownloadComplete;
  RModel.OnAsynchData := DoAsynchData;
  RModel.OnError := ErrorPop;
  RModel.Grab;
end;

procedure TGrabForm.DoAsynchData(Sender: TObject);
begin
  if Sender = RModel then
    begin
//    Memo1.Lines.Add(Format('Read #%d chunks so far : %d of %d',[RModel.Progress.ReadCount, RModel.Progress.ReadBytes, RModel.Progress.TotalBytes]));
      ProgressBar1.Value := (RModel.Progress.ReadBytes / RModel.Progress.TotalBytes) * ProgressBar1.Max;
    end;
end;

procedure TGrabForm.ErrorPop(Sender: TObject; const Code: Integer;
  const Msg: String);
begin
  ShowMessage(Format('ErrorPop : %d - %s', [Code, Msg]));
end;

procedure TGrabForm.FormCreate(Sender: TObject);
begin
//  OnAsynchData := DoAsynchData;
end;

procedure TGrabForm.OnDownloadComplete(Sender: TObject);
var
  S: TStream;
  Elapsed: Int64;
  Secs, Bps: Single;
begin
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
  end;

end;

{ THuggingFace }

function THuggingFace.Grab: Boolean;
var
  Path: String;
begin
  if FModel = '' then
    Exit(False);

  case FModelType of
    GGML:
      begin
        Path := 'ggerganov/whisper.cpp/resolve/main/';
        FileName := 'ggml-' + FModel + '.bin';
      end;
    OpenVino:
      begin
        // ggml-medium-encoder-openvino.zip?download=true
        Path := 'Whisper-Pascal/whisper-openvino/resolve/main/';
        FileName := 'ggml-' + FModel + '-encoder-openvino.zip';
      end;
    CoreML:
      begin
        Path := 'ggerganov/whisper.cpp/resolve/main/';
        FileName := 'ggml-' + FModel + '-encoder.mlmodelc.zip';
      end
  else
    Exit(False);
  end;

  URL := 'https://huggingface.co';
  Resource := Path + FileName;
  { https://huggingface.co/base-encoder.mlmodelc.zip?download=true }
  // https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-base.bin?download=true
  FRestParams.AddItem('download','true');
  Result := inherited;
end;

end.
