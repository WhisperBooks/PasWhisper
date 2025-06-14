unit LibrivoxMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  REST.Client, REST.Types,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Layouts, Data.Bind.Components,
  Data.Bind.ObjectScope;

type
  TNotifyMessageEvent = procedure(Sender: TObject; const Msg: String);

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
    FIsAsynch: Boolean;
    FOnAsynchCompltete: TNotifyEvent;
    FOnAsynchData: TNotifyEvent;
    FProgress: TDownloadProgress;
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
  protected
    FAsynchThread: TRESTExecutionThread;
    FRESTParams: TRESTRequestParameterList;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Grab: Boolean; virtual;
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
    FFile: String;
    FModelType: TModelType;
  public
    function Grab: Boolean; override;
    property NumItems: Integer read FNumItems write FNumItems;
    property Model: String read FModel write FModel;
    property FileName: String read FFile;
    property ModelType: TModelType read FModelType write FModelType;
  end;

  TGrabForm = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    procedure OnGrabComplete(Sender: TObject);
    procedure OnModelComplete(Sender: TObject);
    procedure DoAsynchData(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure RESTClient1ReceiveData(const Sender: TObject; AContentLength,
      AReadCount: Int64; var AAbort: Boolean);
    procedure FormCreate(Sender: TObject);
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
  RJSON.OnAsynchCompltete := OnGrabComplete;
  RJSON.Grab;
end;

procedure TRemoteData.AynchComplete;
begin
  if Assigned(FOnAsynchCompltete) then
    FOnAsynchCompltete(Self);
end;

procedure TRemoteData.AynchCompleteWithErrors(O: TObject);
var
  E: Exception;
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
  FRESTParams := TRESTRequestParameterList.Create(Nil);
  FRESTClient := TRESTClient.Create(AOwner);
  FRESTRequest := TRESTRequest.Create(Nil);
  FRESTResponse := TRESTResponse.Create(Nil);

  FRestClient.OnReceiveData := ClientReceiveData;
  FRestClient.OnHTTPProtocolError := ClientHTTPProtocolError;

  FRESTRequest.Client := FRestClient;
  FRESTRequest.Response := FRESTResponse;

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
        raise Exception.Create('GrabJSON : Exception : Class = ' + E.ClassName + ', Message = ' + E.Message);
      end;
  finally
//    AUrl := RestRequest.GetFullRequestURL(True);
  end;

  Result := True;
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
  Memo1.WordWrap := True;
  RModel := THuggingFace.Create(Self);
//  RModel.Model := 'base.en';
  RModel.Model := 'base';
  RModel.ModelType := GGML;
  RModel.IsAsynch := True;
  RModel.OnAsynchCompltete := OnModelComplete;
  RModel.OnAsynchData := DoAsynchData;
  RModel.Grab;
end;

procedure TGrabForm.DoAsynchData(Sender: TObject);
begin
  if Sender = RModel then
    Memo1.Lines.Add(Format('Read #%d chunks so far : %d of %d',[RModel.Progress.ReadCount, RModel.Progress.ReadBytes, RModel.Progress.TotalBytes]));
end;

procedure TGrabForm.FormCreate(Sender: TObject);
begin
//  OnAsynchData := DoAsynchData;
end;

procedure TGrabForm.OnGrabComplete(Sender: TObject);
begin
  // application/json
  Memo1.Lines.Add(Format('Date : Type = %s, Length = %d',[RJSON.ContentType, RJSON.ContentLength]));
  Memo1.Lines.Add(RJSON.Content);
end;

procedure TGrabForm.OnModelComplete(Sender: TObject);
var
  S: TStream;
begin
  // application/octet-stream
//  if RModel.ContentType = 'application/octetstream' then
  Memo1.Lines.Add(Format('Date : Type = %s, Length = %d, Raw = %d',[RModel.ContentType, RModel.ContentLength, Length(RModel.RawBytes)]));

  S := TFileStream.Create(RModel.FileName, fmCreate);
  S.Position := 0;
  S.Write(Rmodel.RawBytes, Length(RModel.RawBytes));
  S.Free;

end;

procedure TGrabForm.RESTClient1ReceiveData(const Sender: TObject;
  AContentLength, AReadCount: Int64; var AAbort: Boolean);
begin

end;

{ THuggingFace }

function THuggingFace.Grab: Boolean;
begin
  if FModel = '' then
    Exit(False);

  case FModelType of
    GGML: FFile := 'ggml-' + FModel + '.bin';
    OpenVino: FFile := 'ggml-' + FModel + '-encoder-openvino.bin';
    CoreML: FFile := 'ggml-' + FModel + '-encoder.mlmodelc.zip';
  else
    Exit(False);
  end;

  URL := 'https://huggingface.co';
  Resource := 'ggerganov/whisper.cpp/resolve/main/' + FFile;
  { https://huggingface.co/base-encoder.mlmodelc.zip?download=true }
  // https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-base.bin?download=true
  FRestParams.AddItem('download','true');
  Result := inherited;
end;

end.
