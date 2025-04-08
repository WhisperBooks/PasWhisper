unit mainform;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Whisper, CheapLog, GgmlExternal,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Layouts;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Memo1: TMemo;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FWhisper: TWhisper;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses WhisperTypes;

procedure TForm1.Button1Click(Sender: TObject);
var
  zz: TWhisperModel;
  cparams: TWhisperContextParams;
begin
  FWhisper := TWhisper.Create;
  cparams :=  default(TWhisperContextParams);

  zz := FWhisper.Test('C:\models\ggml-base.en.bin');

  WriteLnLog('TWhisperContext              : %d',[SizeOf(TWhisperContext)]);
  WriteLnLog('TWhisper NMels               : %d',[FWhisper.ModelNmels]);
  writelnlog('%d,%d,%d,%d',[zz.hparams.n_vocab, zz.hparams.n_audio_ctx, zz.hparams.n_audio_state, zz.hparams.n_audio_head]);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 // FreeAndNil(FWhisper);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OutLog := Memo1.Lines;
end;

end.
