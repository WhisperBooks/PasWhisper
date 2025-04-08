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

  WriteLnLog('TWhisperModel                : %d',[SizeOf(TWhisperModel)]);
  WriteLnLog('TWhisperVocab                : %d',[SizeOf(TWhisperVocab)]);
  WriteLnLog('TWhisperContextParams        : %d (%p)',[SizeOf(TWhisperContextParams), @cparams]);
  WriteLnLog('SizeOf use_gpu               : %d (%p)',[SizeOf(cparams.use_gpu), @cparams.use_gpu]);
  WriteLnLog('SizeOf flash_attn            : %d (%p)',[SizeOf(cparams.flash_attn), @cparams.flash_attn]);
  WriteLnLog('SizeOf gpu_device            : %d (%p)', [sizeof(cparams.gpu_device), @cparams.gpu_device]);
  WriteLnLog('SizeOf dtw_token_timestamps  : %d (%p)', [sizeof(cparams.dtw_token_timestamps), @cparams.dtw_token_timestamps]);
  WriteLnLog('SizeOf dtw_aheads_preset     : %d (%p)', [sizeof(cparams.dtw_aheads_preset), @cparams.dtw_aheads_preset]);
  WriteLnLog('SizeOf dtw_n_top             : %d (%p)', [sizeof(cparams.dtw_n_top), @cparams.dtw_n_top]);
  WriteLnLog('SizeOf dtw_aheads            : %d (%p)', [sizeof(cparams.dtw_aheads), @cparams.dtw_aheads]);
  WriteLnLog('  SizeOf dtw_aheads.n_heads    : %d (%p)', [sizeof(cparams.dtw_aheads.nHeads), @cparams.dtw_aheads.nHeads]);
  WriteLnLog('  SizeOf dtw_aheads.heads      : %d (%p)',[ sizeof(cparams.dtw_aheads.Heads), @cparams.dtw_aheads.Heads]);
  WriteLnLog('SizeOf dtw_mem_size          : %d (%p)', [sizeof(cparams.dtw_mem_size), @cparams.dtw_mem_size]);
  {$O-}
  zz := FWhisper.Test('C:\models\ggml-base.en.bin');
  writelnlog('%d,%d,%d,%d',[zz.hparams.n_vocab, zz.hparams.n_audio_ctx, zz.hparams.n_audio_state, zz.hparams.n_audio_head]);
  {$O+}
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
