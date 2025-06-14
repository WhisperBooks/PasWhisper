program LibrivoxGrab;

uses
  System.StartUpCopy,
  FMX.Forms,
  LibrivoxMain in 'LibrivoxMain.pas' {GrabForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGrabForm, GrabForm);
  Application.Run;
end.
