program LibrivoxGrab;

uses
  System.StartUpCopy,
  FMX.Forms,
  LibrivoxMain in 'LibrivoxMain.pas' {GrabForm},
  CEDownloader in '..\..\..\src\CEDownloader.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TGrabForm, GrabForm);
  Application.Run;
end.
