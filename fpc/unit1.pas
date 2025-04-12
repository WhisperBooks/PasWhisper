unit Unit1;

{$mode objfpc}{$H+}
{$I platform.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  caption := 'UNSET';
  {$IF DEFINED(OS_LINUX64)}
  caption := 'LINUX64';
  {$ELSEIF DEFINED(OS_LINUX64ARM)}
  caption := 'LINUX64ARM';
  {$ELSEIF DEFINED(OS_WIN64)}
  caption := 'WIN64';
  {$ELSEIF DEFINED(OS_OSXARM64)}
  caption := 'OSX64ARM';
  {$ELSEIF DEFINED(OS_OSX64)}
  caption := 'OSX64';
  {$ELSE}
  caption := 'UNKNOWN';
  {$ENDIF}

end;

end.

