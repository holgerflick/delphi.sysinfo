program SysInfoDemo;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormMain},
  SysInfo in '..\SysInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
