program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1} ,
  SG.ScriptGate in 'ScriptGate\SG.ScriptGate.pas',
  SG.ScriptGate.Win in 'ScriptGate\SG.ScriptGate.Win.pas',
  SG.ScriptGateLog in 'ScriptGate\SG.ScriptGateLog.pas',
  SG.WebBrowserHelper in 'ScriptGate\SG.WebBrowserHelper.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutDown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
