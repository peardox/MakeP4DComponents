program MakePythonComponents;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {MainForm},
  Unit2 in 'Unit2.pas' {Form2};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
