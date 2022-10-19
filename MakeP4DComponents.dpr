program MakeP4DComponents;

uses
  System.StartUpCopy,
  FMX.Forms,
  ProjectUnit in 'ProjectUnit.pas' {MainForm},
  ComponentUnit in 'ComponentUnit.pas' {ComponentForm},
  Settings in 'Settings.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TComponentForm, ComponentForm);
  Application.Run;
end.
