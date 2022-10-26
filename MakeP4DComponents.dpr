program MakeP4DComponents;

uses
  System.StartUpCopy,
  FMX.Forms,
  ProjectUnit in 'ProjectUnit.pas' {MainForm},
  ComponentUnit in 'ComponentUnit.pas' {ComponentForm},
  Settings in 'Settings.pas',
  OSBrowser in 'OSBrowser.pas',
  Replacements in 'Replacements.pas',
  ComponentExporter in 'ComponentExporter.pas',
  ConfirmForm in 'ConfirmForm.pas' {frmConfirm},
  MessageForm in 'MessageForm.pas' {frmMessage};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TComponentForm, ComponentForm);
  Application.CreateForm(TfrmConfirm, frmConfirm);
  Application.CreateForm(TfrmMessage, frmMessage);
  Application.Run;
end.
