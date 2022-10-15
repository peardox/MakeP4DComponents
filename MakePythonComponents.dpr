program MakePythonComponents;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {ZipExtractForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TZipExtractForm, ZipExtractForm);
  Application.Run;
end.
