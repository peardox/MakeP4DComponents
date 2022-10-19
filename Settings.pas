unit Settings;

interface

type
  TComponentSettings = Class
    DelphiPackageName: String;
    PublicPackageName: String;
    PythonPackageName: String;
    PIPPackageName: String;
    CondaPackageName: String;
    DocURL: String;
    PyPiURL: String;
    GithubURL: String;
    PackageIcon: String;
    constructor Create;
  end;
  TProjectComponentSettings = TArray<TComponentSettings>;

  TProjectSettings = Class
    ProjectTitle: String;
    ProjectVersion: String;
    ReadMe: String;
    ProjectGroupName: String;
    ProjectDesc: String;
    ProjectHomepage: String;
    PalettePage: String;
    ComponentSettings: TProjectComponentSettings;
    constructor Create;
  end;

implementation

constructor TComponentSettings.Create;
begin
  Inherited
end;

constructor TProjectSettings.Create;
begin
  Inherited
end;

end.
