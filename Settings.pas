unit Settings;

interface

uses
  System.Classes,
  FMX.Dialogs,
  FMX.Graphics;

type
  TComponentSettings = Class(TObject)
    DelphiPackageName: String;
    PublicPackageName: String;
    PythonPackageName: String;
    PIPPackageName: String;
    CondaPackageName: String;
    HomeURL: String;
    PyPiURL: String;
    DocURL: String;
    GithubURL: String;
    PackageIcon: String;
    constructor Create;
    procedure CopyFrom(AObject: TComponentSettings);
  end;
  TProjectComponentSettings = TArray<TComponentSettings>;

  TProjectSettings = Class(TObject)
    ProjectTitle: String;
    ProjectVersion: String;
    ReadMe: String;
    ProjectGroupName: String;
    ProjectDesc: String;
    ProjectHomepage: String;
    PalettePage: String;
    IncludePackageInfo: Boolean;
    ComponentSettings: TProjectComponentSettings;
    constructor Create;
    destructor Destroy; override;
  end;

var
  AppHome: String;
  ProjectSettings: TProjectSettings;
  LogStrings: TStringList;
  SkipWebsiteChecks: Boolean;

const
  DefaultProjectFile = 'WorkingProject.p4d';

procedure SaveProjectSettings(const AProjectFile: String);
procedure LoadProjectSettings(const AProjectFile: String);
procedure DecodeBase64Image(var ABitmap: TBitmap; Base64Image: String);
procedure Log(const AMsg: String);

implementation

uses
  System.IOUtils,
  System.NetEncoding,
  System.SysUtils,
  System.Json.Serializers;

procedure Log(const AMsg: String);
begin
  LogStrings.Add(AMsg);
end;

constructor TComponentSettings.Create;
begin
  Inherited Create;
end;

procedure TComponentSettings.CopyFrom(AObject: TComponentSettings);
  function FullCopy(const AStr: String): String;
  begin
    Result := Copy(AStr, 1, Length(AStr));
  end;
begin
  DelphiPackageName := FullCopy(AObject.DelphiPackageName);
  PublicPackageName := FullCopy(AObject.PublicPackageName);
  PythonPackageName := FullCopy(AObject.PythonPackageName);
  PIPPackageName := FullCopy(AObject.PIPPackageName);
  CondaPackageName := FullCopy(AObject.CondaPackageName);
  HomeURL := FullCopy(AObject.HomeURL);
  PyPiURL := FullCopy(AObject.PyPiURL);
  DocURL := FullCopy(AObject.DocURL);
  GithubURL := FullCopy(AObject.GithubURL);
  PackageIcon := FullCopy(AObject.PackageIcon);
end;

constructor TProjectSettings.Create;
begin
  Inherited Create;
end;

destructor TProjectSettings.Destroy;
var
  I: Integer;
  AComponent: TComponentSettings;
begin
  if Length(ComponentSettings) > 0 then
    begin
      for I := 0 to Length(ComponentSettings) - 1 do
        begin
          AComponent := ComponentSettings[I];
          FreeAndNil(AComponent);
        end;
      SetLength(ComponentSettings, 0);
    end;

  Inherited Destroy;
end;

procedure LoadProjectSettings(const AProjectFile: String);
var
  lSerializer: TJsonSerializer;
  JsonText: String;
begin
  if Assigned(ProjectSettings) then
    FreeAndNil(ProjectSettings);

  if FileExists(AProjectFile) then
    begin
      lSerializer := TJsonSerializer.Create;
      try

        JsonTExt := TFile.ReadAllText(AProjectFile);
        try
          ProjectSettings := lSerializer.Deserialize<TProjectSettings>(JsonText);
        except
         on E : Exception do
           Raise Exception.Create('LoadSystemSettings - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
        end;
      finally
        FreeAndNil(lSerializer);
      end;
    end
  else
    ProjectSettings := TProjectSettings.Create;

end;

procedure SaveProjectSettings(const AProjectFile: String);
var
  JSONText: String;
  lSerializer: TJsonSerializer;
begin
  if Assigned(ProjectSettings) then
    begin
      try
        lSerializer := TJsonSerializer.Create;

        try
          JSONText := lSerializer.Serialize<TProjectSettings>(ProjectSettings);
          try
            TFile.WriteAllText(AProjectFile, JsonText);
          except
             on E : Exception do
               Raise Exception.Create('SaveSystemSettings - Exception : Class = ' +
                E.ClassName + ', Message = ' + E.Message);
          end;
        except
         on E : Exception do
         begin
           Raise Exception.Create('SaveSystemSettings - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
         end;
        end;
      finally
        FreeAndNil(lSerializer);
      end;
    end;
end;

procedure DecodeBase64Image(var ABitmap: TBitmap; Base64Image: String);
var
  MStream: TMemoryStream;
  SStream: TStringStream;
begin
  MStream := TMemoryStream.Create;
  SStream := TStringStream.Create(Base64Image);
  try
    try
      SStream.Position := 0;
      TNetEncoding.Base64.Decode(SStream, MStream);
      MStream.Position := 0;
      ABitmap.LoadFromStream(MStream);
      except
       on E : Exception do
       begin
         Raise Exception.Create('SaveSystemSettings - Exception : Class = ' +
          E.ClassName + ', Message = ' + E.Message);
       end;
    end;
  finally
    SStream.Free;
    MStream.Free;
  end;
end;

end.
