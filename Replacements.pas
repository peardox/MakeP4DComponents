unit Replacements;

interface

uses
  System.SysUtils, Settings;

procedure ReplaceTokens(var AStr: String; const AToken: String; const Component: TComponentSettings = Nil);
function RenameTemplateFile(const ATeokenisedFilename: String; const Component: TComponentSettings = Nil): String;

implementation

var
  FileNameReplacements: TArray<String> = [
    '__DCL__',
    '__COMPONENT__',
    '__P4D__',
    '__PROJECT_GROUP_NAME__'
  ];

function GetRandomGUID(const PackageID: String; const IsPaletteGUID: Boolean): String;
var
  newGUID: TGUID;
begin
  CreateGUID(newGUID);
  Result := TGUID.NewGuid.ToString;
end;

function ReplaceToken(const Token: String; const Component: TComponentSettings = Nil): String;
begin
  Result := String.Empty;

  if (Component <> Nil) then
    begin
      if Token = '__COMPONENT__' then
        Result := Component.DelphiPackageName
      else if Token = '__PUBLIC_PROPERTY__' then
        Result := Component.PublicPackageName
      else if Token = '__PACKAGE_NAME__' then
        Result := Component.PythonPackageName
      else if Token = '__PACKAGE_NAME_PIP__' then
        Result := Component.PIPPackageName
      else if Token = '__PACKAGE_NAME_CONDA__' then
        Result := Component.CondaPackageName
      else if Token = '__HOME_URL__' then
        Result := Component.HomeURL
      else if Token = '__PYPI_URL__' then
        Result := Component.PyPiURL
      else if Token = '__DOC_URL__' then
        Result := Component.DocURL
      else if Token = '__GITHUB_URL__' then
        Result := Component.GithubURL
      else if Token = '__PALETTE_GUID__' then
        Result := GetRandomGUID(Component.DelphiPackageName, True)
      else if Token = '__PACKAGE_GUID__' then
        Result := GetRandomGUID(Component.DelphiPackageName, False);
    end;

  if Result = String.Empty then
    begin
    if Token = '__DCL__' then
      Result := 'dcl'
    else if Token = '__P4D__' then
      Result := 'p4d'
    else if Token = '__PALETTE_TITLE__' then
      Result := ProjectSettings.ProjectTitle
    else if Token = '__PROJECT_VERSION__' then
      Result := ProjectSettings.ProjectVersion
    else if Token = '__README__' then
      Result := ProjectSettings.ReadMe
    else if Token = '__PROJECT_GROUP_NAME__' then
      Result := ProjectSettings.ProjectGroupName
    else if Token = '__PROJECT_DESCRIPTION__' then
      Result := ProjectSettings.ProjectDesc
    else if Token = '__PROJECT_HOMEPAGE__' then
      Result := ProjectSettings.ProjectHomepage;
    end;

end;

procedure ReplaceTokens(var AStr: String; const AToken: String; const Component: TComponentSettings = Nil);
begin
  AStr.Replace(AToken, ReplaceToken(AToken, Component), [rfReplaceAll]);
end;

function RenameTemplateFile(const ATeokenisedFilename: String; const Component: TComponentSettings = Nil): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(FileNameReplacements) - 1 do
    ReplaceTokens(Result, FileNameReplacements[I], Component);
end;


end.
