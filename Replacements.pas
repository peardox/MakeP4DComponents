unit Replacements;

interface

uses
  System.SysUtils, Settings;

resourcestring
  // __RUNTIME_PROJECTS__
  RuntimeProjects = '        <Projects Include="src\__COMPONENT__\__P4D____COMPONENT__.dproj">' + sLineBreak +
                    '            <Dependencies/>' + sLineBreak +
                    '        </Projects>' + sLineBreak;

  // __COMPONENT_PROJECTS__
  ComponentProjects = '        <Projects Include="src\__COMPONENT__\__DCL____P4D____COMPONENT__.dproj">' + sLineBreak +
                      '            <Dependencies/>' + sLineBreak +
                      '        </Projects>' + sLineBreak;

  //__RUNTIME_TARGETS__
  RuntimeTargets =  '    <Target Name="__P4D____COMPONENT__">' + sLineBreak +
                    '        <MSBuild Projects="src\__COMPONENT__\__P4D____COMPONENT__.dproj"/>' + sLineBreak +
                    '    </Target>' + sLineBreak +
                    '    <Target Name="__P4D____COMPONENT__:Clean">' + sLineBreak +
                    '        <MSBuild Projects="src\__COMPONENT__\__P4D____COMPONENT__.dproj" Targets="Clean"/>' + sLineBreak +
                    '    </Target>' + sLineBreak +
                    '    <Target Name="__P4D____COMPONENT__:Make">' + sLineBreak +
                    '        <MSBuild Projects="src\__COMPONENT__\__P4D____COMPONENT__.dproj" Targets="Make"/>' + sLineBreak +
                    '    </Target>' + sLineBreak;

  //__COMPONENT_TARGETS__
  ComponentTargets =  '    <Target Name="__DCL____P4D____COMPONENT__">' + sLineBreak +
                      '        <MSBuild Projects="src\__COMPONENT__\__DCL____P4D____COMPONENT__.dproj"/>' + sLineBreak +
                      '    </Target>' + sLineBreak +
                      '    <Target Name="__DCL____P4D____COMPONENT__:Clean">' + sLineBreak +
                      '        <MSBuild Projects="src\__COMPONENT__\__DCL____P4D____COMPONENT__.dproj" Targets="Clean"/>' + sLineBreak +
                      '    </Target>' + sLineBreak +
                      '    <Target Name="__DCL____P4D____COMPONENT__:Make">' + sLineBreak +
                      '        <MSBuild Projects="src\__COMPONENT__\__DCL____P4D____COMPONENT__.dproj" Targets="Make"/>' + sLineBreak +
                      '    </Target>' + sLineBreak;

  //__BUILD_COMPONENT_LIST__
  BuildP4DComponentList = '__P4D____COMPONENT__';
  BuildDCLComponentList = '__DCL____P4D____COMPONENT__';

  //__CLEAN_COMPONENT_LIST__
  CleanP4DComponentList = '__P4D____COMPONENT__:Clean';
  CleanDCLComponentList = '__DCL____P4D____COMPONENT__:Clean';

  // __MAKE_COMPONENT_LIST__
  MakeP4DComponentList = '__P4D____COMPONENT__:Make';
  MakeDCLComponentList = '__DCL____P4D____COMPONENT__:Make';

const
  FileNameReplacements: TArray<String> = [
    '__DCL__',
    '__COMPONENT__',
    '__P4D__',
    '__PROJECT_GROUP_NAME__'
  ];

  ConstructedReplacements: TArray<String> = [
    '__DCL__',
    '__COMPONENT__',
    '__P4D__'
  ];

procedure ReplaceTokens(var AStr: String; const AToken: String; const Component: TComponentSettings = Nil);
procedure RenameTemplateFile(var ATokenisedFilename: String; const Component: TComponentSettings = Nil);

implementation

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
      Result := 'P4D'
    else if Token = '__PALETTE_PAGE__' then
      Result := ProjectSettings.PalettePage
    else if Token = '__PROJECT_VERSION__' then
      Result := ProjectSettings.ProjectVersion
    else if Token = '__PROJECT_TITLE__' then
      Result := ProjectSettings.ProjectTitle
    else if Token = '__README__' then
      Result := ProjectSettings.ReadMe
    else if Token = '__PROJECT_GROUP_NAME__' then
      Result := ProjectSettings.ProjectGroupName
    else if Token = '__PROJECT_DESCRIPTION__' then
      Result := ProjectSettings.ProjectDesc
    else if Token = '__GROUPPROJ_GUID__' then
      Result := GetRandomGUID(ProjectSettings.ProjectGroupName, False)
    else if Token = '__PROJECT_HOMEPAGE__' then
      Result := ProjectSettings.ProjectHomepage;
    end;

end;

procedure ReplaceTokens(var AStr: String; const AToken: String; const Component: TComponentSettings = Nil);
begin
  AStr := AStr.Replace(AToken, ReplaceToken(AToken, Component), [rfReplaceAll]);
end;

procedure RenameTemplateFile(var ATokenisedFilename: String; const Component: TComponentSettings = Nil);
var
  I: Integer;
begin
  for I := 0 to Length(FileNameReplacements) - 1 do
    ReplaceTokens(ATokenisedFilename, FileNameReplacements[I], Component);
end;


end.
