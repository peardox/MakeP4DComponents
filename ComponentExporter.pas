unit ComponentExporter;

interface

uses
  System.Classes,
  System.Zip;

type
  TExporter = Class
  protected
    FFilePath: String;
    FFileIsOpen: Boolean;
    FAllowOverWrite: Boolean;
    FWipeBeforeExport: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open(const APath: String); virtual;
    procedure Close; virtual;
    procedure Export(var TemplateList: TList; var ReplacementList: TList);
    procedure WriteFile(const AFile: String; const AStr: String); virtual;
    procedure WriteBitmap(const AFile: String; const ABase64Bitmap: String); overload; virtual;
    procedure WriteBitmap(const AFile: String; const Size: Integer; const ABase64Bitmap: String); overload; virtual;
    procedure WriteBitmap(const AFile: String; const AWidth: Integer; const AHeight: Integer; const ABase64Bitmap: String); overload; virtual;
    property AllowOverWrite: Boolean Read FAllowOverWrite Write FAllowOverWrite;
    property WipeBeforeExport: Boolean Read FWipeBeforeExport Write FWipeBeforeExport;
  End;

  TFileExporter = Class(TExporter)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open(const APath: String); override;
    procedure WriteFile(const AFile: String; const AStr: String); override;
    procedure WriteBitmap(const AFile: String; const ABase64Bitmap: String); overload; override;
    procedure WriteBitmap(const AFile: String; const Size: Integer; const ABase64Bitmap: String); overload; override;
    procedure WriteBitmap(const AFile: String; const AWidth: Integer; const AHeight: Integer; const ABase64Bitmap: String); overload; override;
    procedure Close; override;
    function CheckPath(const AFileName: String): String;
  End;

  TZipExporter = Class(TExporter)
  private
    FZipFile: TZipFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open(const APath: String); override;
    procedure WriteFile(const AFile: String; const AStr: String); override;
    procedure WriteBitmap(const AFile: String; const ABase64Bitmap: String); overload; override;
    procedure WriteBitmap(const AFile: String; const Size: Integer; const ABase64Bitmap: String); overload; override;
    procedure WriteBitmap(const AFile: String; const AWidth: Integer; const AHeight: Integer; const ABase64Bitmap: String); overload; override;
    procedure Close; override;
  End;

implementation

uses
  FMX.Graphics,
  FMX.Surfaces,
  FMX.Types,
  Replacements,
  Settings,
  System.IOUtils,
  System.SysUtils;

procedure TExporter.Export(var TemplateList: TList; var ReplacementList: TList);
var
  Template: TTemplateFile;
  Tokens: TReplacementToken;
  IDX: Integer;
  Comp: Integer;
  Rep: Integer;
  ReplacedText: String;
  ReplacedFileName: String;
  CompNames: Array of String;
  GroupProjIDX: Integer;
  ReadMeIDX: Integer;
  ConstructedRuntimeProjects: String;
  ConstructedComponentProjects: String;
  ConstructedRuntimeTargets: String;
  ConstructedComponentTargets: String;
  ConstructedBuildList: String;
  ConstructedCleanList: String;
  ConstructedMakeList: String;
  ConstructedReadMe: String;
begin
  GroupProjIDX := -1;
  ReadMeIDX := -1;
  ConstructedRuntimeProjects := String.Empty;
  ConstructedComponentProjects := String.Empty;
  ConstructedRuntimeTargets := String.Empty;
  ConstructedComponentTargets := String.Empty;
  ConstructedBuildList := String.Empty;
  ConstructedCleanList := String.Empty;
  ConstructedMakeList := String.Empty;
  ConstructedReadMe := String.Empty;

  // Sanity Check One
  if TemplateList.Count <> ReplacementList.Count then
    Raise Exception.Create('TExporter.Export - FileCount Error');

  // Sanity Check Two
  for IDX := 0 to ReplacementList.Count - 1 do
    begin
      Tokens := ReplacementList[IDX];
      Template := TemplateList[IDX];
      if Tokens.tfile <> Template.TplFileName then
        Raise Exception.Create('TExporter.Export - File Order Error');
    end;

  // Replace tokens in component project files
  for IDX := 0 to ReplacementList.Count - 1 do
    begin
      Tokens := ReplacementList[IDX];
      Template := TemplateList[IDX];

      if Tokens.tmime = 0 then // Tokens.tmime = 0 is static filename + text so just copy write it
        begin
          ReplacedFileName := FullCopy(Template.TplFileName); // One file has a name alteration
          RenameTemplateFile(ReplacedFileName);
          WriteFile(ReplacedFileName, Template.TplTemplate);
          Continue;
        end;

      if Tokens.tmime = 1024 then // Tokens.tmime = This projects .groupproj
        begin
          GroupProjIDX := IDX;
          Continue;
        end;

      if Tokens.tmime = 1025 then // Tokens.tmime = This projects README.md
        begin
          ReadMeIDX := IDX;
          Continue;
        end;

      for Comp := 0 to Length(ProjectSettings.ComponentSettings) - 1 do
        begin
          ReplacedFileName := FullCopy(Template.TplFileName);
          ReplacedText := FullCopy(Template.TplTemplate);

          for Rep := 0 to Length(Tokens.xlat) - 1 do
            begin
              ReplaceTokens(ReplacedText, Tokens.xlat[Rep], ProjectSettings.ComponentSettings[Comp]);
              RenameTemplateFile(ReplacedFileName, ProjectSettings.ComponentSettings[Comp]);
            end;
          WriteFile(ReplacedFileName, ReplacedText);
        end;
    end;

    // We need the Delphi Component name for each component
    // Store then while doing the images for the Components
    // The component names need to be known before the
    // .groupproj is templated as some of the replacement
    // text in them is lists of projects etc so these are
    // built then replaced in the template
    SetLength(CompNames, Length(ProjectSettings.ComponentSettings));

    // Create images based on the names of all components
    // using the 128x128 PackageIcon held in each ComponentSetting record.
    for Comp := 0 to Length(ProjectSettings.ComponentSettings) - 1 do
      begin
        CompNames[Comp] := ProjectSettings.ComponentSettings[Comp].DelphiPackageName;

        ReplacedFileName := '__COMPONENT__';
        RenameTemplateFile(ReplacedFileName, ProjectSettings.ComponentSettings[Comp]);
        WriteBitmap('images/bmp/128/' + ReplacedFileName + '.bmp', 128, ProjectSettings.ComponentSettings[Comp].PackageIcon);
        WriteBitmap('images/png/128/' + ReplacedFileName + '.png', 128, ProjectSettings.ComponentSettings[Comp].PackageIcon);
        WriteBitmap('images/bmp/32/' + ReplacedFileName + '.bmp', 32, ProjectSettings.ComponentSettings[Comp].PackageIcon);
        WriteBitmap('images/png/32/' + ReplacedFileName + '.png', 32, ProjectSettings.ComponentSettings[Comp].PackageIcon);
        WriteBitmap('images/bmp/24/' + ReplacedFileName + '.bmp', 24, ProjectSettings.ComponentSettings[Comp].PackageIcon);
        WriteBitmap('images/png/24/' + ReplacedFileName + '.png', 24, ProjectSettings.ComponentSettings[Comp].PackageIcon);
        WriteBitmap('images/bmp/16/' + ReplacedFileName + '.bmp', 16, ProjectSettings.ComponentSettings[Comp].PackageIcon);
        WriteBitmap('images/png/16/' + ReplacedFileName + '.png', 16, ProjectSettings.ComponentSettings[Comp].PackageIcon);

        ConstructedRuntimeProjects := ConstructedRuntimeProjects + RuntimeProjects;
        for Rep := 0 to Length(ConstructedReplacements) - 1 do
          ReplaceTokens(ConstructedRuntimeProjects, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);

        ConstructedComponentProjects := ConstructedComponentProjects + ComponentProjects;
        for Rep := 0 to Length(ConstructedReplacements) - 1 do
          ReplaceTokens(ConstructedComponentProjects, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);

        ConstructedRuntimeTargets := ConstructedRuntimeTargets + RuntimeTargets;
        for Rep := 0 to Length(ConstructedReplacements) - 1 do
          ReplaceTokens(ConstructedRuntimeTargets, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);

        ConstructedComponentTargets := ConstructedComponentTargets + ComponentTargets;
        for Rep := 0 to Length(ConstructedReplacements) - 1 do
          ReplaceTokens(ConstructedComponentTargets, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);


        if Comp <> 0 then
          ConstructedBuildList := ConstructedBuildList + ';';
        ConstructedBuildList := ConstructedBuildList + BuildComponentList;
        for Rep := 0 to Length(ConstructedReplacements) - 1 do
          ReplaceTokens(ConstructedBuildList, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);

        if Comp <> 0 then
          ConstructedCleanList := ConstructedCleanList + ';';
        ConstructedCleanList := ConstructedCleanList + CleanComponentList;
        for Rep := 0 to Length(ConstructedReplacements) - 1 do
          ReplaceTokens(ConstructedCleanList, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);

        if Comp <> 0 then
          ConstructedMakeList := ConstructedMakeList + ';';
        ConstructedMakeList := ConstructedMakeList + MakeComponentList;
        for Rep := 0 to Length(ConstructedReplacements) - 1 do
          ReplaceTokens(ConstructedMakeList, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);

        ConstructedReadMe := ConstructedReadMe + '![' + ProjectSettings.ComponentSettings[Comp].DelphiPackageName + '](images/png/128/' + ProjectSettings.ComponentSettings[Comp].DelphiPackageName + '.png)' + sLineBreak + sLineBreak;
        if ProjectSettings.ComponentSettings[Comp].PackageDesc = String.Empty then
          ConstructedReadMe := ConstructedReadMe + '* **' + ProjectSettings.ComponentSettings[Comp].DelphiPackageName + '**' + sLineBreak + sLineBreak
        else
          ConstructedReadMe := ConstructedReadMe + '* **' + ProjectSettings.ComponentSettings[Comp].DelphiPackageName + '** - ' + ProjectSettings.ComponentSettings[Comp].PackageDesc + sLineBreak + sLineBreak;
        if ProjectSettings.ComponentSettings[Comp].HomeURL <> String.Empty then
          ConstructedReadMe := ConstructedReadMe + 'Homepage : ' + ProjectSettings.ComponentSettings[Comp].HomeURL + sLineBreak + sLineBreak;
        if ProjectSettings.ComponentSettings[Comp].PyPiURL <> String.Empty then
          ConstructedReadMe := ConstructedReadMe  + 'PIPi : ' + ProjectSettings.ComponentSettings[Comp].PyPiURL + sLineBreak + sLineBreak;
        if ProjectSettings.ComponentSettings[Comp].DocURL <> String.Empty then
          ConstructedReadMe := ConstructedReadMe  + 'Documentation : ' + ProjectSettings.ComponentSettings[Comp].DocURL + sLineBreak + sLineBreak;
        if ProjectSettings.ComponentSettings[Comp].GithubURL <> String.Empty then
          ConstructedReadMe := ConstructedReadMe  + 'Github : ' + ProjectSettings.ComponentSettings[Comp].GithubURL + sLineBreak;
        ConstructedReadMe := ConstructedReadMe + sLineBreak + sLineBreak;
      end;

    Tokens := ReplacementList[GroupProjIDX];
    Template := TemplateList[GroupProjIDX];
    ReplacedFileName := FullCopy(Template.TplFileName);
    ReplacedText := FullCopy(Template.TplTemplate);
    RenameTemplateFile(ReplacedFileName);

    // Replace in groupproj
    ReplacedText := ReplacedText.Replace('__RUNTIME_PROJECTS__', ConstructedRuntimeProjects);
    ReplacedText := ReplacedText.Replace('__COMPONENT_PROJECTS__', ConstructedComponentProjects);
    ReplacedText := ReplacedText.Replace('__RUNTIME_TARGETS__', ConstructedRuntimeTargets);
    ReplacedText := ReplacedText.Replace('__COMPONENT_TARGETS__', ConstructedComponentTargets);

    ReplacedText := ReplacedText.Replace('__BUILD_LIST__', ConstructedBuildList);
    ReplacedText := ReplacedText.Replace('__CLEAN_LIST__', ConstructedCleanList);
    ReplacedText := ReplacedText.Replace('__MAKE_LIST__', ConstructedMakeList);

    for Rep := 0 to Length(Tokens.xlat) - 1 do
      ReplaceTokens(ReplacedText, Tokens.xlat[Rep]);

    // And write it
    WriteFile(ReplacedFileName, ReplacedText);

    Tokens := ReplacementList[ReadMeIDX];
    Template := TemplateList[ReadMeIDX];
    ReplacedFileName := FullCopy(Template.TplFileName);
    ReplacedText := FullCopy(Template.TplTemplate);
    RenameTemplateFile(ReplacedFileName);

    // Replace groupproj
    for Rep := 0 to Length(Tokens.xlat) - 1 do
      ReplaceTokens(ReplacedText, Tokens.xlat[Rep]);
    ReplacedText := ReplacedText.Replace('__WEB_URLS__', ConstructedReadMe);

    WriteFile(ReplacedFileName, ReplacedText);

    SetLength(CompNames, 0);
end;

constructor TExporter.Create;
begin
  Inherited Create;
  FFilePath := String.Empty;
  FAllowOverWrite := False;
  FWipeBeforeExport := False;
end;

Destructor TExporter.Destroy;
begin
  Inherited Destroy;
end;

procedure TExporter.Open(const APath: String);
begin
  FFilePath := APath;
  FFileIsOpen := True;
end;

procedure TExporter.WriteFile(const AFile: String; const AStr: String);
begin
end;
procedure TExporter.WriteBitmap(const AFile: String; const ABase64Bitmap: String);
begin
end;

procedure TExporter.WriteBitmap(const AFile: String; const Size: Integer; const ABase64Bitmap: String);
begin
end;

procedure TExporter.WriteBitmap(const AFile: String; const AWidth: Integer; const AHeight: Integer; const ABase64Bitmap: String);
begin
end;
procedure TExporter.Close;
begin
  FFileIsOpen := False;
end;

constructor TFileExporter.Create;
begin
  Inherited Create;
end;

Destructor TFileExporter.Destroy;
begin
  Inherited Destroy;
end;

procedure TFileExporter.Open(const APath: String);
begin
  Inherited;
  try
    Log('TFileExporter.Open ' + FFilePath);
    if FWipeBeforeExport then
      begin
        // Don't let them delete C:\ :)
        if Length(FFilePath) > 4 then
          begin
            if DirectoryExists(FFilePath) then
              TDirectory.Delete(FFilePath, True);
            ForceDirectories(FFilePath);
          end;
      end;
    if not DirectoryExists(FFilePath) then
       Raise Exception.Create('TFileExporter.Open : Directory Does Not Exist - '  + FFilePath);
    if not AllowOverWrite then
      if not TDirectory.IsEmpty(FFilePath) then
         Raise Exception.Create('TFileExporter.Open : Directory Exists but has content - '  + FFilePath);
  except
    on E : Exception do
      begin
        Log('TFileExporter.Open - Exception : Class = ' +
          E.ClassName + ', Message = ' + E.Message);
        Raise Exception.Create('TFileExporter.Open - Exception : Class = ' +
          E.ClassName + ', Message = ' + E.Message);
      end;
  end;
end;

function TFileExporter.CheckPath(const AFileName: String): String;
var
  FullPath: String;
  FilePath: String;
  FileName: String;
begin
{$IFDEF MSWINDOWS}
  FileName := StringReplace(AFileName, '/', '\', [rfReplaceAll]);
{$ELSE}
  FileName := AFileName;
{$ENDIF}
    FullPath := TPath.Combine(IncludeTrailingPathDelimiter(FFilePath), FileName);
    FilePath := ExcludeTrailingPathDelimiter(ExtractFilePath(FullPath));
    if not DirectoryExists(FilePath) then
      if not ForceDirectories(FilePath) then
         Raise Exception.Create('TFileExporter.WriteFile couldn''t create '  + FilePath);
  Result := FullPath;
end;

procedure TFileExporter.WriteFile(const AFile: String; const AStr: String);
var
  FullPath: String;
begin
  try
    FullPath := CheckPath(AFile);
    Log('TFileExporter.WriteFile Path = ' + FullPath);
    TFile.WriteAllText(FullPath, AStr);
  except
    on E : Exception do
      begin
        Log('TFileExporter.WriteFile - Exception : Class = ' +
          E.ClassName + ', Message = ' + E.Message);
        Raise Exception.Create('TFileExporter.WriteFile - Exception : Class = ' +
          E.ClassName + ', Message = ' + E.Message);
      end;
  end;
end;

procedure TFileExporter.WriteBitmap(const AFile: String; const ABase64Bitmap: String);
begin
  WriteBitmap(AFile, 0, 0, ABase64Bitmap);
end;

procedure TFileExporter.WriteBitmap(const AFile: String; const Size: Integer; const ABase64Bitmap: String);
begin
  WriteBitmap(AFile, Size, Size, ABase64Bitmap);
end;

procedure TFileExporter.WriteBitmap(const AFile: String; const AWidth: Integer; const AHeight: Integer; const ABase64Bitmap: String);
var
  LBitmap: TBitmap;
  FullPath: String;
begin
  LBitmap := TBitmap.Create;
  try
    try
      FullPath := CheckPath(AFile);
      Log('TFileExporter.WriteFile Path = ' + FullPath);
      DecodeBase64Image(LBitmap, ABase64Bitmap);
      if (AWidth > 0) and (AHeight > 0) then
        begin
          if (AWidth <> LBitmap.Width) and (AHeight <> LBitmap.Height) then
            LBitmap.Resize(AWidth, AHeight);
        end;
      LBitmap.SaveToFile(FullPath);
    except
      on E : Exception do
        begin
          Log('TFileExporter.WriteBitmap - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
          Raise Exception.Create('TFileExporter.WriteBitmap - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
        end;
    end;
  finally
    LBitmap.Free;
  end;
end;

procedure TFileExporter.Close;
begin
  Log('TFileExporter.Close');
  Inherited;
end;

constructor TZipExporter.Create;
begin
  Inherited Create;
end;

Destructor TZipExporter.Destroy;
begin
  Inherited Destroy;
end;

procedure TZipExporter.Open(const APath: String);
begin
  Inherited;
  try
    Log('TZipExporter.Open ' + FFilePath);
    if FWipeBeforeExport then
      begin
        if FileExists(FFilePath) then
          TFile.Delete(FFilePath);
      end;
    if not AllowOverWrite then
      if FileExists(FFilePath) then
         Raise Exception.Create('TZipExporter.Open : File Exists - '  + FFilePath);

    FZipFile := TZipfile.Create;
    FZipFile.Open(FFilePath, TZipMode.zmWrite);
  except
    on E : Exception do
      begin
        Log('TZipExporter.Open - Exception : Class = ' +
          E.ClassName + ', Message = ' + E.Message);
        Raise Exception.Create('TZipExporter.Open - Exception : Class = ' +
          E.ClassName + ', Message = ' + E.Message);
      end;
  end;
end;

procedure TZipExporter.WriteFile(const AFile: String; const AStr: String);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(AStr);
  try
    try
      Log('TZipExporter.WriteFile Path = ' + AFile);
      SS.Position := 0;
      FZipFile.Add(SS, AFile);
    except
      on E : Exception do
        begin
          Log('TZipExporter.WriteFile - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
          Raise Exception.Create('TZipExporter.WriteFile - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
        end;
    end;
  finally
    SS.Free;
  end;
end;

procedure TZipExporter.WriteBitmap(const AFile: String; const ABase64Bitmap: String);
begin
  WriteBitmap(AFile, 0, 0, ABase64Bitmap);
end;

procedure TZipExporter.WriteBitmap(const AFile: String; const Size: Integer; const ABase64Bitmap: String);
begin
  WriteBitmap(AFile, Size, Size, ABase64Bitmap);
end;

procedure TZipExporter.WriteBitmap(const AFile: String; const AWidth: Integer; const AHeight: Integer; const ABase64Bitmap: String);
var
 // i: TBMPImage;
  LBitmap: TBitmap;
  MStream: TMemoryStream;
  Surf: TBitmapSurface;
  Ext: String;
begin
  Ext := ExtractFileExt(AFile);
  if TBitmapCodecManager.CodecExists(Ext) then
    begin
      LBitmap := TBitmap.Create;
      MStream := TMemoryStream.Create;
      try
        try
          Log('TZipExporter.WriteBitmap Path = ' + AFile);
          DecodeBase64Image(LBitmap, ABase64Bitmap);
          if (AWidth > 0) and (AHeight > 0) then
            begin
              if (AWidth <> LBitmap.Width) and (AHeight <> LBitmap.Height) then
                LBitmap.Resize(AWidth, AHeight);
            end;
          Surf := TBitmapSurface.Create;
          try
            Surf.Assign(LBitmap);
            // use the codec to save Surface to stream
            if not TBitmapCodecManager.SaveToStream(
                             MStream,
                             Surf,
                             Ext) then
              raise EBitmapSavingFailed.Create(
                    'Error saving Bitmap to ' + Ext);
          finally
            Surf.Free;
          end;
    //      LBitmap.SaveToStream(MStream);
          MStream.Position := 0;
          FZipFile.Add(MStream, AFile);
        except
          on E : Exception do
            begin
              Log('TZipExporter.WriteBitmap - Exception : Class = ' +
                E.ClassName + ', Message = ' + E.Message);
              Raise Exception.Create('TZipExporter.WriteBitmap - Exception : Class = ' +
                E.ClassName + ', Message = ' + E.Message);
            end;
        end;
      finally
        MStream.Free;
        LBitmap.Free;
      end;
    end;
end;

procedure TZipExporter.Close;
begin
  if Assigned(FZipFile) then
    begin
      FZipFile.Close;
      FZipFile.Free;
    end;
  Log('TZipExporter.Close');
  Inherited;
end;

end.
