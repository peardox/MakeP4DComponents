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
  System.JSON.Serializers,
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
  ConstructedP4DBuildList: String;
  ConstructedP4DCleanList: String;
  ConstructedP4DMakeList: String;
  ConstructedDCLBuildList: String;
  ConstructedDCLCleanList: String;
  ConstructedDCLMakeList: String;
  ConstructedReadMe: String;
  ProjectFileName: String;
  ProjectFileText: String;
  lSerializer: TJsonSerializer;
begin
  GroupProjIDX := -1;
  ReadMeIDX := -1;
  ConstructedRuntimeProjects := String.Empty;
  ConstructedComponentProjects := String.Empty;
  ConstructedRuntimeTargets := String.Empty;
  ConstructedComponentTargets := String.Empty;
  ConstructedP4DBuildList := String.Empty;
  ConstructedP4DCleanList := String.Empty;
  ConstructedP4DMakeList := String.Empty;
  ConstructedDCLBuildList := String.Empty;
  ConstructedDCLCleanList := String.Empty;
  ConstructedDCLMakeList := String.Empty;
  ConstructedReadMe := String.Empty;

  try
    try
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
            ConstructedP4DBuildList := ConstructedP4DBuildList + ';';
          ConstructedP4DBuildList := ConstructedP4DBuildList + BuildP4DComponentList;
          for Rep := 0 to Length(ConstructedReplacements) - 1 do
            ReplaceTokens(ConstructedP4DBuildList, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);

          if Comp <> 0 then
            ConstructedDCLBuildList := ConstructedDCLBuildList + ';';
          ConstructedDCLBuildList := ConstructedDCLBuildList + BuildDCLComponentList;
          for Rep := 0 to Length(ConstructedReplacements) - 1 do
            ReplaceTokens(ConstructedDCLBuildList, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);

          if Comp <> 0 then
            ConstructedP4DCleanList := ConstructedP4DCleanList + ';';
          ConstructedP4DCleanList := ConstructedP4DCleanList + CleanP4DComponentList;
          for Rep := 0 to Length(ConstructedReplacements) - 1 do
            ReplaceTokens(ConstructedP4DCleanList, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);

          if Comp <> 0 then
            ConstructedDCLCleanList := ConstructedDCLCleanList + ';';
          ConstructedDCLCleanList := ConstructedDCLCleanList + CleanDCLComponentList;
          for Rep := 0 to Length(ConstructedReplacements) - 1 do
            ReplaceTokens(ConstructedDCLCleanList, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);

          if Comp <> 0 then
            ConstructedP4DMakeList := ConstructedP4DMakeList + ';';
          ConstructedP4DMakeList := ConstructedP4DMakeList + MakeP4DComponentList;
          for Rep := 0 to Length(ConstructedReplacements) - 1 do
            ReplaceTokens(ConstructedP4DMakeList, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);

          if Comp <> 0 then
            ConstructedDCLMakeList := ConstructedDCLMakeList + ';';
          ConstructedDCLMakeList := ConstructedDCLMakeList + MakeDCLComponentList;
          for Rep := 0 to Length(ConstructedReplacements) - 1 do
            ReplaceTokens(ConstructedDCLMakeList, ConstructedReplacements[Rep], ProjectSettings.ComponentSettings[Comp]);

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

      ReplacedText := ReplacedText.Replace('__BUILD_LIST__', ConstructedP4DBuildList + ';' + ConstructedDCLBuildList);
      ReplacedText := ReplacedText.Replace('__CLEAN_LIST__', ConstructedP4DCleanList + ';' + ConstructedDCLCleanList);
      ReplacedText := ReplacedText.Replace('__MAKE_LIST__', ConstructedP4DMakeList + ';' + ConstructedDCLMakeList);

      for Rep := 0 to Length(Tokens.xlat) - 1 do
        ReplaceTokens(ReplacedText, Tokens.xlat[Rep]);

      // And write it
      WriteFile(ReplacedFileName, ReplacedText);

      Tokens := ReplacementList[ReadMeIDX];
      Template := TemplateList[ReadMeIDX];
      ReplacedFileName := FullCopy(Template.TplFileName);
      ReplacedText := FullCopy(Template.TplTemplate);
      RenameTemplateFile(ReplacedFileName);

      // Replace the README.md
      for Rep := 0 to Length(Tokens.xlat) - 1 do
        ReplaceTokens(ReplacedText, Tokens.xlat[Rep]);
      ReplacedText := ReplacedText.Replace('__WEB_URLS__', ConstructedReadMe);

      WriteFile(ReplacedFileName, ReplacedText);
    except
      on E : Exception do
        begin
          Log('TFileExporter.Export - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
          Raise Exception.Create('TFileExporter.Open - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
        end;
    end;
  finally
    SetLength(CompNames, 0);
  end;

  // This is not part of the template, per-se
  // As the Component Package that is being created may need adjusting
  // by a third party we'll save the p4d that would result from a
  // File -> Save and add it to the Component Package thereby providing
  // the ability to modify the resultant output by anyone who has
  // a copy of this application
  if Assigned(ProjectSettings) then
    begin
      ProjectFileName := ProjectSettings.ProjectGroupName + '.p4d';
      lSerializer := TJsonSerializer.Create;
      try
        try
          ProjectFileText := lSerializer.Serialize<TProjectSettings>(ProjectSettings);
          WriteFile(ProjectFileName, ProjectFileText);
        except
          on E : Exception do
            begin
              Log('TFileExporter.Export - Exception : Class = ' +
                E.ClassName + ', Message = ' + E.Message);
              Raise Exception.Create('TFileExporter.Open - Exception : Class = ' +
                E.ClassName + ', Message = ' + E.Message);
            end;
        end;

      finally
        lSerializer.Free;
      end;
    end;
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
