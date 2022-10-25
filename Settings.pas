unit Settings;

// Also includes odds + sods functions etc

interface

uses
  System.Classes,
  System.Zip,
  FMX.Dialogs,
  FMX.Graphics;

type
  TTemplateFile = Class
    TplFileName: String; // Filename
    TplTemplate: String; // Template Text with replacement tokens
    constructor Create(AFileName: string; ATemplate: String);
  End;

  //
  TReplacementToken = Class
    tfile: String; // Filename
    xlat: TArray<String>; // The tokens that need replacing in this template
    tmime: Integer; // A semi-bitmapped Mime-esque numbering system
                    // 0 = One-Off file
                    // 1 = Pascal File (pas)
                    // 2 = Package File (dpk)
                    // 3 = Project File (dproj)
                    // Bit 2 : Set if part of component, unset if runtime
    constructor Create(AFile: string; AToken: TArray<String>; ATMime: Integer);
  end;
  TReplacementTokens = TArray<TReplacementToken>;

  TZipFileHelper = class helper for TZipFile
    function ExtractToTemplate(Index: Integer): TTemplateFile; overload;
  end;

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
    PackageDesc: String;
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
function TokenSortFunc(Item1, Item2: Pointer): Integer;
function TemplateSortFunc(Item1, Item2: Pointer): Integer;
function FullCopy(const AStr: String): String;

implementation

uses
  System.IOUtils,
  System.NetEncoding,
  System.SysUtils,
  System.Json.Serializers;

function FullCopy(const AStr: String): String;
begin
  Result := Copy(AStr, 1, Length(AStr));
end;

procedure Log(const AMsg: String);
begin
  LogStrings.Add(AMsg);
end;

function TokenSortFunc(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TReplacementToken(Item1).tfile, TReplacementToken(Item2).tfile);
end;

function TemplateSortFunc(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TTemplateFile(Item1).TplFileName, TTemplateFile(Item2).TplFileName);
end;

constructor TComponentSettings.Create;
begin
  Inherited Create;
end;

procedure TComponentSettings.CopyFrom(AObject: TComponentSettings);
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
  PackageDesc := FullCopy(AObject.PackageDesc);
  PackageIcon := FullCopy(AObject.PackageIcon);
end;

constructor TProjectSettings.Create;
begin
  Inherited Create;
  ProjectVersion := '1.0.0';
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

constructor TReplacementToken.Create(AFile: string; AToken: TArray<String>; ATMime: Integer);
begin
  Inherited Create;
  tfile := AFile;
  xlat := AToken;
  tmime := ATMime;
end;

constructor TTemplateFile.Create(AFileName: string; ATemplate: String);
begin
  Inherited Create;
  TplFileName := AFileName;
  TplTemplate := ATemplate;
end;

Function TZipFileHelper.ExtractToTemplate(Index: Integer): TTemplateFile;
var
  LInStream: TStream;
  LHeader: TZipHeader;
  LFileName: string;
  Template: TTemplateFile;
  TemplateText: String;
  SS: TStringStream;
begin
  SS := TStringStream.Create;
  Template := Nil;
  // Get decompression stream for file
  Read(Index, LInStream, LHeader);
  try
    if not GetUTF8PathFromExtraField(LHeader, LFileName) then
      LFileName := InternalGetFileName(Index);
{$IFDEF MSWINDOWS} // ZIP stores files with '/', so translate to a relative Windows path.
    LFileName := StringReplace(LFileName, '/', '\', [rfReplaceAll]);
{$ENDIF}
    // Open the File For output
    if not (LFileName.Chars[LFileName.Length-1] = PathDelim) then
      Begin
        try // And Copy from the decompression stream.
          // See Bit 3 at http://www.pkware.com/documents/casestudies/APPNOTE.TXT
          if (LHeader.Flag and (1 shl 3)) = 0 then
          begin
            // Empty files should not be read
            if LHeader.UncompressedSize > 0 then
              begin
                SS.CopyFrom(LInStream , 0);
                TemplateText := SS.DataString;
                Template := TTemplateFile.Create(LFileName, TemplateText);
              end;
          end
          else
          begin
            SS.CopyFrom(LInStream , 0);
            TemplateText := SS.DataString;
            Template := TTemplateFile.Create(LFileName, TemplateText);
          end;
        except
          on E: Exception do
            begin
              Log('Unhandled Exception in ExtractToTemplate');
              Log('Class : ' + E.ClassName);
              Log('Error : ' + E.Message);
            end;
        end;
      end;
  finally
    SS.Free;
    LInStream.Free;
    Result := Template;
  end;
end;


end.
