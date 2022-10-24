unit ComponentExporter;

interface

uses
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
    property AllowOverWrite: Boolean Read FAllowOverWrite Write FAllowOverWrite;
    property WipeBeforeExport: Boolean Read FWipeBeforeExport Write FWipeBeforeExport;
  End;

  TFileExporter = Class(TExporter)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open(const APath: String); override;
    procedure WriteFile(const AFile: String; const AStr: String);
    procedure WriteBitmap(const AFile: String; const ABase64Bitmap: String); overload;
    procedure WriteBitmap(const AFile: String; const Size: Integer; const ABase64Bitmap: String); overload;
    procedure WriteBitmap(const AFile: String; const AWidth: Integer; const AHeight: Integer; const ABase64Bitmap: String); overload;
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
    procedure WriteFile(const AFile: String; const AStr: String);
    procedure WriteBitmap(const AFile: String; const ABase64Bitmap: String); overload;
    procedure WriteBitmap(const AFile: String; const Size: Integer; const ABase64Bitmap: String); overload;
    procedure WriteBitmap(const AFile: String; const AWidth: Integer; const AHeight: Integer; const ABase64Bitmap: String); overload;
    procedure Close; override;
  End;

implementation

uses
  FMX.Graphics,
  FMX.Surfaces,
  FMX.Types,
  Settings,
  System.Classes,
  System.IOUtils,
  System.SysUtils;

constructor TExporter.Create;
begin
  Inherited Create;
  FFilePath := '';
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
  FilePath: String;
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
  if FFileIsOpen then
    Log('File is open but who cares?');
  Log('TFileExporter.Close');
  Inherited;
  if not FFileIsOpen then
    Log('File is marked as closed');
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
