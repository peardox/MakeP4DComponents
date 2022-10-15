unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  System.Zip, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  PyCommon, PyModule, PyPackage, H5Py;

type
  TZipFileHelper = class helper for TZipFile
    procedure ExtractToStream(var OutStream: TStream; Index: Integer; const Path: string; CreateSubdirs: Boolean = True);
  end;

  TZipExtractForm = class(TForm)
   ProgressBar1: TProgressBar;
    Panel1: TPanel;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    Memo1: TMemo;
    H5Py1: TH5Py;
    procedure Button1Click(Sender: TObject);
    procedure ExtractOneZip(const AFile: String; const DestPath: String);
    procedure ShowZipProgress(Sender: TObject; AFilename: String; AHeader: TZipHeader; APosition: Int64);
    procedure ExtractTemplate;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    ProgCount: Int64;
    PaintCount: Int64;
    ProgTick: Int64;
    ProgFile: String;
    ProgressBar: TProgressBar;
    procedure FormReset;
    procedure Log(const AMsg: String);
    function ClassExists(const AClass: String): Boolean;
  public
    { Public declarations }
  end;

var
  ZipExtractForm: TZipExtractForm;
  AppHome: String;

implementation

{$R *.fmx}
uses
  Math,
  System.IOUtils;

procedure TZipFileHelper.ExtractToStream(var OutStream: TStream; Index: Integer; const Path: string; CreateSubdirs: Boolean);
var
  LInStream, LOutStream: TStream;
  LHeader: TZipHeader;
  LDir, LFileName: string;
  LModifiedDateTime: TDateTime;

  FCurrentHeader: TZipHeader;
  FCurrentFile: String;

  // Not exposed by TZip.pas
  function WinFileDateToDateTime(FileDate: UInt32; out DateTime: TDateTime): Boolean;
  var
    LDate: TDateTime;
    LTime: TDateTime;
  begin
    Result := TryEncodeDate(
      LongRec(FileDate).Hi shr 9 + 1980,
      LongRec(FileDate).Hi shr 5 and 15,
      LongRec(FileDate).Hi and 31,
      LDate);

    if Result then
    begin
      Result := TryEncodeTime(
        LongRec(FileDate).Lo shr 11,
        LongRec(FileDate).Lo shr 5 and 63,
        LongRec(FileDate).Lo and 31 shl 1, 0, LTime);

      if Result then
        DateTime := LDate + LTime;
    end;
  end;

begin
  // Get decompression stream for file
  Read(Index, LInStream, LHeader);
  FCurrentHeader := LHeader;
  try
    if not GetUTF8PathFromExtraField(LHeader, LFileName) then
      LFileName := InternalGetFileName(Index);
{$IFDEF MSWINDOWS} // ZIP stores files with '/', so translate to a relative Windows path.
    LFileName := StringReplace(LFileName, '/', '\', [rfReplaceAll]);
{$ENDIF}
    // CreateSubDirs = False assumes the user passed in the path where they want the file to end up
    if CreateSubdirs then
      LFileName := TPath.Combine(Path, LFileName)
    else
      LFileName := TPath.Combine(Path, ExtractFileName(LFileName));
    // Force directory creation
    LDir := ExtractFileDir(LFileName);
    if CreateSubdirs and (LDir <> '') then
      TDirectory.CreateDirectory(ExtractFileDir(LFileName));
    // Open the File For output
    if LFileName.Chars[LFileName.Length-1] = PathDelim then
      Exit; // Central Directory Entry points at a directory, not a file.
    LOutStream := TFileStream.Create(LFileName, fmCreate);
    try // And Copy from the decompression stream.
      FCurrentFile := LFileName;
      // See Bit 3 at http://www.pkware.com/documents/casestudies/APPNOTE.TXT
      if (LHeader.Flag and (1 shl 3)) = 0 then
      begin
        // Empty files should not be read
        if LHeader.UncompressedSize > 0 then
          LOutStream.CopyFrom(LInStream, LHeader.UncompressedSize);
      end
      else
      begin
        LOutStream.CopyFrom(LInStream, LHeader.UncompressedSize);
      end;
      if Assigned(OnProgress) then
        OnProgress(Self, FCurrentFile, FCurrentHeader, LOutStream.Position);
    finally
      LOutStream.Free;
      FCurrentFile := '';
    end;
    if FileExists(LFileName) then
    begin
      if WinFileDateToDateTime(LHeader.ModifiedDateTime, LModifiedDateTime) then
      begin
        TFile.SetCreationTime(LFileName, LModifiedDateTime);
        TFile.SetLastWriteTime(LFileName, LModifiedDateTime);
      end;
{$IFDEF MSWINDOWS}
      if (Hi(LHeader.MadeByVersion) = MADEBY_MSDOS) then
        TFile.SetAttributes(LFileName, TFile.IntegerToFileAttributes(LHeader.ExternalAttributes and $000000FF));
{$ENDIF}
{$IFDEF POSIX}
      if (Hi(FFiles[Index].MadeByVersion) = MADEBY_UNIX) and (FFiles[Index].ExternalAttributes shr 16 <> 0) then
        TFile.SetAttributes(LFileName, TFile.IntegerToFileAttributes(FFiles[Index].ExternalAttributes shr 16));
{$ENDIF}
    end;
  finally
    FCurrentHeader := Default(TZipHeader);
    LInStream.Free;
  end;
end;


procedure TZipExtractForm.Log(const AMsg: String);
begin
  Memo1.Lines.Add(AMsg);
end;

procedure TZipExtractForm.ShowZipProgress(Sender: TObject; AFilename: String; AHeader: TZipHeader; APosition: Int64);
var
  PosTick: Int64;
begin
  if Assigned(ProgressBar) then
    begin
      if(ProgFile <> AFilename) then
        begin
          ProgFile := AFilename;
          Inc(ProgCount);
          ProgressBar.Value := ProgCount;
          PosTick := floor((ProgCount / ProgressBar.Max) * ProgressBar.Width);
          if(ProgTick <> PosTick) then
            begin
              ProgTick := PosTick;
              Inc(PaintCount);
              ProgressBar.Repaint;
              Application.ProcessMessages;
            end;
        end;
    end;
end;

procedure TZipExtractForm.Button2Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TZipExtractForm.ExtractOneZip(const AFile: String; const DestPath: String);
var
  z: TZipFile;
  I, ZipCount: Int64;
  OutStream: TStream;
begin
  ProgressBar.Value := 0;
  ProgFile := '';
  ProgTick := 0;
  ProgCount := 0;
  PaintCount := 0;
  Application.ProcessMessages;

  try
    try
      z := TZipFile.Create;
      z.Open(AFile, TZipMode.zmRead);


      ZipCount := Length(z.FileNames);
      Log('Files = ' + IntToStr(ZipCount));

      ProgressBar.Min := 0;
      ProgressBar.Max := ZipCount - 1;
      ProgressBar.Value := 0;
      z.OnProgress := ShowZipProgress;

      for I := 0 to ZipCount - 1 do
        begin
          Log(sLineBreak + 'Processing ' + AFile
            + ' (' + IntToStr(I+1)
            + ' of ' + IntToStr(ZipCount) + ')');
{
            + sLineBreak
            + sLineBreak
            + 'Extracting ' + z.FileName[I]
}
            ;
          z.ExtractToStream(OutStream, i, DestPath);
        end;

    except
      on E: Exception do
        begin
          Log('Unhandled Exception in ExtractOneZip');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end;
    end;
  finally
    z.Free;
  end;
end;

function TZipExtractForm.ClassExists(const AClass: String): Boolean;
var
  ExistingClass: TPersistentClass;
begin
  Result := False;
  try
    try
      ExistingClass := GetClass(AClass);
      RegisterComponents('Python Packages for Delphi', [TH5Py]);
      if ExistingClass = Nil then
        Log('Not Registered')
      else
        Log('Registered');
    except
      on E: Exception do
        begin
          Result := True;
          Log('Unhandled Exception in ClassExists');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end;
    end;
  finally

  end;
end;

procedure TZipExtractForm.ExtractTemplate;
var
  z: TZipFile;
  ZipOut: String;
  I: Integer;
begin
  ZipOut := TPath.Combine(AppHome, 'extracted');

  ClassExists('TH5Py');

  ExtractOneZip('template.zip', ZipOut);

  Log('Extracted Template');
  Button1.Text := 'Close';
  Button1.Enabled := True;

  ProgressBar.Value := 0;
end;

procedure TZipExtractForm.Button1Click(Sender: TObject);
begin
  Button2.Visible := False;

  Button1.Enabled := False;
  Button2.Enabled := False;
  Application.ProcessMessages;

  if Button1.Text = 'Close' then
    begin
      Application.Terminate
    end
  else
    ExtractTemplate;
end;

procedure TZipExtractForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult <> mrClose then
    ModalResult := mrCancel;
end;

procedure TZipExtractForm.FormCreate(Sender: TObject);
var
  DownPath: String;
  DocPath: String;
begin
  AppHome := '.';
  Log('Import a Lartis Style Archive');
  ProgressBar := ProgressBar1;
  DocPath := TPath.GetDocumentsPath;
  DownPath := ExpandFileName(TPath.Combine(TPath.Combine(DocPath, '..'), 'Downloads'));
  if not DirectoryExists(DownPath) then
    begin
      if DirectoryExists(DocPath) then
        DownPath := DocPath
      else
        DownPath := '';
    end;

  OpenDialog1.Filter:='Lartis Style Archives (*.lartis.zip)|*.lartis.zip';
  OpenDialog1.InitialDir := DownPath;
end;

procedure TZipExtractForm.FormShow(Sender: TObject);
begin
  FormReset;
end;

procedure TZipExtractForm.FormActivate(Sender: TObject);
begin
//  FormReset;
end;

procedure TZipExtractForm.FormReset;
begin
  Button1.Enabled := True;
  Button1.Text := 'Import';
  Button2.Enabled := True;
  Button2.Visible := True;
end;


end.
