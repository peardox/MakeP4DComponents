unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  System.Zip, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  PyCommon, PyModule, PyPackage, H5Py;

type
  TTemplateFile = Class
    TplFileName: String;
    TplTemplate: String;
    constructor Create(AFileName: string; ATemplate: String);
  End;

  TZipFileHelper = class helper for TZipFile
    function ExtractToStream(var OutStream: TStream; Index: Integer): TTemplateFile;
  end;

  TMainForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure ExtractTemplateResourceZip;
    procedure ExtractTemplate;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    TemplateList: TList;
    procedure FormReset;
    procedure FreeList;
    procedure Log(const AMsg: String);
    procedure HaltAndCatchFire;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  AppHome: String;

implementation

{$R *.fmx}
{$R EmbeddedResources.RES}

uses
  Math,
  System.IOUtils;

constructor TTemplateFile.Create(AFileName: string; ATemplate: String);
begin
  Inherited Create;
  TplFileName := AFileName;
  TplTemplate := ATemplate;
end;

function TemplateSortFunc(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TTemplateFile(Item1).TplFileName, TTemplateFile(Item2).TplFileName);
end;

Function TZipFileHelper.ExtractToStream(var OutStream: TStream; Index: Integer): TTemplateFile;
var
  LInStream: TStream;
  LHeader: TZipHeader;
  LDir, LFileName: string;
  LModifiedDateTime: TDateTime;
  Template: TTemplateFile;
  TemplateText: String;
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
    if LFileName.Chars[LFileName.Length-1] = PathDelim then
      Exit(Nil); // Central Directory Entry points at a directory, not a file.
    try // And Copy from the decompression stream.
      FCurrentFile := LFileName;
      // See Bit 3 at http://www.pkware.com/documents/casestudies/APPNOTE.TXT
      if (LHeader.Flag and (1 shl 3)) = 0 then
      begin
        // Empty files should not be read
        if LHeader.UncompressedSize > 0 then
          begin
            SetString(TemplateText, PChar(LInStream), LHeader.UncompressedSize);
            Template := TTemplateFile.Create(FCurrentFile, TemplateText);
          end;
      end
      else
      begin
        SetString(TemplateText, PChar(LInStream), LHeader.UncompressedSize);
        Template := TTemplateFile.Create(FCurrentFile, TemplateText);
      end;
    finally
      FCurrentFile := '';
    end;
  finally
    LInStream.Free;
    Result := Template;
  end;
end;


procedure TMainForm.Log(const AMsg: String);
begin
  Memo1.Lines.Add(AMsg);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.ExtractTemplateResourceZip;
var
  z: TZipFile;
  I, ZipCount: Int64;
  OutStream: TStream;
  LResStream: TResourceStream;
  Template: TTemplateFile;
begin
  Application.ProcessMessages;

  try
    try
      z := TZipFile.Create;
      LResStream := TResourceStream.Create(HInstance, 'Template', RT_RCDATA);
      z.Open(LResStream, TZipMode.zmRead);


      ZipCount := Length(z.FileNames);
      Log('Files = ' + IntToStr(ZipCount));

      if Assigned(TemplateList) then
        FreeList;
      TemplateList := TList.Create;

      for I := 0 to ZipCount - 1 do
        begin
          Template := z.ExtractToStream(OutStream, i);
          if not (Template = Nil) then
            begin
              TemplateList.Add(Template);
            end;
        end;

    except
      on E: Exception do
        begin
          Log('Unhandled Exception in ExtractTemplateResourceZip');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end;
    end;
  finally
    z.Free;
    LResStream.Free;
    if Assigned(TemplateList) then
      begin
        TemplateList.Sort(@TemplateSortFunc);
        Log('Templates = ' + IntToStr(TemplateList.Count));
        for I := 0 to TemplateList.Count -1 do
          begin
            Template := TemplateList[I];
            Log(Template.TplFileName);
          end;
      end;
  end;
end;

procedure TMainForm.HaltAndCatchFire;
begin
  ShowMessage('Something went horribly wrong');
  Application.Terminate;
end;

procedure TMainForm.ExtractTemplate;
begin
  ExtractTemplateResourceZip;
  if Not Assigned(TemplateList) then
    HaltAndCatchFire;
  if Not TemplateList.Count = 0 then
    HaltAndCatchFire;

  Log('Extracted Template');
  Button1.Text := 'Close';
  Button1.Enabled := True;
end;

procedure TMainForm.Button1Click(Sender: TObject);
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

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult <> mrClose then
    ModalResult := mrCancel;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  DownPath: String;
  DocPath: String;
begin
  AppHome := '.';
  Log('Import a Python Template');
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

// Tidy up after ourselves
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeList;
end;

procedure TMainForm.FreeList;
var
  I: Integer;
  Template: TTemplateFile;
begin
    if Assigned(TemplateList) then
      begin
        for I := 0 to TemplateList.Count -1 do
          begin
            Template := TemplateList[I];
            Template.Free;
          end;
        FreeAndNil(TemplateList);
      end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FormReset;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
//  FormReset;
end;

procedure TMainForm.FormReset;
begin
  Button1.Enabled := True;
  Button1.Text := 'Import';
  Button2.Enabled := True;
  Button2.Visible := True;
end;


end.
