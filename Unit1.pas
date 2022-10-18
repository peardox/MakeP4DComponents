unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  System.Zip, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  PyCommon, PyModule, PyPackage, H5Py, FMX.Edit, System.Rtti, FMX.Grid.Style,
  FMX.Grid;

type
  TTemplateFile = Class
    TplFileName: String;
    TplTemplate: String;
    constructor Create(AFileName: string; ATemplate: String);
  End;

  TZipFileHelper = class helper for TZipFile
    function ExtractToTemplate(Index: Integer): TTemplateFile; overload;
  end;

  TMainForm = class(TForm)
    Panel1: TPanel;
    btnAddComponent: TButton;
    OpenDialog1: TOpenDialog;
    mmoReadMe: TMemo;
    lblProjectTitle: TLabel;
    edtProjectTitle: TEdit;
    lblReadMe: TLabel;
    lblProjectGroupName: TLabel;
    edtProjectGroupName: TEdit;
    lblProjectGroupNameExt: TLabel;
    lblProjectHomepage: TLabel;
    edtProjectHomepage: TEdit;
    lblProjectDesc: TLabel;
    edtProjectDesc: TEdit;
    lblProjectVersion: TLabel;
    edtProjectVersion: TEdit;
    edtPalettePage: TLabel;
    Edit4: TEdit;
    lblComponents: TLabel;
    StringGrid1: TStringGrid;
    procedure btnAddComponentClick(Sender: TObject);
    procedure ExtractTemplateResourceZip;
    procedure ExtractTemplate;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    TemplateList: TList;
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
              MainForm.Log('Unhandled Exception in ExtractTemplateResourceZip');
              MainForm.Log('Class : ' + E.ClassName);
              MainForm.Log('Error : ' + E.Message);
            end;
        end;
      end;
  finally
    SS.Free;
    LInStream.Free;
    Result := Template;
  end;
end;


procedure TMainForm.Log(const AMsg: String);
begin
  mmoReadMe.Lines.Add(AMsg);
end;

procedure TMainForm.ExtractTemplateResourceZip;
var
  z: TZipFile;
  I, ZipCount: Int64;
  LResStream: TResourceStream;
  Template: TTemplateFile;
begin
  Application.ProcessMessages;

  try
    try
      LResStream := TResourceStream.Create(HInstance, 'Template', RT_RCDATA);
      z := TZipFile.Create;
      z.Open(LResStream, TZipMode.zmRead);


      ZipCount := Length(z.FileNames);
      Log('Files = ' + IntToStr(ZipCount));

      if Assigned(TemplateList) then
        FreeList;
      TemplateList := TList.Create;

      for I := 0 to ZipCount - 1 do
        begin
          Template := z.ExtractToTemplate(i);
          if not (Template = Nil) then
            begin
              TemplateList.Add(Template);
            end;
        end;

      z.Free;
    LResStream.Free;
    except
      on E: Exception do
        begin
          Log('Unhandled Exception in ExtractTemplateResourceZip');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end;
    end;
  finally
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
end;

procedure TMainForm.btnAddComponentClick(Sender: TObject);
begin
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

  OpenDialog1.Filter:='Component Project Files (*.cpf)|*.cpf';
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

end.
