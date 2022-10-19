unit ProjectUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  System.Zip, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  PyCommon, PyModule, PyPackage, H5Py, FMX.Edit, System.Rtti, FMX.Grid.Style,
  FMX.Grid, FMX.Menus, FMX.Objects,
  Settings;

type
  TTemplateFile = Class
    TplFileName: String;
    TplTemplate: String;
    constructor Create(AFileName: string; ATemplate: String);
  End;

  TReplacementToken = Class
    tfile: String;
    xlat: TArray<String>;
    constructor Create(AFile: string; AToken: TArray<String>);
  end;
  TReplacementTokens = TArray<TReplacementToken>;

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
    lblPalettePage: TLabel;
    edtPalettePage: TEdit;
    lblComponents: TLabel;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    procedure btnAddComponentClick(Sender: TObject);
    procedure ExtractTemplateResourceZip;
    procedure ExtractReplacementResourceJson;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    TemplateList: TList;
    ReplacementList: TList;
    AppHome: String;
    Settings: TProjectSettings;
    procedure FreeTemplateList;
    procedure FreeReplacementList;
    procedure Log(const AMsg: String);
    procedure HaltAndCatchFire;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

const
  AppName = 'MakeP4DComponenets';

implementation

{$R *.fmx}
{$R EmbeddedResources.RES}

uses
  ComponentUnit,
  Math,
  System.Json,
  System.Json.Serializers,
  System.Json.Types,
  System.Json.Readers,
  System.IOUtils;

constructor TReplacementToken.Create(AFile: string; AToken: TArray<String>);
begin
  tfile := AFile;
  xlat := AToken;
end;

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

function TokenSortFunc(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TReplacementToken(Item1).tfile, TReplacementToken(Item2).tfile);
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

procedure TMainForm.ExtractReplacementResourceJson;
var
  LResStream: TResourceStream;
  SS: TStringStream;
  Tokens: TReplacementTokens;
  Token: TReplacementToken;
  JSONText: String;
  lSerializer: TJsonSerializer;
  I, J: Integer;
begin
  try
    LResStream := TResourceStream.Create(HInstance, 'ReplacementTokens', RT_RCDATA);
    lSerializer := TJsonSerializer.Create;
    SS := TStringStream.Create;
    SS.CopyFrom(LResStream , 0);

    JSONText := SS.DataString;
    try
      Tokens :=  lSerializer.Deserialize<TReplacementTokens>(JSONText);

      if Assigned(ReplacementList) then
        FreeReplacementList;
      ReplacementList := TList.Create;

      for I := 0 to Length(Tokens) - 1 do
        begin
          Token := Tokens[I];
          ReplacementList.Add(Token);
        end;

    except
      on E : Exception do
      begin
        ShowMessage('Unhandled Exception in ExtractReplacementResourceJson' + sLineBreak
          + 'Exception class name = '+E.ClassName + sLineBreak
          + 'Exception message = '+E.Message + sLineBreak
          + JSONText);
      end;
    end;
  finally
    if Assigned(TemplateList) then
      begin
        ReplacementList.Sort(@TokenSortFunc);
        Log('Replacemnts = ' + IntToStr(ReplacementList.Count));
        for I := 0 to ReplacementList.Count -1 do
          begin
            Token := ReplacementList[I];
            Log(IntToStr(I) + ' : ' + Token.tfile);
            for J := 0 to Length(Token.xlat) -1 do
              begin
                Log('  + ' + Token.xlat[J]);
              end;
          end;
      end;
  end;

  lSerializer.Free;
  SS.Free;
  LResStream.Free;

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

      if Assigned(TemplateList) then
        FreeTemplateList;
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
        ShowMessage('Unhandled Exception in ExtractTemplateResourceZip' + sLineBreak
          + 'Exception class name = '+E.ClassName + sLineBreak
          + 'Exception message = '+E.Message);
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
            Log(IntToStr(I) + ' : ' + Template.TplFileName);
          end;
      end;
  end;
end;

procedure TMainForm.HaltAndCatchFire;
begin
  ShowMessage('Something went horribly wrong');
  Application.Terminate;
end;

procedure TMainForm.btnAddComponentClick(Sender: TObject);
var
  mr: TModalResult;
begin
  ComponentForm.ModalResult := mrNone;
  ComponentForm.LoadDefaults;
  mr := ComponentForm.ShowModal;
  if mr = mrOK then
    begin

    end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult <> mrClose then
    ModalResult := mrCancel;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + AppName;
  if not DirectoryExists(AppHome) then
    begin
      ForceDirectories(AppHome);
    end;

  ExtractTemplateResourceZip;
  ExtractReplacementResourceJson;

  OpenDialog1.Filter:='Component Project Files (*.cpf)|*.cpf';
  OpenDialog1.InitialDir := AppHome;
end;

// Tidy up after ourselves
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeTemplateList;
  FreeReplacementList;
end;

procedure TMainForm.FreeTemplateList;
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

procedure TMainForm.FreeReplacementList;
var
  I: Integer;
  Token: TReplacementToken;
begin
    if Assigned(ReplacementList) then
      begin
        for I := 0 to ReplacementList.Count -1 do
          begin
            Token := ReplacementList[I];
            Token.Free;
          end;
        FreeAndNil(ReplacementList);
      end;
end;

end.
