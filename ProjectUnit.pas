unit ProjectUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  System.Zip, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  PyCommon, PyModule, PyPackage, H5Py, FMX.Edit, System.Rtti, FMX.Grid.Style,
  FMX.Grid, FMX.Menus, FMX.Objects,
  Settings, FMX.Layouts;

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

  TCellImage = Class(TLayout)
  private
    FComponentSettings: TComponentSettings;
    FImage: TImage;
    FCaption: TLabel;
    procedure CellEditComponentClick(Sender: TObject);
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Image: TImage Read FImage Write FImage Default Nil;
    property Caption: TLabel Read FCaption Write FCaption Default Nil;
    property ComponentSettings: TComponentSettings Read FComponentSettings Write FComponentSettings Default Nil;
  End;

  TMainForm = class(TForm)
    OpenProjectDialog: TOpenDialog;
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
    Rectangle2: TRectangle;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuOpenProject: TMenuItem;
    mnuSave: TMenuItem;
    mnuExit: TMenuItem;
    mnuResetProject: TMenuItem;
    mnuExport: TMenuItem;
    SaveProjectDialog: TSaveDialog;
    Panel2: TPanel;
    rectComponent: TRectangle;
    VertScrollBox1: TVertScrollBox;
    Panel3: TPanel;
    Button1: TButton;
    ComponentGrid: TGridLayout;
    cbIncludePackageInfo: TCheckBox;
    ContextMenu: TPopupMenu;
    mnuDeletePackage: TMenuItem;
    mnuSkipWebsiteChecks: TMenuItem;
    procedure btnAddComponentClick(Sender: TObject);
    procedure ExtractTemplateResourceZip;
    procedure ExtractReplacementResourceJson;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure edtProjectTitleChange(Sender: TObject);
    procedure edtProjectVersionChange(Sender: TObject);
    procedure mmoReadMeChange(Sender: TObject);
    procedure edtProjectGroupNameChange(Sender: TObject);
    procedure edtProjectDescChange(Sender: TObject);
    procedure edtProjectHomepageChange(Sender: TObject);
    procedure edtPalettePageChange(Sender: TObject);
    procedure mnuResetProjectClick(Sender: TObject);
    procedure mnuOpenProjectClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuExportClick(Sender: TObject);
    procedure cbIncludePackageInfoChange(Sender: TObject);
    procedure mnuDeletePackageClick(Sender: TObject);
    procedure mnuSkipWebsiteChecksClick(Sender: TObject);
  private
    { Private declarations }
    MarkedForDeletion: TCellImage;
    TemplateList: TList;
    ReplacementList: TList;
    procedure FreeTemplateList;
    procedure FreeReplacementList;
    procedure PopulateForm;
    function EditPythonComponent(AComponent: TComponentSettings): Boolean;
    function AddPythonComponent(AComponent: TComponentSettings): Boolean;
    procedure FillComponentGrid;
    procedure DeleteCellImage;
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
  System.Json.Serializers,
  System.IOUtils;

constructor TCellImage.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

  Align := TAlignLayout.Client;

  Margins.Top := 8;
  Margins.Left := 8;
  Margins.Bottom := 8;
  Margins.Right := 8;

  FComponentSettings := Nil;

  FImage := TImage.Create(Self);
  FImage.Position.Y := 20;
  FImage.Align := TAlignLayout.Bottom;
  FImage.OnMouseDown := HandleMouseDown;
//  FImage.PopupMenu := Mainform.ContextMenu;
  FImage.Parent := Self;

  FCaption := TLabel.Create(Self);
  FCaption.TextSettings.HorzAlign := TTextAlign.Center;
  FCaption.Parent := Self;
end;

destructor TCellImage.Destroy;
begin
  FComponentSettings := Nil;
  FImage.Free;
  FCaption.Free;
  Inherited Destroy;
end;

// Completely paranoid removal procedure
procedure TMainForm.DeleteCellImage;
var
  I: Integer;
  IDX: Integer;
  CellImage: TCellImage;
begin
  IDX := -1;
  CellImage := Nil;

  if Length(ProjectSettings.ComponentSettings) > 0 then
      if Assigned(ComponentGrid.Children) then
          if ComponentGrid.Children.Count > 0 then
              for I := 0 to ComponentGrid.Children.Count - 1 do
                begin
                  if ComponentGrid.Children[I] is TCellImage then
                    if TCellImage(ComponentGrid.Children[I]) = MarkedForDeletion then
                      begin
                        IDX := I;
                        CellImage := TCellImage(ComponentGrid.Children[I]);
                        Break;
                      end;
                end;

  if (IDX <> -1) and (CellImage = MarkedForDeletion) then
    begin
      ComponentGrid.Children[IDX].Free;
      ProjectSettings.ComponentSettings[IDX].Free;
      Delete(ProjectSettings.ComponentSettings, IDX, 1);
    end;

  MarkedForDeletion := Nil;
end;

procedure TMainForm.mnuDeletePackageClick(Sender: TObject);
begin
  DeleteCellImage;
end;

procedure TCellImage.HandleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  PopLoc: TPointF;
begin
  if Button = TMouseButton.mbRight then
    begin
      PopLoc := Screen.MousePos;
      MainForm.MarkedForDeletion := Self;
      Mainform.ContextMenu.Popup(PopLoc.X, PopLoc.Y);
    end
  else if Button = TMouseButton.mbLeft then
    begin
      CellEditComponentClick(Sender);
    end;
end;

procedure TCellImage.CellEditComponentClick(Sender: TObject);
var
  ExistingComponent: TComponentSettings;
  ClickedIcon: TImage;
  LBitmap: TBitmap;
begin

  ClickedIcon := TImage(Sender);
  if Assigned(ClickedIcon) then
    begin
      ExistingComponent := ComponentSettings;
      if MainForm.EditPythonComponent(ExistingComponent) then
        begin
          LBitmap := TBitmap.Create;
          try
            DecodeBase64Image(LBitmap, ExistingComponent.PackageIcon);
            ClickedIcon.Bitmap.Assign(LBitmap);
            Caption.Text := ExistingComponent.DelphiPackageName;
          finally
            LBitmap.Free;
          end;
        end;
    end;
end;

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

procedure TMainForm.mnuResetProjectClick(Sender: TObject);
begin
  if Assigned(ProjectSettings) then
    FreeAndNil(ProjectSettings);
  ProjectSettings := TProjectSettings.Create;
  PopulateForm;
end;

procedure TMainForm.mnuSaveClick(Sender: TObject);
begin
  if SaveProjectDialog.Execute then
    begin
      SaveProjectSettings(SaveProjectDialog.Filename);
    end;
end;

procedure TMainForm.edtPalettePageChange(Sender: TObject);
begin
  ProjectSettings.PalettePage := edtPalettePage.Text;
end;

procedure TMainForm.edtProjectDescChange(Sender: TObject);
begin
  ProjectSettings.ProjectDesc := edtProjectDesc.Text;
end;

procedure TMainForm.edtProjectGroupNameChange(Sender: TObject);
begin
  ProjectSettings.ProjectGroupName := edtProjectGroupName.Text;
end;

procedure TMainForm.edtProjectHomepageChange(Sender: TObject);
begin
  ProjectSettings.ProjectHomepage := edtProjectHomepage.Text;
end;

procedure TMainForm.edtProjectTitleChange(Sender: TObject);
begin
  ProjectSettings.ProjectTitle := edtProjectTitle.Text;
end;

procedure TMainForm.edtProjectVersionChange(Sender: TObject);
begin
  ProjectSettings.ProjectVersion := edtProjectVersion.Text;
end;

procedure TMainForm.mmoReadMeChange(Sender: TObject);
begin
  ProjectSettings.ReadMe := mmoReadMe.Text;
end;

procedure TMainForm.cbIncludePackageInfoChange(Sender: TObject);
begin
  ProjectSettings.IncludePackageInfo := cbIncludePackageInfo.IsChecked;
end;

procedure TMainForm.PopulateForm;
begin
  edtProjectTitle.Text := ProjectSettings.ProjectTitle;
  edtProjectVersion.Text := ProjectSettings.ProjectVersion;
  mmoReadMe.Text := ProjectSettings.ReadMe;
  cbIncludePackageInfo.IsChecked := ProjectSettings.IncludePackageInfo;
  edtProjectGroupName.Text := ProjectSettings.ProjectGroupName;
  edtProjectDesc.Text := ProjectSettings.ProjectDesc;
  edtProjectHomepage.Text := ProjectSettings.ProjectHomepage;
  edtPalettePage.Text := ProjectSettings.PalettePage;
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

procedure TMainForm.btnAddComponentClick(Sender: TObject);
var
  NewComponent: TComponentSettings;
  LBitmap: TBitmap;
  IconImage: TImage;
  CellImage: TCellImage;
begin
  NewComponent := TComponentSettings.Create;
  if (AddPythonComponent(NewComponent)) then
    begin
      LBitmap := TBitmap.Create;
      try
        // Needs Refactoring
        CellImage := TCellImage.Create(ComponentGrid);
        CellImage.ComponentSettings := NewComponent;
        IconImage := CellImage.Image;
        IconImage.Width := 128;
        IconImage.Height := 128;
        DecodeBase64Image(LBitmap, NewComponent.PackageIcon);
        IconImage.Bitmap.Assign(LBitmap);
        CellImage.Caption.Text := NewComponent.DelphiPackageName;
        ComponentGrid.AddObject(CellImage);
      finally
        LBitmap.Free;
      end;
    end;
end;

function TMainForm.AddPythonComponent(AComponent: TComponentSettings): Boolean;
var
  MR: TModalResult;
  Idx: Integer;
begin
  Result := False;
  ComponentForm.ModalResult := mrNone;
  ComponentForm.ComponentSettings := AComponent;
  ComponentForm.LoadDefaultIcon;
  ComponentForm.PopulateForm;
  MR := ComponentForm.ShowModal;
  if MR = mrOK then
    begin
      Idx := Length(ProjectSettings.ComponentSettings);
      SetLength(ProjectSettings.ComponentSettings, Idx + 1);
      ProjectSettings.ComponentSettings[Idx] := AComponent;
      Result := True;
    end
  else
    FreeAndNil(AComponent);
end;

// Returns true if Icon or Delphi Class name changed
function TMainForm.EditPythonComponent(AComponent: TComponentSettings): Boolean;
var
  CopyComponent: TComponentSettings;
  MR: TModalResult;
begin
  Result := False;
  CopyComponent := TComponentSettings.Create;
  try
    ComponentForm.ModalResult := mrNone;
    CopyComponent.CopyFrom(AComponent);
    ComponentForm.ComponentSettings := CopyComponent;
    ComponentForm.LoadDefaultIcon;
    ComponentForm.PopulateForm;
    MR := ComponentForm.ShowModal;
    if MR = mrOK then
      begin
        if (AComponent.PackageIcon <> CopyComponent.PackageIcon) or
           (AComponent.DelphiPackageName <> CopyComponent.DelphiPackageName) then
          begin
            Result := True;
          end;
        AComponent.CopyFrom(CopyComponent);
      end;
  finally
    CopyComponent.Free;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult <> mrClose then
    ModalResult := mrCancel;
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

procedure TMainForm.mnuSkipWebsiteChecksClick(Sender: TObject);
begin
  SkipWebsiteChecks := not SkipWebsiteChecks;
  mnuSkipWebsiteChecks.IsChecked  := SkipWebsiteChecks;
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

procedure TMainForm.mnuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.mnuExportClick(Sender: TObject);
begin
  // Export
end;

procedure TMainForm.mnuOpenProjectClick(Sender: TObject);
begin
  if OpenProjectDialog.Execute then
    begin
      LoadProjectSettings(OpenProjectDialog.Filename);
      PopulateForm;
      FillComponentGrid;
    end;
end;

procedure TMainForm.FillComponentGrid;
var
  I: Integer;
  IconImage: TImage;
  LBitmap: TBitmap;
  CellImage: TCellImage;
begin
  if Length(ProjectSettings.ComponentSettings) > 0 then
    begin

      if Assigned(ComponentGrid.Children) then
        begin
          if ComponentGrid.Children.Count > 0 then
            begin
              for I := ComponentGrid.Children.Count - 1 downto 0 do
                begin
                  ComponentGrid.Children[I].Free;
                end;
            end;
        end;

      LBitmap := TBitmap.Create;
      try
        for I := 0 to Length(ProjectSettings.ComponentSettings) - 1 do
          begin
            // Needs Refactoring
            CellImage := TCellImage.Create(ComponentGrid);
            CellImage.ComponentSettings := ProjectSettings.ComponentSettings[I];

            IconImage := CellImage.Image;
            IconImage.Width := 128;
            IconImage.Height := 128;

            DecodeBase64Image(LBitmap, ProjectSettings.ComponentSettings[I].PackageIcon);
            IconImage.Bitmap.Assign(LBitmap);
            CellImage.Caption.Text := ProjectSettings.ComponentSettings[I].DelphiPackageName;

            ComponentGrid.AddObject(CellImage);
          end;
      finally
        LBitmap.Free;
      end;
    end;
end;

// Tidy up after ourselves
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeTemplateList;
  FreeReplacementList;
  if Assigned(ProjectSettings) then
    begin
      SaveProjectSettings(IncludeTrailingPathDelimiter(AppHome) + DefaultProjectFile);
      FreeAndNil(ProjectSettings);
    end;
  LogStrings.SaveToFile(IncludeTrailingPathDelimiter(AppHome) + 'Debug.log');
  LogStrings.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LogStrings := TStringList.Create;;
  SkipWebsiteChecks := False;

  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + AppName;
  if not DirectoryExists(AppHome) then
    begin
      ForceDirectories(AppHome);
    end;

  OpenProjectDialog.Filter:='P4D Project Files (*.p4d)|*.p4d';
  OpenProjectDialog.DefaultExt := '.p4d';
  OpenProjectDialog.InitialDir := AppHome;

  SaveProjectDialog.Filter:='P4D Project Files (*.p4d)|*.p4d';
  SaveProjectDialog.DefaultExt := '.p4d';
  SaveProjectDialog.InitialDir := AppHome;

  edtProjectTitle.TextPrompt := 'This text will appear as the title of the README.md';
  edtProjectVersion.TextPrompt := 'e.g. 1.0.0';
//  mmoReadMe.TextPrompt := '';
  edtProjectGroupName.TextPrompt := 'Your project''s main installation file';
  edtProjectDesc.TextPrompt := 'Short project description';
  edtProjectHomepage.TextPrompt := 'If you have one for it';
  edtPalettePage.TextPrompt := 'Python - My Components';

  LoadProjectSettings(IncludeTrailingPathDelimiter(AppHome) + DefaultProjectFile);

  ExtractTemplateResourceZip;
  ExtractReplacementResourceJson;

  PopulateForm;
  FillComponentGrid;
end;

end.
