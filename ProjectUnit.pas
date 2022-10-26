unit ProjectUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  System.Zip, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  FMX.Edit, System.Rtti, FMX.Grid.Style,
  FMX.Grid, FMX.Menus, FMX.Objects,
  Settings, FMX.Layouts;

type
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
    Rectangle2: TRectangle;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuOpenProject: TMenuItem;
    mnuSave: TMenuItem;
    mnuExit: TMenuItem;
    mnuResetProject: TMenuItem;
    mnuExportDisk: TMenuItem;
    SaveProjectDialog: TSaveDialog;
    Panel2: TPanel;
    rectComponent: TRectangle;
    Panel3: TPanel;
    Button1: TButton;
    cbIncludePackageInfo: TCheckBox;
    ContextMenu: TPopupMenu;
    mnuDeletePackage: TMenuItem;
    mnuOfflineMode: TMenuItem;
    mnuBug: TMenuItem;
    mnuAbout: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    mnuHelp: TMenuItem;
    mnuExportZip: TMenuItem;
    SaveExportZipDialog: TSaveDialog;
    StyleBook1: TStyleBook;
    ComponentGrid: TGridLayout;
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
    procedure mnuExportDiskClick(Sender: TObject);
    procedure cbIncludePackageInfoChange(Sender: TObject);
    procedure mnuDeletePackageClick(Sender: TObject);
    procedure mnuOfflineModeClick(Sender: TObject);
    procedure mnuBugClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure mnuHelpClick(Sender: TObject);
    procedure mnuExportZipClick(Sender: TObject);
  private
    { Private declarations }
    MarkedForDeletion: TCellImage;
    TemplateList: TList;
    ReplacementList: TList;
    procedure FreeTemplateList;
    procedure FreeReplacementList;
    procedure PopulateForm;
    function ExportCheck: Boolean;
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
  ComponentExporter,
  ComponentUnit,
  ConfirmForm,
  Math,
  MessageForm,
  OSBrowser,
  System.IOUtils,
  System.Json.Serializers;

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
  FImage.Parent := Self;
  FImage.ShowHint := True;

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

procedure TMainForm.mnuBugClick(Sender: TObject);
begin
  TOSBrowser.Open('https://github.com/peardox/MakeP4DComponents/issues');
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

procedure TMainForm.mnuResetProjectClick(Sender: TObject);
begin
  if Assigned(ProjectSettings) then
    FreeAndNil(ProjectSettings);
  ProjectSettings := TProjectSettings.Create;
  PopulateForm;
  FillComponentGrid;
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
            Log(IntToStr(I) + ' : ' + Token.tfile + ' : ' + IntToStr(Token.tmime));
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
        IconImage.Hint := NewComponent.DelphiPackageName;

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
  ComponentForm.Caption := 'Add New Package';
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
    ComponentForm.Caption := 'Edit Existing Package';
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

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin
  TOSBrowser.Open('https://peardox.com/delphi/');
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
begin
  TOSBrowser.Open('https://patreon.com/peardox');
end;

procedure TMainForm.MenuItem3Click(Sender: TObject);
begin
  TOSBrowser.Open('https://discord.gg/e2rGEEHrBd');
end;

procedure TMainForm.MenuItem4Click(Sender: TObject);
begin
  TOSBrowser.Open('https://www.paypal.com/donate/?hosted_button_id=XLCTBUMJRNQLL');
end;

procedure TMainForm.mnuOfflineModeClick(Sender: TObject);
begin
  SkipWebsiteChecks := not SkipWebsiteChecks;
  mnuOfflineMode.IsChecked  := SkipWebsiteChecks;
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

function TMainForm.ExportCheck: Boolean;
var
  ErrorState : String;
begin
  Result := True;

  ErrorState := String.Empty;

  if edtProjectTitle.Text = String.Empty then
    ErrorState := ErrorState + 'The Project Title is missing' + sLineBreak;
  if edtProjectVersion.Text = String.Empty then
    ErrorState := ErrorState + 'The Project Version is missing' + sLineBreak;
  if edtProjectGroupName.Text = String.Empty then
    ErrorState := ErrorState + 'The Project Group Name is missing' + sLineBreak;
  if edtProjectDesc.Text = String.Empty then
    ErrorState := ErrorState + 'The Project Description is missing' + sLineBreak;
  if edtPalettePage.Text = String.Empty then
    ErrorState := ErrorState + 'The Delphi Palette Page is missing' + sLineBreak;

  if ErrorState <> String.Empty then
    begin
      frmMessage.ModalResult := mrNone;
      frmMessage.lblPrompt.Text := ErrorState;
      frmMessage.ShowModal;
      Result := False;
    end;
end;

procedure TMainForm.mnuExportDiskClick(Sender: TObject);
var
  MR: TModalResult;
  Dir: String;
  WipeBeforeExport: Boolean;
  Exporter: TFileExporter;
  InitialDir: String;
begin
  Exporter := Nil; // Stupid 'might not have been initialized...'
  WipeBeforeExport := False;
  if ExportCheck then
    begin
    SaveExportZipDialog.Filename := ProjectSettings.ProjectGroupName + '.zip';

    InitialDir := TPath.GetSharedDocumentsPath;
    {$IFDEF PEARDOX_TESTING}
    InitialDir := 'D:\Temp\ExportTest';
    {$ENDIF}
    if SelectDirectory('Export Component Package as a Folder', InitialDir, Dir) then
      begin
        if DirectoryExists(Dir) then
          begin
            if not TDirectory.IsEmpty(Dir) then
              begin
                frmConfirm.ModalResult := mrNone;
                frmConfirm.lblPrompt.Text := Dir + ' is not empty' +
                  sLineBreak + sLineBreak + 'Do you want to erase it''s contents and' +
                  sLineBreak +              'replace it with your Component Package';
                MR := frmConfirm.ShowModal;
                if MR = mrYes then
                  begin
                    WipeBeforeExport := True;
                  end
                else
                  Exit;
              end;
            try
              try
                Exporter := TFileExporter.Create;
                Exporter.WipeBeforeExport := WipeBeforeExport;
                Exporter.Open(Dir);
                Exporter.Export(TemplateList, ReplacementList);
                Exporter.Close;
              except
                on E : Exception do
                  begin
                    Log('TMainForm.mnuExportDiskClick - Exception : Class = ' +
                      E.ClassName + ', Message = ' + E.Message);
                    Raise Exception.Create('TMainForm.mnuExportDiskClick - Exception : Class = ' +
                      E.ClassName + ', Message = ' + E.Message);
                  end;
              end;
            finally
              Exporter.Free;
            end;
          end;
      end;
    end;
end;

procedure TMainForm.mnuExportZipClick(Sender: TObject);
var
  MR: TModalResult;
  AllowOverWrite: Boolean;
  Exporter: TZipExporter;
begin
  Exporter := Nil; // Stupid 'might not have been initialized...'
  AllowOverWrite := False;
  if ExportCheck then
    begin
    SaveExportZipDialog.Filename := ProjectSettings.ProjectGroupName + '.zip';

    if SaveExportZipDialog.Execute then
      begin
        if FileExists(SaveExportZipDialog.Filename) then
          begin
            frmConfirm.ModalResult := mrNone;
            frmConfirm.lblPrompt.Text := SaveExportZipDialog.Filename + ' alreasy exists' +
              sLineBreak + sLineBreak + 'Do you want to overwrite the existing file?';
            MR := frmConfirm.ShowModal;
            if MR = mrYes then
              begin
                AllowOverWrite := True;
              end
            else
              Exit;
          end;
        try
          try
            Exporter := TZipExporter.Create;
            Exporter.AllowOverWrite := AllowOverWrite;
            Exporter.Open(SaveExportZipDialog.Filename);
            Exporter.Export(TemplateList, ReplacementList);
            Exporter.Close;
          except
            on E : Exception do
              begin
                Log('TMainForm.mnuExportZipClick - Exception : Class = ' +
                  E.ClassName + ', Message = ' + E.Message);
                Raise Exception.Create('TMainForm.mnuExportZipClick - Exception : Class = ' +
                  E.ClassName + ', Message = ' + E.Message);
              end;
          end;
        finally
          Exporter.Free;
        end;
      end;
    end;
end;

procedure TMainForm.mnuHelpClick(Sender: TObject);
begin
  TOSBrowser.Open('https://peardox.com/makep4dcomponents-help/');
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
  if Assigned(ComponentGrid) and Assigned(ComponentGrid.Children) then
    begin
      if ComponentGrid.Children.Count > 0 then
        begin
          for I := ComponentGrid.Children.Count - 1 downto 0 do
            begin
              ComponentGrid.Children[I].Free;
            end;
        end;
    end;

  if Length(ProjectSettings.ComponentSettings) > 0 then
    begin
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
            IconImage.Hint := ProjectSettings.ComponentSettings[I].DelphiPackageName;

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
  mnuExit.ShortCut := TextToShortcut('Alt+X');
  mnuFile.ShortCut := TextToShortcut('Alt+F');
  mnuExportDisk.ShortCut := TextToShortcut('F2');
  mnuExportZip.ShortCut := TextToShortcut('F3');
  mmoReadMe.WordWrap := True;

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

  SaveExportZipDialog.Filter:='Zip Archive (*.zip)|*zip';
  SaveExportZipDialog.DefaultExt := '.zip';
  SaveExportZipDialog.InitialDir := TPath.GetSharedDocumentsPath;
  SaveExportZipDialog.Title := 'Export Component Package as a Zip File';

  edtProjectTitle.TextPrompt := 'This text will appear as the title of the README.md';
  edtProjectVersion.TextPrompt := 'e.g. 1.0.0';
  edtProjectGroupName.TextPrompt := 'Your project''s main installation file';
  edtProjectDesc.TextPrompt := 'Short project description';
  edtProjectHomepage.TextPrompt := 'If you have one for it';
  edtPalettePage.TextPrompt := 'Python - My Components';

  LoadProjectSettings(IncludeTrailingPathDelimiter(AppHome) + DefaultProjectFile);

  ExtractTemplateResourceZip;
  ExtractReplacementResourceJson;

  PopulateForm;
  FillComponentGrid;

{
  var zz: TZipExporter;
  zz := TZipExporter.Create;
  zz.WipeBeforeExport := True;
  zz.Open('D:\Temp\ZipExport.zip');
  zz.WriteFile('fred.txt', 'abcdef');
  zz.WriteFile('bill/harry.txt', 'abcdef');
  zz.WriteBitmap('aimage.bmp', ProjectSettings.ComponentSettings[0].PackageIcon);
  zz.WriteBitmap('aimage.png', ProjectSettings.ComponentSettings[0].PackageIcon);
  zz.WriteBitmap('aimage.jpg', ProjectSettings.ComponentSettings[0].PackageIcon);
  zz.WriteBitmap('aimage-32.bmp', 32, ProjectSettings.ComponentSettings[0].PackageIcon);
  zz.WriteBitmap('aimage-32.png', 32, ProjectSettings.ComponentSettings[0].PackageIcon);
  zz.WriteBitmap('aimage-32.jpg', 32, ProjectSettings.ComponentSettings[0].PackageIcon);
  zz.Close;
  zz.Free;
  }
end;

end.
