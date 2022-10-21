unit ComponentUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Edit,
  Settings;

type
  TComponentForm = class(TForm)
    Panel1: TPanel;
    btnCancel: TButton;
    btnSave: TButton;
    imgIcon: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblDelphiPackageName: TLabel;
    edtDelphiPackageName: TEdit;
    lblDelphiPublicName: TLabel;
    edtPublicPackageName: TEdit;
    edtPythonPackageName: TEdit;
    lblPythonPackageName: TLabel;
    edtPIPPackageName: TEdit;
    lblPIPPackageName: TLabel;
    edtCondaPackageName: TEdit;
    lblCondaPackageName: TLabel;
    lblType: TLabel;
    lblPyPiURL: TLabel;
    edtPyPiURL: TEdit;
    edtDocURL: TEdit;
    lblDocURL: TLabel;
    edtGithubURL: TEdit;
    lblGithubURL: TLabel;
    Label3: TLabel;
    OpenIconDialog: TOpenDialog;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblHomeURL: TLabel;
    edtHomeURL: TEdit;
    Label8: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure edtDelphiPackageNameChange(Sender: TObject);
    procedure edtPublicPackageNameChange(Sender: TObject);
    procedure edtPythonPackageNameChange(Sender: TObject);
    procedure edtPIPPackageNameChange(Sender: TObject);
    procedure edtCondaPackageNameChange(Sender: TObject);
    procedure edtPyPiURLChange(Sender: TObject);
    procedure edtDocURLChange(Sender: TObject);
    procedure edtGithubURLChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgIconClick(Sender: TObject);
    procedure edtHomeURLChange(Sender: TObject);
  private
    { Private declarations }
    PreLoad: Boolean;
    procedure UpdateEncodedIcon;
    procedure SetOpenIconDialogExtension(Sender: TObject);
  public
    { Public declarations }
    ComponentSettings: TComponentSettings;
    procedure LoadDefaultIcon;
    procedure PopulateForm;
  end;

var
  ComponentForm: TComponentForm;

implementation

{$R *.fmx}

uses
  System.NetEncoding;

procedure TComponentForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TComponentForm.btnSaveClick(Sender: TObject);
var
  ErrorState : String;
begin
  ErrorState := String.Empty;

  if edtDelphiPackageName.Text = String.Empty then
    ErrorState := ErrorState + 'The Delphi Package Name is missing' + sLineBreak;
  if edtPublicPackageName.Text = String.Empty then
    ErrorState := ErrorState + 'The Public Package Name is missing' + sLineBreak;
  if edtPythonPackageName.Text = String.Empty then
    ErrorState := ErrorState + 'The Python Package Name is missing' + sLineBreak;
  if edtPIPPackageName.Text = String.Empty then
    ErrorState := ErrorState + 'The PIP Package Name is missing' + sLineBreak;
  if edtCondaPackageName.Text = String.Empty then
    ErrorState := ErrorState + 'The Conda Package Name is missing' + sLineBreak;

  if ErrorState = String.Empty then
    ModalResult := mrOK
  else
    ShowMessage(ErrorState);
end;

procedure TComponentForm.edtCondaPackageNameChange(Sender: TObject);
begin
  ComponentSettings.CondaPackageName := edtCondaPackageName.Text;
end;

procedure TComponentForm.edtDelphiPackageNameChange(Sender: TObject);
begin
  ComponentSettings.DelphiPackageName := edtDelphiPackageName.Text;

  if not Preload then
    begin
      if edtPublicPackageName.Text = String.Empty then
        edtPublicPackageName.Text := edtDelphiPackageName.Text.ToLower;
      if edtPythonPackageName.Text = String.Empty then
        edtPythonPackageName.Text := edtDelphiPackageName.Text.ToLower;
      if edtPIPPackageName.Text = String.Empty then
        edtPIPPackageName.Text := edtDelphiPackageName.Text.ToLower;
      if edtCondaPackageName.Text = String.Empty then
        edtCondaPackageName.Text := edtDelphiPackageName.Text.ToLower;
      if edtPyPiURL.Text = String.Empty then
        edtPyPiURL.Text := 'https://pypi.org/project/' + edtDelphiPackageName.Text.ToLower + '/';
    end;
end;

procedure TComponentForm.edtDocURLChange(Sender: TObject);
begin
  ComponentSettings.DocURL := edtDocURL.Text;
end;

procedure TComponentForm.edtGithubURLChange(Sender: TObject);
begin
  ComponentSettings.GithubURL := edtGithubURL.Text;
end;

procedure TComponentForm.edtHomeURLChange(Sender: TObject);
begin
  ComponentSettings.HomeURL := edtHomeURL.Text;
end;

procedure TComponentForm.edtPIPPackageNameChange(Sender: TObject);
begin
  ComponentSettings.PIPPackageName := edtPIPPackageName.Text;
end;

procedure TComponentForm.edtPublicPackageNameChange(Sender: TObject);
begin
  ComponentSettings.PublicPackageName := edtPublicPackageName.Text;
end;

procedure TComponentForm.edtPyPiURLChange(Sender: TObject);
begin
  ComponentSettings.PyPiURL := edtPyPiURL.Text;
end;

procedure TComponentForm.PopulateForm;
begin
  Preload := True;
  edtDelphiPackageName.Text := ComponentSettings.DelphiPackageName;
  edtPublicPackageName.Text := ComponentSettings.PublicPackageName;
  edtPythonPackageName.Text := ComponentSettings.PythonPackageName;
  edtPIPPackageName.Text := ComponentSettings.PIPPackageName;
  edtCondaPackageName.Text := ComponentSettings.CondaPackageName;
  edtHomeURL.Text := ComponentSettings.HomeURL;
  edtPyPiURL.Text := ComponentSettings.PyPiURL;
  edtDocURL.Text := ComponentSettings.DocURL;
  edtGithubURL.Text := ComponentSettings.GithubURL;
  Preload := False;
end;

procedure TComponentForm.edtPythonPackageNameChange(Sender: TObject);
begin
  ComponentSettings.PythonPackageName := edtPythonPackageName.Text;
end;

procedure TComponentForm.SetOpenIconDialogExtension(Sender: TObject);
begin
  case (Sender as TSaveDialog).FilterIndex of
    0: (Sender as TSaveDialog).DefaultExt := '.png';
    1: (Sender as TSaveDialog).DefaultExt := '.png';
    2: (Sender as TSaveDialog).DefaultExt := '.jpg';
  end;
end;

procedure TComponentForm.FormCreate(Sender: TObject);
begin
  Preload := False;

  OpenIconDialog.Filter:='Images (*.png; *jpg)|*.png; *jpg|PNG Images (*.png)|*.png|JPG Images (*.jpg)|*.jpg';
  OpenIconDialog.DefaultExt := '.png';
  OpenIconDialog.OnTypeChange := SetOpenIconDialogExtension;

  edtDelphiPackageName.TextPrompt := 'The Delphi Class for this component';
  edtPublicPackageName.TextPrompt := 'The Delphi property for this component';
  edtPythonPackageName.TextPrompt := 'The generic name of this package';
  edtPIPPackageName.TextPrompt := 'The PIP name of this package';
  edtCondaPackageName.TextPrompt := 'The Conda name of this package';
  edtHomeURL.TextPrompt := 'The homepage of this package';
  edtPyPiURL.TextPrompt := 'The PyPi page for this package';
  edtDocURL.TextPrompt := 'The Documentation page for this package';
  edtGithubURL.TextPrompt := 'The Github page for this package';
end;

procedure TComponentForm.imgIconClick(Sender: TObject);
var
  LBitmap: TBitmap;
begin
  if OpenIconDialog.Execute then
    begin
      LBitmap := TBitmap.Create;
      try
        try
          LBitmap.LoadFromFile(OpenIconDialog.Filename);
          if (LBitmap.Width > 0) and (LBitmap.Height > 0) then
            begin
              if (LBitmap.Width <> 128) or (LBitmap.Height <> 128) then
                begin
                  LBitmap.Resize(128, 128);
                end;
              imgIcon.Bitmap.Assign(LBitmap);
              UpdateEncodedIcon;
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
        LBitmap.Free;
      end;
    end;
end;

procedure TComponentForm.UpdateEncodedIcon;
var
  MStream: TMemoryStream;
  SStream: TStringStream;
begin
  MStream := TMemoryStream.Create;
  SStream := TStringStream.Create;
  try
    imgIcon.Bitmap.SaveToStream(MStream);
    MStream.Position := 0;
    TNetEncoding.Base64.Encode(MStream, SStream);
    ComponentSettings.PackageIcon := SStream.DataString;
  finally
    SStream.Free;
    MStream.Free;
  end;
end;

procedure TComponentForm.LoadDefaultIcon;
var
  LBitmap: TBitmap;
  LResStream: TResourceStream;
begin
  LBitmap := TBitmap.Create;
  try
    if ComponentSettings.PackageIcon = String.Empty then
      begin
        LResStream := TResourceStream.Create(HInstance, 'P4DIcon', RT_RCDATA);
        try
          LBitmap.LoadFromStream(LResStream);
          if Assigned(LBitmap) then
            begin
              imgIcon.Bitmap.Assign(LBitmap);
              UpdateEncodedIcon;
            end;
        finally
          LResStream.Free;
        end;
      end
    else
      begin
        DecodeBase64Image(LBitmap, ComponentSettings.PackageIcon);
        if Assigned(LBitmap) then
          imgIcon.Bitmap.Assign(LBitmap);
      end;
  finally
    FreeAndNil(LBitmap);
  end;
end;

end.
