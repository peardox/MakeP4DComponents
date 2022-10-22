unit ComponentUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Edit,
  Settings, REST.Types, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope;

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
    lblHomeURL: TLabel;
    edtHomeURL: TEdit;
    RESTClient: TRESTClient;
    RESTRequest: TRESTRequest;
    RESTResponse: TRESTResponse;
    btnHomeURL: TButton;
    btnPyPiURL: TButton;
    btnDocURL: TButton;
    btnGithubURL: TButton;
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
    procedure btnHomeURLClick(Sender: TObject);
    procedure btnPyPiURLClick(Sender: TObject);
    procedure btnDocURLClick(Sender: TObject);
    procedure btnGithubURLClick(Sender: TObject);
  private
    { Private declarations }
    PreLoad: Boolean;
    procedure UpdateEncodedIcon;
    procedure SetOpenIconDialogExtension(Sender: TObject);
    function CheckForWebsite(const AURL: String): Boolean;
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
  OSBrowser,
  System.NetEncoding;

// Cancel a bad form fill
procedure TComponentForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TComponentForm.btnHomeURLClick(Sender: TObject);
begin
  if ComponentSettings.HomeURL <> String.Empty then
    TOSBrowser.Open(ComponentSettings.HomeURL);
end;

procedure TComponentForm.btnPyPiURLClick(Sender: TObject);
begin
  if ComponentSettings.PyPiURL <> String.Empty then
    TOSBrowser.Open(ComponentSettings.PyPiURL);
end;

procedure TComponentForm.btnDocURLClick(Sender: TObject);
begin
  if ComponentSettings.DocURL <> String.Empty then
    TOSBrowser.Open(ComponentSettings.DocURL);
end;

procedure TComponentForm.btnGithubURLClick(Sender: TObject);
begin
  if ComponentSettings.GithubURL <> String.Empty then
    TOSBrowser.Open(ComponentSettings.GithubURL);
end;

// Only allow user to exit if they have filled in all required info
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

// Check if a website exists. This is important as edtDelphiPackageNameChange
// guesses at probable PyPi and ReadTheDocs addresses based on the package
// name which may be incorrect or missing from one of the trial URL
// possibilities
function TComponentForm.CheckForWebsite(const AURL: String): Boolean;
begin
  if SkipWebsiteChecks or PreLoad then
    begin
      Result := True;
      Exit;
    end;
  Result := False;

  // Skip if URL is blank
  if AURL.Trim <> String.Empty then
    begin
      Log('Checking for URL = ' + AURL);
      RESTClient.BaseURL := AURL;
      RESTRequest.Resource := '';
      RESTRequest.AcceptEncoding := 'gzip, deflate';
      try
        // Cheezy Synchronous Site Check
        RESTRequest.Execute;
        if RestResponse.StatusCode = 200 then
          begin
            Result := True;
            Log('Found URL = ' + AURL + ' - ' + IntToStr(RestResponse.ContentLength) + ' bytes');
          end;
      except
        // Most likely caused by screwed up URL
        on E: Exception do
          begin
            Log('Unhandled Exception in CheckForWebsite');
            Log('Class : ' + E.ClassName);
            Log('Error : ' + E.Message);
            Log('Passed URL = ' + AURL);
          end;
      end;
    end;
end;

// When entering the Delphi Class this is most likely going to be some Camel
// Case version of the package name. We therefore can fill in the likely
// entries the user would make in a lot of the fields that remain including
// the PyPi and ReadTheDocs URLs if they exist (which we'll check for anyway)
procedure TComponentForm.edtDelphiPackageNameChange(Sender: TObject);
var
  PossibleWebPage: String;
begin
  ComponentSettings.DelphiPackageName := edtDelphiPackageName.Text.Trim;

  if not Preload then
    begin
      if edtPublicPackageName.Text.Trim = String.Empty then
        edtPublicPackageName.Text := edtDelphiPackageName.Text.Trim.ToLower;
      if edtPythonPackageName.Text.Trim = String.Empty then
        edtPythonPackageName.Text := edtDelphiPackageName.Text.Trim.ToLower;
      if edtPIPPackageName.Text.Trim = String.Empty then
        edtPIPPackageName.Text := edtDelphiPackageName.Text.Trim.ToLower;
      if edtCondaPackageName.Text.Trim = String.Empty then
        edtCondaPackageName.Text := edtDelphiPackageName.Text.Trim.ToLower;
      if edtPyPiURL.Text.Trim = String.Empty then
        begin
          PossibleWebPage := 'https://pypi.org/project/' + edtDelphiPackageName.Text.Trim.ToLower + '/';
          if CheckForWebsite(PossibleWebPage) then
            begin
              edtPyPiURL.Text := PossibleWebPage;
              btnPyPiURL.Enabled := True;
            end
          else
            btnPyPiURL.Enabled := False;
        end;
      if edtDocURL.Text.Trim = String.Empty then
        begin
          PossibleWebPage := 'https://' + edtDelphiPackageName.Text.Trim.ToLower + '.readthedocs.io/';
          if CheckForWebsite(PossibleWebPage) then
            begin
              edtDocURL.Text := PossibleWebPage;
              btnDocURL.Enabled := True;
            end
          else
            btnDocURL.Enabled := False;
        end;
    end;
end;

procedure TComponentForm.edtPublicPackageNameChange(Sender: TObject);
begin
  ComponentSettings.PublicPackageName := edtPublicPackageName.Text.Trim;
end;

// Could also try a edtDelphiPackageNameChange auto-fill with this
// entry - seems like overkill and over-compilcating things.
procedure TComponentForm.edtPythonPackageNameChange(Sender: TObject);
begin
  ComponentSettings.PythonPackageName := edtPythonPackageName.Text.Trim;
end;

procedure TComponentForm.edtPIPPackageNameChange(Sender: TObject);
begin
  ComponentSettings.PIPPackageName := edtPIPPackageName.Text.Trim;
end;

procedure TComponentForm.edtCondaPackageNameChange(Sender: TObject);
begin
  ComponentSettings.CondaPackageName := edtCondaPackageName.Text.Trim;
end;

procedure TComponentForm.edtHomeURLChange(Sender: TObject);
begin
  ComponentSettings.HomeURL := edtHomeURL.Text.Trim;

  // User can enter bad URL but we'll turn off the ability to open webpage
  if CheckForWebsite(ComponentSettings.HomeURL) then
    btnHomeURL.Enabled := True
  else
    btnHomeURL.Enabled := False;
end;

procedure TComponentForm.edtPyPiURLChange(Sender: TObject);
begin
  ComponentSettings.PyPiURL := edtPyPiURL.Text.Trim;

  // User can enter bad URL but we'll turn off the ability to open webpage
  if CheckForWebsite(ComponentSettings.PyPiURL) then
    btnPyPiURL.Enabled := True
  else
    btnPyPiURL.Enabled := False;
end;

procedure TComponentForm.edtDocURLChange(Sender: TObject);
begin
  ComponentSettings.DocURL := edtDocURL.Text.Trim;

  // User can enter bad URL but we'll turn off the ability to open webpage
  if CheckForWebsite(ComponentSettings.DocURL) then
    btnDocURL.Enabled := True
  else
    btnDocURL.Enabled := False;
end;

procedure TComponentForm.edtGithubURLChange(Sender: TObject);
begin
  ComponentSettings.GithubURL := edtGithubURL.Text.Trim;

  // User can enter bad URL but we'll turn off the ability to open webpage
  if CheckForWebsite(ComponentSettings.GithubURL) then
    btnGithubURL.Enabled := True
  else
    btnGithubURL.Enabled := False;
end;

// Load the data with existing entries if present. If this is a new record
// then checking will occur when user starts filling it in so here we just
// fill in everything verbatim turning on webpage buttons if not blank as
// non-blank implies the page was previously checked and OK
procedure TComponentForm.PopulateForm;
begin
  // Preload : hack to prevents edtDelphiPackageNameChange from auto-completing on load
  Preload := True;

  btnHomeURL.Enabled := False;
  btnPyPiURL.Enabled := False;
  btnDocURL.Enabled := False;
  btnGithubURL.Enabled := False;

  edtDelphiPackageName.Text := ComponentSettings.DelphiPackageName.Trim;
  edtPublicPackageName.Text := ComponentSettings.PublicPackageName.Trim;
  edtPythonPackageName.Text := ComponentSettings.PythonPackageName.Trim;
  edtPIPPackageName.Text := ComponentSettings.PIPPackageName.Trim;
  edtCondaPackageName.Text := ComponentSettings.CondaPackageName.Trim;

  edtHomeURL.Text := ComponentSettings.HomeURL.Trim;
  if edtHomeURL.Text <> String.Empty then
    btnHomeURL.Enabled := True;

  edtPyPiURL.Text := ComponentSettings.PyPiURL.Trim;
  if edtPyPiURL.Text <> String.Empty then
    btnPyPiURL.Enabled := True;

  edtDocURL.Text := ComponentSettings.DocURL.Trim;
  if edtDocURL.Text <> String.Empty then
    btnDocURL.Enabled := True;

  edtGithubURL.Text := ComponentSettings.GithubURL.Trim;
  if edtGithubURL.Text <> String.Empty then
    btnGithubURL.Enabled := True;

  Preload := False;
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
  // Reformat labels adding second line with (Optional)
  procedure AddOptional(var ALabel: TLabel);
  begin
    ALabel.Text := ALabel.Text + sLineBreak + '(Optional)';
    ALabel.Height := ALabel.Height * 2;
    Alabel.TextSettings.VertAlign := TTextAlign.Leading;
  end;
begin
  // Initialize Flag to prevent auto-comletion in PopulateForm
  Preload := False;

  // Only set a short timeout for checking websites
  RestClient.ConnectTimeout := 750;

  // Set Open File Dialog properties
  OpenIconDialog.Filter:='Images (*.png; *jpg)|*.png; *jpg|PNG Images (*.png)|*.png|JPG Images (*.jpg)|*.jpg';
  OpenIconDialog.DefaultExt := '.png';
  OpenIconDialog.OnTypeChange := SetOpenIconDialogExtension;

  // Set Edit Boxes Placeholder Text
  edtDelphiPackageName.TextPrompt := 'The Delphi Class for this component';
  edtPublicPackageName.TextPrompt := 'The Delphi property for this component';
  edtPythonPackageName.TextPrompt := 'The generic name of this package';
  edtPIPPackageName.TextPrompt := 'The PIP name of this package';
  edtCondaPackageName.TextPrompt := 'The Conda name of this package';
  edtHomeURL.TextPrompt := 'The homepage of this package';
  edtPyPiURL.TextPrompt := 'The PyPi page for this package';
  edtDocURL.TextPrompt := 'The Documentation page for this package';
  edtGithubURL.TextPrompt := 'The Github page for this package';

  // Adjust Labels for Edit Boxes
  AddOptional(lblHomeURL);
  AddOptional(lblPyPiURL);
  AddOptional(lblDocURL);
  AddOptional(lblGithubURL);
end;

// If the user clicks on the icon and loads an image
// then load it and resize to 128x128. If user screws up
// leave icon alone
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

// Encode the icon as base64 so it can be stored in the P4D (Json really) file
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

// Load existing icon via base64 JSON save if present otherwise this is a
// new record and we'll default to the default Icon stored as a recource
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

