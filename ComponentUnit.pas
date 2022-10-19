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
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
    Settings: TComponentSettings;
  public
    { Public declarations }
    procedure LoadDefaults;
  end;

var
  ComponentForm: TComponentForm;

implementation

{$R *.fmx}


procedure TComponentForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TComponentForm.btnSaveClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TComponentForm.LoadDefaults;
var
  Bitmap: TBitmap;
  LResStream: TResourceStream;
begin
  LResStream := TResourceStream.Create(HInstance, 'P4DIcon', RT_RCDATA);
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromStream(LResStream);
    imgIcon.Bitmap.Assign(Bitmap);
  finally
    Bitmap.Free;
    LResStream.Free;
  end;
end;

end.
