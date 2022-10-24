unit ConfirmForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TfrmConfirm = class(TForm)
    lblPrompt: TLabel;
    Panel1: TPanel;
    btnYes: TButton;
    btnNo: TButton;
    procedure btnYesClick(Sender: TObject);
    procedure btnNoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmConfirm: TfrmConfirm;

implementation

{$R *.fmx}

procedure TfrmConfirm.btnNoClick(Sender: TObject);
begin
  ModalResult := mrNo;
end;

procedure TfrmConfirm.btnYesClick(Sender: TObject);
begin
  ModalResult := mrYes;
end;

end.
