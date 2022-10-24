unit MessageForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TfrmMessage = class(TForm)
    lblPrompt: TLabel;
    Panel1: TPanel;
    btnOK: TButton;
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMessage: TfrmMessage;

implementation

{$R *.fmx}

procedure TfrmMessage.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

end.
