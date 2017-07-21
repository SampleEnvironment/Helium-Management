unit TextMsg;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls;

type
  TTextMsgForm = class(TForm)
    textLab: TLabel;
    sendBtn: TButton;
    typeLab: TLabel;
    typeCoBox: TComboBox;
    CancelBtn: TButton;
    textEd: TEdit;
    procedure sendBtnClick(Sender: TObject);
  private
    function GetText: string;
  public
    constructor CreateWithProperties(AOwner: TComponent);
    destructor Destroy; override;
  published
    property Text: string read GetText;
  end;

implementation

{$R *.dfm}

constructor TTextMsgForm.CreateWithProperties(AOwner: TComponent);
begin
  create(AOwner);

  typeCoBox.Items.Add('NOTICE');  // Index 0 :: Type 1
  typeCoBox.Items.Add('WARNING'); // Index 1 :: Type 2
  typeCoBox.Items.Add('ERROR');   // Index 2 :: Type 3
  typeCoBox.ItemIndex := 0;
end;

destructor TTextMsgForm.Destroy;
begin

  inherited Destroy;
end;

function TTextMsgForm.GetText: string;
begin
  case typeCoBox.ItemIndex of
   0 : Result := #1 + textEd.Text;
   1 : Result := #2 + textEd.Text;
   2 : Result := #3 + textEd.Text;
  else Result := #1 + textEd.Text;
  end;
end;

procedure TTextMsgForm.sendBtnClick(Sender: TObject);
begin
  if (TextEd.Text = '') then
    MessageDlg('You can not send an empty string!', mtWarning, [mbOK], 0)
  else
    ModalResult := mrOk;
end;

end.
