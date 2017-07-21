unit FrmODBCSelectDSN;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PlugNRollCtrls, ExtCtrls;

type
  TFormODBCSelectDSN = class(TForm)
    PanelBtns: TPanel;
    PanelMain: TPanel;
    LBDSNs: TListBox;
    Panel1: TPanel;
    FlatBtnCancel: TFlatBtn;
    FlatBtnOk: TFlatBtn;
    Label1: TLabel;
    LabCurrentDSN: TLabel;
    procedure FlatBtnOkClick(Sender: TObject);
    procedure FlatBtnCancelClick(Sender: TObject);
    procedure LBDSNsClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function Execute(var ASelected: string): Boolean;
  end;

var
  FormODBCSelectDSN: TFormODBCSelectDSN;

implementation

uses
  SQLAccessClasses;

{$R *.dfm}

{ TFormODBCSelectDSN }

function TFormODBCSelectDSN.Execute(var ASelected: string): Boolean;
var
  s: string;
begin
  Result := False;
  LabCurrentDSN.Caption := ASelected;
  SQLAccessClasses.GetDataSources(LBDSNs.Items);
  if LBDSNs.Items.Count > 0 then
  begin
    result := (ShowModal = mrOk) and (LBDSNs.ItemIndex > -1);
    if result then
    begin
      s := LBDSNs.Items[LBDSNs.ItemIndex];
      ASelected := copy(s, 1, pos('@', s) - 1);
    end;
  end
  else
    MessageDlg('ODBC(32Bit): No data sources found', mtError, [mbOk], 0);
end;

procedure TFormODBCSelectDSN.FlatBtnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormODBCSelectDSN.LBDSNsClick(Sender: TObject);
begin
  FlatBtnOk.Enabled := LBDSNs.ItemIndex > -1;
  if LBDSNs.ItemIndex > -1 then
    LabCurrentDSN.Caption := LBDSNs.Items[LBDSNs.ItemIndex];
end;

procedure TFormODBCSelectDSN.FlatBtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
