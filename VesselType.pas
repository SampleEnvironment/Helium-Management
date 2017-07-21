unit VesselType;

interface

uses
  Classes, Forms, SysUtils, StdCtrls, Controls, HeliumDataMgmt;

type
  TVesselTypeForm = class(TForm)
    HeadLab:   TLabel;
    NameLab:   TLabel;
    NameCoBox: TComboBox;
    TypeLab:   TLabel;
    TypeEd:    TEdit;
    OkBtn:     TButton;
    CancelBtn: TButton;
    procedure OkBtnClick(Sender: TObject);
  private
    function    GetId: integer;
  public
    constructor CreateWithProperties(AVesId, ATypeId: integer; var ADataMgmt: TDataMgmt; AOwner: TComponent);
    destructor  Destroy; override;
  published
    property    Id: integer read GetId;
  end;

implementation

{$R *.dfm}

constructor TVesselTypeForm.CreateWithProperties(AVesId, ATypeId: integer; var ADataMgmt: TDataMgmt; AOwner: TComponent);
var i: integer;
begin
  inherited Create(AOwner);

  with ADataMgmt do
  try
    LockWhileWorkingWithList;

    for i := 0 to VesselList.Count - 1 do
      if (TVessel(VesselList[i]).ves_id = AVesId) then
      begin
        HeadLab.Caption := HeadLab.Caption + intToStr(TVessel(VesselList[i]).ves_name) + '.';
        TypeEd.Text     := TVessel(VesselList[i]).ves_type;
      end
      else if (TVessel(VesselList[i]).ves_type_id = ATypeId)
            and TVessel(VesselList[i]).ves_options_in_db
            then NameCoBox.AddItem(intToStr(TVessel(VesselList[i]).ves_name),
                                   TObject( TVessel(VesselList[i]).ves_id)); 
  finally
    UnlockAfterWorkingWithList;
  end;
end;

destructor TVesselTypeForm.Destroy;
begin
  
  inherited Destroy;
end;

function TVesselTypeForm.GetId: integer;
begin
  Result := -1;
  
  with NameCoBox do
   if (ItemIndex > -1) then Result := integer(Items.Objects[ItemIndex]);
end;

procedure TVesselTypeForm.OkBtnClick(Sender: TObject);
begin
  if (NameCoBox.ItemIndex > -1) then ModalResult := mrOk;
end;

end.
