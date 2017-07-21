unit Vessel;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls,
  Dialogs, HeliumDataMgmt, DatabaseEdit;

type
  TVesselForm = class(TForm)
    DeleteBtn: TButton;
    SaveBtn: TButton;
    CopyLab: TLabel;
    HintLab: TLabel;
    Comment2RiEd: TRichEdit;
    Comment2Lab: TLabel;
    AdcEd: TOraEdit;
    AdcLab: TLabel;
    MaxResEd: TOraEdit;
    MaxResLab: TLabel;
    LongIntEd: TOraEdit;
    LongIntLab: TLabel;
    Comment1RiEd: TRichEdit;
    Comment1Lab: TLabel;
    ZeroEd: TOraEdit;
    ZeroLab: TLabel;
    SpanEd: TOraEdit;
    SpanLab: TLabel;
    FilltimeEd: TOraEdit;
    FilltimeLab: TLabel;
    HeatEd: TOraEdit;
    HeatLab: TLabel;
    MinResEd: TOraEdit;
    MinResLab: TLabel;
    ShortIntEd: TOraEdit;
    ShortIntLab: TLabel;
    Panel2: TPanel;
    TareEd: TEdit;
    TareLab: TLabel;
    VolumeEd: TEdit;
    VolumeLab: TLabel;
    TypeEd: TEdit;
    TypeLab: TLabel;
    VesselCoBox: TComboBox;
    NameLab: TLabel;
    Panel1: TPanel;
    CancelBtn: TButton;
    procedure VesselCoBoxChange(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure CopyLabClick(Sender: TObject);
  private
    FDataMgmt:  TDataMgmt;
    FVessel:    TVessel;

    procedure   InitForm;
    procedure   AdoptSettings(AVesId: integer);
  public
    constructor CreateWithProperties(ADataMgmt: TDataMgmt; AOwner: TComponent);
    destructor  Destroy; override;
  end;

implementation
uses VesselType;
{$R *.dfm}

constructor TVesselForm.CreateWithProperties(ADataMgmt: TDataMgmt; AOwner: TComponent);
begin
  inherited Create(AOwner);

  HintLab.Caption := '';
  FVessel         := nil;
  FDataMgmt       := ADataMgmt;
  InitForm;
end;

destructor TVesselForm.Destroy;
begin
  if Assigned(FVessel) then FreeAndNil(FVessel);
  inherited Destroy;
end;


procedure TVesselForm.InitForm;
var i: integer;
begin
  if Assigned(FVessel) then FreeAndNil(FVessel);
  VesselCoBox.Clear;

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    RefreshVesListByDB;

    for i := 0 to VesselList.Count - 1
     do VesselCoBox.AddItem(intToStr(TVessel(VesselList[i]).ves_name),
                            TObject( TVessel(VesselList[i]).ves_id));
  finally
    UnlockAfterWorkingWithList;
  end;

  TypeEd.Text       := '';
  VolumeEd.Text     := '';
  TareEd.Text       := '';
  Comment1RiEd.Text := '';

  ShortIntEd.Text   := '';
  LongIntEd.Text    := '';
  MinResEd.Text     := '';
  MaxResEd.Text     := '';
  HeatEd.Text       := '';
  AdcEd.Text        := '';
  FilltimeEd.Text   := '';
  SpanEd.Text       := '';
  ZeroEd.Text       := '';
  Comment2RiEd.Text := '';

  copyLab.Caption   := '';

  ActiveControl     := VesselCoBox;
end;

procedure TVesselForm.VesselCoBoxChange(Sender: TObject);
var i: integer;
begin
  if Assigned(FVessel) then FreeAndNil(FVessel);
  DeleteBtn.Enabled := false;
  HintLab.Caption   := '';
  CopyLab.Caption   := '';

  if (VesselCoBox.ItemIndex > -1) then
    with FDataMgmt, VesselCoBox do
    try
      LockWhileWorkingWithList;
      FVessel := GetCopyOfVessel(GetVesIndex(integer(Items.Objects[ItemIndex])));

      if Assigned(FVessel) then
      begin
        TypeEd.Text         := FVessel.ves_type;
        VolumeEd.Text       := floatToStr(FVessel.ves_volume);
        TareEd.Text         := floatToStr(FVessel.ves_tare);
        Comment1RiEd.Text   := FVessel.ves_comment1;

        if FVessel.ves_options_in_db then
        begin
          ShortIntEd.Text   := intToStr(FVessel.ves_shortinterval);
          LongIntEd.Text    := intToStr(FVessel.ves_longinterval);
          MinResEd.Text     := floatToStr(FVessel.ves_minresistance);
          MaxResEd.Text     := floatToStr(FVessel.ves_maxresistance);
          HeatEd.Text       := intToStr(FVessel.ves_heattime);
          AdcEd.Text        := intToStr(FVessel.ves_adcloops);
          FilltimeEd.Text   := intToStr(FVessel.ves_filltimeout);
          SpanEd.Text       := intToStr(FVessel.ves_span);
          ZeroEd.Text       := intToStr(FVessel.ves_zero);
          Comment2RiEd.Text := FVessel.ves_comment2;

          DeleteBtn.Enabled := true;
        end
        else begin
          ShortIntEd.Text   := '';
          LongIntEd.Text    := '';
          MinResEd.Text     := '';
          MaxResEd.Text     := '';
          HeatEd.Text       := '';
          AdcEd.Text        := '';
          FilltimeEd.Text   := '';
          SpanEd.Text       := '';
          ZeroEd.Text       := '';
          Comment2RiEd.Text := '';
        end;

        for i := 0 to VesselList.Count - 1 do
         if not (FVessel = VesselList[i]) then
          if (TVessel(VesselList[i]).ves_type_id = FVessel.ves_type_id) then
           if TVessel(VesselList[i]).ves_options_in_db then
           begin
             CopyLab.Caption := 'Adopt settings of an other "'
                                + FVessel.ves_type + '" vessel.';
             break;
           end;
      end;
    finally
      UnlockAfterWorkingWithList;
    end;
end;


procedure TVesselForm.SaveBtnClick(Sender: TObject);
var i: integer;

  function securityQuery: boolean;
  begin
    Result := not FVessel.ves_options_in_db;
    if not Result
     then Result := (MessageDlg('Do you really want to change the settings of vessel ' + VesselCoBox.Text + '?', mtCustom, [mbYes, mbNo], 0) = mrYes);
  end;

begin
  if (VesselCoBox.ItemIndex > -1) and Assigned(FVessel) then
  begin
    if securityQuery()             then
      if (ShortIntEd.Text <> '')    then
       if (LongIntEd.Text  <> '')    then
        if (MinResEd.Text   <> '')    then
         if (MaxResEd.Text   <> '')    then
          if (HeatEd.Text     <> '')    then
           if (AdcEd.Text      <> '')    then
            if (FilltimeEd.Text <> '')    then
             if (SpanEd.Text     <> '')    then
              if (ZeroEd.Text     <> '')    then
              begin
                FVessel.ves_shortinterval := strToInt(ShortIntEd.Text);
                FVessel.ves_longinterval  := strToInt(LongIntEd.Text);
                FVessel.ves_minresistance := strToFloat(MinResEd.Text);
                FVessel.ves_maxresistance := strToFloat(MaxResEd.Text);
                FVessel.ves_heattime      := strToInt(HeatEd.Text);
                FVessel.ves_adcloops      := strToInt(AdcEd.Text);
                FVessel.ves_filltimeout   := strToInt(FilltimeEd.Text);
                FVessel.ves_span          := strToInt(SpanEd.Text);
                FVessel.ves_zero          := strToInt(ZeroEd.Text);
                FVessel.ves_comment2      := Comment2RiEd.Text;

                with FDataMgmt do
                try
                  LockWhileWorkingWithList;
                  if FVessel.ves_options_in_db then
                  begin
                    if UpdateVesOptionsToDb(FVessel)
                     then HintLab.Caption := 'Update on Vessel '
                                           + intToStr(FVessel.ves_name)
                                           + ' succeeded.';
                  end
                  else begin
                    if InsertVesOptionsToDb(FVessel)
                     then HintLab.Caption := 'Update on Vessel '
                                           + intToStr(FVessel.ves_name)
                                           + ' succeeded.';
                  end;

                  for i := 0 to ModuleList.Count - 1 do
                   if Assigned(TModule(ModuleList[i]).mod_vessel) then
                    if (TModule(ModuleList[i]).mod_vessel.ves_id = FVessel.ves_id) then
                    begin
                      TModule(ModuleList[i]).mod_setOptions := true;
                      break;
                    end;
                finally
                  UnlockAfterWorkingWithList;
                end;

                HintLab.Caption := 'Update on Vessel ' + intToStr(FVessel.ves_name) + ' succeeded.';
                InitForm;
              end
              else showMessage('Please enter a value for zero!')
             else showMessage('Please enter a value for span!')
            else showMessage('Please enter the fill timeout!')
           else showMessage('Please enter the adc loop!')
          else showMessage('Please enter the heat time!')
         else showMessage('Please enter the maximal resistance!')
        else showMessage('Please enter the minimal resistance!')
       else showMessage('Please enter the long interval!')
      else showMessage('Please enter the short interval!')
  end
  else showMessage('Select a vessel to edit!');
end;

procedure TVesselForm.DeleteBtnClick(Sender: TObject);
begin
  if (VesselCoBox.ItemIndex > -1) and Assigned(FVessel) then
  begin
    if (MessageDlg('Do you really want to delete the settings of vessel ' + VesselCoBox.Text + '?', mtCustom, [mbYes, mbNo], 0) = mrYes) then
    with FDataMgmt do
    try
      LockWhileWorkingWithList;
      if DeleteVesOptionsToDb(FVessel.ves_id)
       then HintLab.Caption := 'Settings (Vessel '
                             + intToStr(FVessel.ves_name)
                             + ') successfully deleted.';
    finally
      UnlockAfterWorkingWithList;
      InitForm;
    end;
  end
  else showMessage('Select a vessel to delete!');
end;


procedure TVesselForm.AdoptSettings(AVesId: Integer);
var LVessel: TVessel;
begin
  if (AVesId > 0) then   
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVessel := GetVesItem(AVesId);

    if Assigned(LVessel) then
    begin
      ShortIntEd.Text := intToStr(LVessel.ves_shortinterval);
      LongIntEd.Text  := intToStr(LVessel.ves_longinterval);
      MinResEd.Text   := floatToStr(LVessel.ves_minresistance);
      MaxResEd.Text   := floatToStr(LVessel.ves_maxresistance);
      HeatEd.Text     := intToStr(LVessel.ves_heattime);
      AdcEd.Text      := intToStr(LVessel.ves_adcloops);
      FilltimeEd.Text := intToStr(LVessel.ves_filltimeout);
      SpanEd.Text     := intToStr(LVessel.ves_span);
      ZeroEd.Text     := intToStr(LVessel.ves_zero);
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TVesselForm.CopyLabClick(Sender: TObject);
begin
  if (VesselCoBox.ItemIndex > -1) and Assigned(FVessel) then
    with TVesselTypeForm.CreateWithProperties(FVessel.ves_id,
                                              FVessel.ves_type_id,
                                              FDataMgmt,
                                              nil) do
    try
      ShowModal;
      if (ModalResult = mrOK) then
        AdoptSettings(Id);
    finally
      Free;
    end;
end;

end.
