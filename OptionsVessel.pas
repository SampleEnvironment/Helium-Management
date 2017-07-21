unit OptionsVessel;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Registry,
  HeliumDataMgmt;

type
  TOptionsForm = class(TForm)
    AcceptBtn: TButton;
    DiscardBtn: TButton;
    CancelBtn: TButton;
    cShortIntLab: TLabel;
    cShortIntEd: TEdit;
    cLongIntLab: TLabel;
    cLongIntEd: TEdit;
    cMaxResLab: TLabel;
    cMaxResEd: TEdit;
    cMinResLab: TLabel;
    cMinResEd: TEdit;
    cHeatLab: TLabel;
    cHeatEd: TEdit;
    cAdcLab: TLabel;
    cAdcEd: TEdit;
    cFillTimeLab: TLabel;
    cFillTimeEd: TEdit;
    uShortIntEd: TEdit;
    uLongIntEd: TEdit;
    uMinResEd: TEdit;
    uMaxResEd: TEdit;
    uHeatEd: TEdit;
    uAdcEd: TEdit;
    uFillTimeEd: TEdit;
    shortIntChBox: TCheckBox;
    longIntChBox: TCheckBox;
    minResChBox: TCheckBox;
    maxResChBox: TCheckBox;
    heatChBox: TCheckBox;
    adcChBox: TCheckBox;
    filltimeChBox: TCheckBox;
    curLab: TLabel;
    newLab: TLabel;
    typeLab: TLabel;
    typeEd: TEdit;
    tareLab: TLabel;
    tareEd: TEdit;
    volumeEd: TEdit;
    volumeLab: TLabel;
    nameCoBox: TComboBox;
    dNameLab: TLabel;
    cSpanLab: TLabel;
    cSpanEd: TEdit;
    uSpanEd: TEdit;
    spanChBox: TCheckBox;
    cZeroLab: TLabel;
    cZeroEd: TEdit;
    uZeroEd: TEdit;
    zeroChBox: TCheckBox;
    procedure DiscardBtnClick(Sender: TObject);
    procedure nameCoBoxChange(Sender: TObject);
    procedure AcceptBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    FDataMgmt: TDataMgmt;
    id: integer;
    procedure getNameList;
    procedure clearAll;
    function lastItem: boolean;
    function fillParams: boolean;
   public
    constructor CreateWithProperties(ADataMgmt: TDataMgmt; AOwner: TComponent);
  end;

implementation
uses HeliumFunctions;
{$R *.dfm}

constructor TOptionsForm.CreateWithProperties(ADataMgmt: TDataMgmt; AOwner: TComponent);
begin
  create(AOwner);
  FDataMgmt := ADataMgmt; 
  getNameList;
  if (nameCoBox.Items.Count = 0) then
    MessageDlg('There are no modified Vessel Options!', mtInformation, [mbOK], 0);
end;

procedure TOptionsForm.getNameList;
var LReg: TRegistry;
begin
  nameCoBox.Clear;
  nameCoBox.Text := 'select ...';
  LReg         := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  if LReg.KeyExists('Software\HZB_Helium\New_VesOptions\') then
  try  LReg.OpenKey('Software\HZB_Helium\New_VesOptions\', false);
       LReg.GetKeyNames(nameCoBox.Items);
  finally LReg.CloseKey;
  end;
  LReg.Free;
end;

procedure TOptionsForm.clearAll;
begin
  id := 0;

  typeEd.Text     := '';
  tareEd.Text     := '';
  volumeEd.Text   := '';

  cShortIntEd.Text := '';
  cLongIntEd.Text  := '';
  cMaxResEd.Text   := '';
  cMinResEd.Text   := '';
  cHeatEd.Text     := '';
  cAdcEd.Text      := '';
  cFillTimeEd.Text := '';
  cSpanEd.Text     := '';
  cZeroEd.Text     := '';

  uShortIntEd.Text := '';
  uLongIntEd.Text  := '';
  uMinResEd.Text   := '';
  uMaxResEd.Text   := '';
  uHeatEd.Text     := '';
  uAdcEd.Text      := '';
  uFillTimeEd.Text := '';
  uSpanEd.Text     := '';
  uZeroEd.Text     := '';

  shortIntChBox.Checked := false;
  longIntChBox.Checked  := false;
  minResChBox.Checked   := false;
  maxResChBox.Checked   := false;
  heatChBox.Checked     := false;
  adcChBox.Checked      := false;
  filltimeChBox.Checked := false;
  spanChBox.Checked     := false;
  zeroChBox.Checked     := false;
end;

function TOPtionsForm.lastItem: boolean;
begin
  Result := (nameCoBox.Items.Count = 0);
  if Result then ModalResult := mrOk;
end;

function TOptionsForm.fillParams: boolean;
var LReg:    TRegistry;
    LVessel: TVessel;
begin
  Result := false;

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVessel := GetVesItemByName(strToInt(nameCoBox.Text));

    if Assigned(LVessel) then
    begin
      id               := LVessel.ves_id;
      typeEd.Text      := LVessel.ves_type;
      tareEd.Text      := floatToStr(LVessel.ves_tare);
      volumeEd.Text    := floatToStr(LVessel.ves_volume);

      cShortIntEd.Text := intToStr(LVessel.ves_shortinterval);
      cLongIntEd.Text  := intToStr(LVessel.ves_longinterval);
      cMinResEd.Text   := floatToStr(LVessel.ves_minresistance);
      cMaxResEd.Text   := floatToStr(LVessel.ves_maxresistance);
      cHeatEd.Text     := intToStr(LVessel.ves_heattime);
      cAdcEd.Text      := intToStr(LVessel.ves_adcloops);
      cFillTimeEd.Text := intToStr(LVessel.ves_filltimeout);
      cSpanEd.Text     := intToStr(LVessel.ves_span);
      cZeroEd.Text     := intToStr(LVessel.ves_zero);
    end;
  finally
    UnlockAfterWorkingWithList;
  end;

  if (id > 0) then
  try
    LReg         := TRegistry.Create;
    LReg.RootKey := HKEY_CURRENT_USER;
    if LReg.KeyExists('Software\HZB_Helium\New_VesOptions\' + nameCoBox.Text + '\') then
    try
      LReg.OpenKey('Software\HZB_Helium\New_VesOptions\' + nameCoBox.Text + '\', false);


// Short Interval
      if LReg.ValueExists('interval_short') then
      begin
        uShortIntEd.Text := intToStr(LReg.ReadInteger('interval_short'));
        ShortIntChBox.Checked := true;
        Result := true;
      end;

// Long Interval
      if LReg.ValueExists('interval_long') then
      begin
        uLongIntEd.Text := intToStr(LReg.ReadInteger('interval_long'));
        LongIntChBox.Checked := true;
        Result := true;
      end;

// Min Resistance
      if LReg.ValueExists('resistance_min') then
      begin
        uMinResEd.Text := floatToStr(LReg.ReadFloat('resistance_min'));
        MinResChBox.Checked := true;
        Result := true;
      end;

// Max Resistance
      if LReg.ValueExists('resistance_max') then
      begin
        uMaxResEd.Text := floatToStr(LReg.ReadFloat('resistance_max'));
        MaxResChBox.Checked := true;
        Result := true;
      end;

// Heattime
      if LReg.ValueExists('heattime') then
      begin
        uHeatEd.Text := intToStr(LReg.ReadInteger('heattime'));
        HeatChBox.Checked := true;
        Result := true;
      end;

// ADC Loops
      if LReg.ValueExists('adc_cycles') then
      begin
        uAdcEd.Text := intToStr(LReg.ReadInteger('adc_cycles'));
        AdcChBox.Checked := true;
        Result := true;
      end;

// Fill Timeout
      if LReg.ValueExists('fill_timeout') then
      begin
        uFillTimeEd.Text := intToStr(LReg.ReadInteger('fill_timeout'));
        FillTimeChBox.Checked := true;
        Result := true;
      end;

// Span
      if LReg.ValueExists('span') then
      begin
        uSpanEd.Text := intToStr(LReg.ReadInteger('span'));
        spanChBox.Checked := true;
        Result := true;
      end;

// Zero
      if LReg.ValueExists('zero') then
      begin
        uZeroEd.Text := intToStr(LReg.ReadInteger('zero'));
        zeroChBox.Checked := true;
        Result := true;
      end;                                       
    finally LReg.CloseKey;
    end;
    LReg.Free;
  finally
  end;
end;

procedure TOptionsForm.nameCoBoxChange(Sender: TObject);
begin
  if (nameCoBox.ItemIndex > -1) then
  begin
    clearAll;
    if not fillParams then
      MessageDlg('No Parameters changed on this Vessel!', mtInformation, [mbOK], 0);
  end
  else
    nameCoBox.Text := 'select ...';
end;

procedure TOptionsForm.AcceptBtnClick(Sender: TObject);
var LVessel: TVessel;
    s:       string;
begin
  s := '';

  if (nameCoBox.ItemIndex > -1) and (id > 0) then
   if (MessageDlg('Do you really want to save the marked Option Change(s) of Vessel ' + nameCoBox.Text + '?', mtCustom, [mbYes, mbNo], 0) = mrYes) then
   begin


// Short Interval
     if shortIntChBox.checked
       then s := 'VES_SHORTINTERVAL = ' + uShortIntEd.Text;

// Long Interval
     if longIntChBox.checked then
      if (s = '')
       then s := 'VES_LONGINTERVAL = ' + uLongIntEd.Text
       else s := s + ', VES_LONGINTERVAL = ' + uLongIntEd.Text;

// Min Resistance
     if minResChBox.checked then
      if (s = '')
       then s := 'VES_MINRESISTANCE = ' + StringReplace(uMinResEd.Text, DecimalSeparator, '.', [rfReplaceAll])
       else s := s + ', VES_MINRESISTANCE = ' + StringReplace(uMinResEd.Text, DecimalSeparator, '.', [rfReplaceAll]);

// Max Resistance
     if maxResChBox.checked then
      if (s = '')
       then s := 'VES_MAXRESISTANCE = ' + StringReplace(uMaxResEd.Text, DecimalSeparator, '.', [rfReplaceAll])
       else s := s + ', VES_MAXRESISTANCE = ' + StringReplace(uMaxResEd.Text, DecimalSeparator, '.', [rfReplaceAll]);

// Heattime
     if heatChBox.checked then
      if (s = '')
       then s := 'VES_HEATTIME = ' + uHeatEd.Text
       else s := s + ', VES_HEATTIME = ' + uHeatEd.Text;

// ADC Loops
     if adcChBox.checked then
      if (s = '')
       then s := 'VES_ADCLOOP = ' + uAdcEd.Text
       else s := s + ', VES_ADCLOOP = ' + uAdcEd.Text;

// Fill Timeout
     if fillTimeChBox.checked then
      if (s = '')
       then s := 'VES_FILLTIMEOUT = ' + uFillTimeEd.Text
       else s := s + ', VES_FILLTIMEOUT = ' + uFillTimeEd.Text;

// Span
     if spanChBox.checked then
      if (s = '')
       then s := 'VES_SPAN = ' + uSpanEd.Text
       else s := s + ', VES_SPAN = ' + uSpanEd.Text;

// Zero
     if zeroChBox.checked then
      if (s = '')
       then s := 'VES_ZERO = ' + uZeroEd.Text
       else s := s + ', VES_ZERO = ' + uZeroEd.Text;


     if not (s = '') then
     begin
       deleteKey('Software\HZB_Helium\New_VesOptions\' + nameCoBox.Text);
       nameCoBox.DeleteSelected;

       with FDataMgmt do
       try
         LockWhileWorkingWithList;
         LVessel := GetVesItem(id);

         if Assigned(LVessel) then
         begin
           if shortIntChBox.checked
            then LVessel.ves_shortinterval := strToInt(uShortIntEd.Text);
           if longIntChBox.checked
            then LVessel.ves_longinterval := strToInt(uLongIntEd.Text);
           if minResChBox.checked
            then LVessel.ves_minresistance := strToFloat(uMinResEd.Text);
           if maxResChBox.checked
            then LVessel.ves_maxresistance := strToFloat(uMaxResEd.Text);
           if heatChBox.checked
            then LVessel.ves_heattime := strToInt(uHeatEd.Text);
           if adcChBox.checked
            then LVessel.ves_adcloops := strToInt(uAdcEd.Text);
           if fillTimeChBox.checked
            then LVessel.ves_filltimeout := strToInt(uFillTimeEd.Text);
           if spanChBox.checked
            then LVessel.ves_span := strToInt(uSpanEd.Text);
           if zeroChBox.checked
            then LVessel.ves_zero := strToInt(uZeroEd.Text);
         end;

         SqlCommand('UPDATE ' + db_t_hlm_vessel + ' SET ' + s + ' WHERE VES_V_ID = '  + intToStr(id));
       finally
         UnlockAfterWorkingWithList;
       end;

       if not lastItem then
       begin
         clearAll;
         nameCoBox.Text := 'select ...';
       end;
     end;
   end;
end;

procedure TOptionsForm.DiscardBtnClick(Sender: TObject);
begin
  if (nameCoBox.ItemIndex > -1) then
   if (MessageDlg('Do you really want to discard the Option Change(s) of Vessel ' + nameCoBox.Text + '?', mtCustom, [mbYes, mbNo], 0) = mrYes) then
   begin
     deleteKey('Software\HZB_Helium\New_VesOptions\' + nameCoBox.Text);
     nameCoBox.DeleteSelected;

     if not lastItem then
     begin
       clearAll;
       nameCoBox.Text := 'select ...';
     end;
   end;
end;

procedure TOptionsForm.CancelBtnClick(Sender: TObject);
begin
  if not lastItem then ModalResult := mrCancel;
end;

end.
