unit OptionsModule;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Registry,
  HeliumDataMgmt;

type
  TModuleOptionForm = class(TForm)
    addressLab: TLabel;
    dNameLab: TLabel;
    nameEd: TEdit;
    addressCoBox: TComboBox;
    curLab: TLabel;
    newLab: TLabel;
    cVoltMaxEd: TEdit;
    cVoltMinEd: TEdit;
    uVoltMaxEd: TEdit;
    uVoltMinEd: TEdit;
    VoltMaxChBox: TCheckBox;
    VoltMinChBox: TCheckBox;
    cVoltAlarmEd: TEdit;
    uVoltAlarmEd: TEdit;
    VoltAlarmBox: TCheckBox;
    AcceptBtn: TButton;
    DiscardBtn: TButton;
    CancelBtn: TButton;
    cVoltMaxLab: TLabel;
    cVoltMinLab: TLabel;
    cVoltAlarmLab: TLabel;
    procedure addressCoBoxChange(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DiscardBtnClick(Sender: TObject);
    procedure AcceptBtnClick(Sender: TObject);
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

constructor TModuleOptionForm.CreateWithProperties(ADataMgmt: TDataMgmt; AOwner: TComponent);
begin
  create(AOwner);
  FDataMgmt := ADataMgmt;
  getNameList;
  if (addressCoBox.Items.Count = 0) then
    MessageDlg('There are no modified Module Options!', mtWarning, [mbOK], 0);
end;

procedure TModuleOptionForm.getNameList;
var LReg: TRegistry;
begin
  addressCoBox.Clear;
  addressCoBox.Text := 'select ...';
  LReg         := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  if LReg.KeyExists('Software\HZB_Helium\New_ModOptions\') then
  try  LReg.OpenKey('Software\HZB_Helium\New_ModOptions\', false);
       LReg.GetKeyNames(addressCoBox.Items);
  finally LReg.CloseKey;
  end;
  LReg.Free;
end;

procedure TModuleOptionForm.clearAll;
begin
  id                   := 0;
  nameEd.Text          := '';
  cVoltMaxEd.Text      := '';
  cVoltMinEd.Text      := '';
  cVoltAlarmEd.Text    := '';
  uVoltMaxEd.Text      := '';
  uVoltMinEd.Text      := '';
  uVoltAlarmEd.Text    := '';
  VoltMaxChBox.Checked := false;
  VoltMinChBox.Checked := false;
  VoltAlarmBox.Checked := false;
end;

function TModuleOptionForm.lastItem: boolean;
begin
  Result := (addressCoBox.Items.Count = 0);
  if Result then ModalResult := mrOk;
end;

function TModuleOptionForm.fillParams: boolean;
var LReg:    TRegistry;
    LModule: TModule;
begin
  Result := false;

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LModule := GetModItemByAddr(addressCoBox.Text);

    if Assigned(LModule) then
    begin
      id                := LModule.mod_id;
      nameEd.Text       := LModule.mod_name;
      cVoltMaxEd.Text   := floatToStr(LModule.mod_maxvoltage);
      cVoltMinEd.Text   := floatToStr(LModule.mod_minvoltage);
      cVoltAlarmEd.Text := intToStr(LModule.mod_criticvoltage);
    end;
  finally
    UnlockAfterWorkingWithList;
  end;

  if (id > 0) then
  try
    LReg         := TRegistry.Create;
    LReg.RootKey := HKEY_CURRENT_USER;
    if LReg.KeyExists('Software\HZB_Helium\New_ModOptions\' + addressCoBox.Text + '\') then
    try
      LReg.OpenKey('Software\HZB_Helium\New_ModOptions\' + addressCoBox.Text + '\', false);

// Max Voltage
      if LReg.ValueExists('voltage_max') then
      begin
        uVoltMaxEd.Text := floattostr(LReg.ReadFloat('voltage_max'));
        VoltMaxChBox.Checked := true;
        Result := true;
      end;

// Min Voltage
      if LReg.ValueExists('voltage_min') then
      begin
        uVoltMinEd.Text := floattostr(LReg.ReadFloat('voltage_min'));
        VoltMinChBox.Checked := true;
        Result := true;
      end;

// Alarm Voltage
      if LReg.ValueExists('voltage_alarm') then
      begin
        uVoltAlarmEd.Text := inttostr(LReg.ReadInteger('voltage_alarm'));
        VoltAlarmBox.Checked := true;
        Result := true;
      end;
    finally LReg.CloseKey;
    end;
    LReg.Free;
  finally
  end;
end;

procedure TModuleOptionForm.addressCoBoxChange(Sender: TObject);
begin
  if (addressCoBox.ItemIndex > -1) then
  begin
    clearAll;
    if not fillParams then
      MessageDlg('No changed parameters for this Vessel!', mtWarning, [mbOK], 0);
  end
  else
    addressCoBox.Text := 'select ...';
end;

procedure TModuleOptionForm.AcceptBtnClick(Sender: TObject);
var LModule: TModule;
    s:       string;
begin
  s := '';

  if (addressCoBox.ItemIndex > -1) and (id > 0) then
   if (MessageDlg('Do you really want to save the marked Option Change(s) of Module ' + nameEd.Text + '?', mtCustom, [mbYes, mbNo], 0) = mrYes) then
   begin

     // Max Voltage
     if VoltMaxChBox.checked
      then s := 'MOD_MAXVOLTAGE = ' + StringReplace(uVoltMaxEd.Text, DecimalSeparator, '.', [rfReplaceAll]);

     // Min Voltage
     if VoltMinChBox.checked then
      if (s = '')
       then s := 'MOD_MINVOLTAGE = ' + StringReplace(uVoltMinEd.Text, DecimalSeparator, '.', [rfReplaceAll])
       else s := s + ', MOD_MINVOLTAGE = ' + StringReplace(uVoltMinEd.Text, DecimalSeparator, '.', [rfReplaceAll]);

     // Critical Voltage
     if VoltAlarmBox.checked then
      if (s = '')
       then s := 'MOD_CRITICALVOLTAGE = ' + uVoltAlarmEd.Text
       else s := s + ', MOD_CRITICALVOLTAGE = ' + uVoltAlarmEd.Text;
   end;

  if not (s = '') then
  begin
    deleteKey('Software\HZB_Helium\New_ModOptions\' + addressCoBox.Text);
    addressCoBox.DeleteSelected;

    with FDataMgmt do
    try
      LockWhileWorkingWithList;
      LModule := GetModItem(id);

      if Assigned(LModule) then
      begin
        if VoltMaxChBox.checked
         then LModule.mod_maxvoltage    := strToFloat(uVoltMaxEd.Text);
        if VoltMinChBox.checked
         then LModule.mod_minvoltage    := strToFloat(uVoltMinEd.Text);
        if VoltAlarmBox.checked
         then LModule.mod_criticvoltage := strToInt(uVoltAlarmEd.Text);
      end;

      SqlCommand('UPDATE ' + db_t_module + ' SET ' + s + ' WHERE MOD_ID = '  + intToStr(id));
    finally
      UnlockAfterWorkingWithList;
    end;

    if not lastItem then
    begin
      clearAll;
      addressCoBox.Text := 'select ...';
    end;
  end;
end;

procedure TModuleOptionForm.DiscardBtnClick(Sender: TObject);
begin
  if (addressCoBox.ItemIndex > -1) then
   if (MessageDlg('Do you really want to discard the Option Change(s) of Module ' + nameEd.Text + '?', mtCustom, [mbYes, mbNo], 0) = mrYes) then
   begin
     deleteKey('Software\HZB_Helium\New_ModOptions\' + addressCoBox.Text);
     addressCoBox.DeleteSelected;

     if not lastItem then
     begin
       clearAll;
       addressCoBox.Text := 'select ...';
     end;
   end;
end;

procedure TModuleOptionForm.CancelBtnClick(Sender: TObject);
begin
  if not lastItem then ModalResult := mrCancel;
end;

end.
