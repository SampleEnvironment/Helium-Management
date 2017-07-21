unit XBeeModule;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, Dialogs,
  HeliumDataMgmt, DatabaseEdit;

type
  TModuleForm = class(TForm)
    NameLab: TLabel;
    modCoBox: TComboBox;
    LastMsgLab: TLabel;
    LastMsgEd: TEdit;
    ePanel: TPanel;
    eNameLab: TLabel;
    NameEd: TOraEdit;
    eVoltMinLab: TLabel;
    VoltMinEd: TOraEdit;
    eVoltMaxLab: TLabel;
    VoltMaxEd: TOraEdit;
    eVoltAlarmLab: TLabel;
    VoltAlarmEd: TOraEdit;
    CommentRiEd: TRichEdit;
    eCommentLab: TLabel;
    AddressEd: TOraEdit;
    eAddressLab: TLabel;
    ActiveLab: TLabel;
    SaveBtn: TButton;
    DeleteBtn: TButton;
    HintLab: TLabel;
    CancelBtn: TButton;
    Label1: TLabel;
    VoltCritEd: TEdit;
    procedure modCoBoxChange(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
  private
    FDataMgmt: TDataMgmt;
    procedure  InitForm;
    procedure OnCriticalVoltageChanged(Sender: TObject);
  public
    constructor CreateWithProperties(ADataMgmt: TDataMgmt; AOwner: TComponent);
    destructor  Destroy; override;
  end;

implementation
{$R *.dfm}

constructor TModuleForm.CreateWithProperties(ADataMgmt: TDataMgmt; AOwner: TComponent);
begin
  inherited Create(AOwner);

  HintLab.Caption := '';
  FDataMgmt       := ADataMgmt;

  VoltAlarmEd.OnChange := OnCriticalVoltageChanged;
  VoltMinEd.OnChange   := OnCriticalVoltageChanged;
  VoltMaxEd.OnChange   := OnCriticalVoltageChanged;
  InitForm;
end;

destructor TModuleForm.Destroy;
begin

  inherited;
end;

procedure TModuleForm.InitForm;
var i: integer;
begin
  modCoBox.Clear;
  modCoBox.AddItem('- CREATE NEW MODULE -', nil);

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    RefreshModListByDB;
    for i := 0 to ModuleList.Count - 1
     do modCoBox.AddItem(TModule(ModuleList[i]).mod_name,
                 TObject(TModule(ModuleList[i]).mod_id));
  finally
    UnlockAfterWorkingWithList;
  end;

  ActiveLab.Caption  := '';
  LastMsgEd.Text     := '';
  NameEd.Text        := '';
  AddressEd.Text     := '';
  VoltMinEd.Text     := '';
  VoltMaxEd.Text     := '';
  VoltAlarmEd.Text   := '';
  CommentRiEd.Text   := '';

  AddressEd.Enabled  := true;
  DeleteBtn.Enabled  := true;

  modCoBox.ItemIndex := 0;
  ActiveControl      := modCoBox;
  modCoBoxChange(nil);
end;


procedure TModuleForm.modCoBoxChange(Sender: TObject);
var LMod: TModule;
begin
  if Assigned(Sender) then HintLab.Caption := '';
  AddressEd.Enabled := true;
  DeleteBtn.Enabled := true;

  if (modCoBox.ItemIndex = 0) then
  begin
    ActiveLab.Caption  := 'Enter values for the new module.';
    LastMsgEd.Text     := '';
    NameEd.Text        := '';
    AddressEd.Text     := '';
    VoltMinEd.Text     := '';
    VoltMaxEd.Text     := '';
    VoltAlarmEd.Text   := '';
    CommentRiEd.Text   := '';
  end;

  if (modCoBox.ItemIndex > 0) then
  begin
    with FDataMgmt, modCoBox do
    try
      LockWhileWorkingWithList;
      LMod := GetCopyOfModule(GetModIndex(integer(Items.Objects[ItemIndex])));
    finally
      UnlockAfterWorkingWithList;
    end;

    if Assigned(LMod) then
    begin
      if not (LMod.mod_status = 0) then
      begin
        AddressEd.Enabled := false;
        DeleteBtn.Enabled := false;
        ActiveLab.Caption := modCoBox.Text + ' is currently active!';
      end
      else ActiveLab.Caption := '';

      LastMsgEd.Text   := LMod.mod_lastactive;
      NameEd.Text      := LMod.mod_name;
      AddressEd.Text   := LMod.mod_address;
      VoltMinEd.Text   := floatToStr(LMod.mod_minvoltage);
      VoltMaxEd.Text   := floatToStr(LMod.mod_maxvoltage);
      VoltAlarmEd.Text := intToStr(LMod.mod_criticvoltage);
      CommentRiEd.Text := LMod.mod_comment;
    end
    else InitForm;
  end;
end;


procedure TModuleForm.OnCriticalVoltageChanged(Sender: TObject);
var
  VRel, VMin, VMax: Extended;
begin
  if TryStrToFloat(VoltAlarmEd.Text, VRel) and
     TryStrToFloat(VoltMinEd.Text, VMin) and
     TryStrToFloat(VoltMaxEd.Text, VMax) then
    VoltCritEd.Text := FloatToStr(VMin + (VMax - VMin)*(VRel/100));
end;

procedure TModuleForm.SaveBtnClick(Sender: TObject);
var LMod: TModule;
begin
  LMod := nil;
  if (modCoBox.ItemIndex    <> -1) then
   if (trim(NameEd.Text)     <> '') then
    if (trim(AddressEd.Text)  <> '') then
     if (VoltMinEd.Text        <> '') then
      if (VoltMaxEd.Text        <> '') then
       if (VoltAlarmEd.Text      <> '') then
       begin
         if (modCoBox.ItemIndex = 0) then
          if (MessageDlg('Do you really want to save the new module?', mtCustom, [mbYes, mbNo], 0) = mrYes) then
            with FDataMgmt do
            try
              LockWhileWorkingWithList;
              LMod := TModule.Create;
              LMod.mod_id            := 0;
              LMod.mod_name          := NameEd.Text;
              LMod.mod_address       := AddressEd.Text;
              LMod.mod_minvoltage    := strToFloat(VoltMinEd.Text);
              LMod.mod_maxvoltage    := strToFloat(VoltMaxEd.Text);
              LMod.mod_criticvoltage := strToInt(VoltAlarmEd.Text);
              LMod.mod_comment       := CommentRiEd.Text;

               if InsertModuleToDb(LMod)
               then HintLab.Caption := 'Creating new module succeeded.'
               else HintLab.Caption := 'Creating new module failed.';
            finally
              UnlockAfterWorkingWithList;
              InitForm;
            end;

         if (modCoBox.ItemIndex > 0) then
          if (MessageDlg('Do you really want to change the module settings?', mtCustom, [mbYes, mbNo], 0) = mrYes) then
            with FDataMgmt, modCoBox do
            try
              LockWhileWorkingWithList;
              LMod := GetModItem(integer(Items.Objects[ItemIndex]));

              if Assigned(LMod) then
              begin
                LMod.mod_name          := NameEd.Text;
                LMod.mod_address       := AddressEd.Text;
                LMod.mod_minvoltage    := strToFloat(VoltMinEd.Text);
                LMod.mod_maxvoltage    := strToFloat(VoltMaxEd.Text);
                LMod.mod_criticvoltage := strToInt(VoltAlarmEd.Text);
                LMod.mod_comment       := CommentRiEd.Text;
                LMod.mod_setOptions    := true;

                if UpdateModuleToDb(LMod)
                 then HintLab.Caption := 'Update on ' + modCoBox.Text + ' succeeded.'
                 else HintLab.Caption := 'Update on ' + modCoBox.Text + ' failed.';

              end
              else HintLab.Caption := 'Update on ' + modCoBox.Text + ' failed.';
            finally
              LMod := nil;
              UnlockAfterWorkingWithList;
              InitForm;
            end;        
       end
       else showMessage('Please enter a critical battery capacity!')
      else showMessage('Please enter the max battery voltage!')
     else showMessage('Please enter the min battery voltage!')
    else showMessage('Please enter the XBee address of the module!')
   else showMessage('Please enter a name for the module!');
   if Assigned(LMod) then LMod.Free;
end;

procedure TModuleForm.DeleteBtnClick(Sender: TObject);
begin
  if (modCoBox.ItemIndex > 0) then
  begin
    if (MessageDlg('Do you really want to delete ' + modCoBox.Text + '?', mtCustom, [mbYes, mbNo], 0) = mrYes) then
     with FDataMgmt, modCoBox do
     try
       LockWhileWorkingWithList;
       if DeleteModuleToDb(integer(Items.Objects[ItemIndex]))
        then HintLab.Caption := modCoBox.Text + ' successfully deleted.';
     finally
       UnlockAfterWorkingWithList;
       InitForm;
     end;
  end
  else showMessage('Select a module to delete!');
end;

end.
