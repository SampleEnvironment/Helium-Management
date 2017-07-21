unit IlmDevice;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, DatabaseEdit, HeliumDataMgmt;

resourcestring
  CREATE_NEW_CATEGORY = 'Create new category';

type
  TIlmDeviceForm = class(TForm)
    NameLab: TLabel;
    AddressLab: TLabel;
    CommentLab: TLabel;
    ilmLab: TLabel;
    HintLab: TLabel;
    ilmCoBox: TComboBox;
    devPanel: TPanel;
    NameEd: TOraEdit;
    AddressEd: TOraEdit;
    CommentRiEd: TRichEdit;
    SaveBtn: TButton;
    DeleteBtn: TButton;
    CancelBtn: TButton;
    Ch1SpanLab: TLabel;
    Ch1ZeroEd: TOraEdit;
    Ch1ZeroLab: TLabel;
    Ch1ChkBox: TCheckBox;
    Ch1SpanEd: TOraEdit;
    Ch2SpanLab: TLabel;
    Ch2ZeroEd: TOraEdit;
    Ch2ZeroLab: TLabel;
    Ch2ChkBox: TCheckBox;
    Ch2SpanEd: TOraEdit;
    Ch3SpanLab: TLabel;
    Ch3ZeroEd: TOraEdit;
    Ch3ZeroLab: TLabel;
    Ch3SpanEd: TOraEdit;
    Ch3ChkBox: TCheckBox;
    Ch1GasCoBox: TComboBox;
    Ch1GasLab: TLabel;
    Ch2GasLab: TLabel;
    Ch3GasLab: TLabel;
    Ch2GasCoBox: TComboBox;
    Ch3GasCoBox: TComboBox;
    CBCategory: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    EdNewCatName: TEdit;
    procedure ilmCoBoxChange(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure Ch1ChkBoxClick(Sender: TObject);
    procedure Ch2ChkBoxClick(Sender: TObject);
    procedure Ch3ChkBoxClick(Sender: TObject);
    procedure CBCategoryChange(Sender: TObject);
  private
    FAddCategoryIndex: integer;
    FRegisteredClasses: TStringList;
    FDataMgmt: TDataMgmt;
    procedure  InitForm;
  public
    constructor CreateWithProperties(ADataMgmt: TDataMgmt; AOwner: TComponent);
    destructor  Destroy; override;
  end;

implementation

{$R *.dfm}

{ TIlmDeviceForm }

constructor TIlmDeviceForm.CreateWithProperties(ADataMgmt: TDataMgmt;
  AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAddCategoryIndex := -1;
  HintLab.Caption := '';
  FDataMgmt := ADataMgmt;
  InitForm;
end;

destructor TIlmDeviceForm.Destroy;
begin
  if Assigned(FRegisteredClasses) then
    FRegisteredClasses.Free;
  inherited;
end;

procedure TIlmDeviceForm.InitForm;
var i: integer;
begin
  ilmCoBox.Clear;
  ilmCoBox.AddItem('- CREATE NEW CRYOSTAT -', nil);

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    if Assigned(FRegisteredClasses) then
      FRegisteredCLasses.Free;
    FRegisteredClasses := GetRegisteredCryoClasses;
    // Initializing Categories
    CBCategory.Items.Clear;
    with FRegisteredClasses do
      while CBCategory.Items.Count < Count do
        CBCategory.Items.Add(Strings[CBCategory.Items.Count]);
    FAddCategoryIndex := CBCategory.Items.Add(Format('- %s -', [Uppercase(CREATE_NEW_CATEGORY)]));

    RefreshDevListByDB;
    for i := 0 to DeviceList.Count - 1 do
      ilmCoBox.AddItem(TCryostat(DeviceList[i]).dev_name,
                       TObject(TCryostat(DeviceList[i]).dev_id));
  finally
    UnlockAfterWorkingWithList;
  end;

  ilmCoBox.ItemIndex := 0;
  ActiveControl      := ilmCoBox;
  IlmCoBoxChange(nil);
end;

procedure TIlmDeviceForm.ilmCoBoxChange(Sender: TObject);
var LDev: TCryostat;

  function GetGasIndex(const AGas: string): integer;
  begin
    Result := -1;
    if AGas = 'He' then
      Result := 0
    else
      if AGas = 'N' then
        Result := 1;
  end;

  function GetClassName(AID: integer): string;
  var i: integer;
  begin
    result := '';
    i := 0;
    while (i < FRegisteredCLasses.Count) and (result = '') do
    begin
      if AID = Integer(FRegisteredCLasses.Objects[i]) then
        result := FRegisteredClasses[i];
      inc(i);
    end;
  end;

var s: string;
begin
  if Assigned(Sender) then HintLab.Caption := '';

  if (ilmCoBox.ItemIndex = 0) then
  begin
    NameEd.Text      := '';
    AddressEd.Text   := '';
    CommentRiEd.Text := ''; 
    CBCategory.Text := '';

    Ch1ChkBox.Checked := false;
    Ch2ChkBox.Checked := false;
    Ch3ChkBox.Checked := false;

    Ch1ChkBoxClick(nil);
    Ch2ChkBoxClick(nil);
    Ch3ChkBoxClick(nil);
  end;

  if (ilmCoBox.ItemIndex > 0) then
  begin
    with FDataMgmt, ilmCoBox do
    try
      LockWhileWorkingWithList;
      LDev := GetCopyOfIlmDevice(GetDevIndex(integer(Items.Objects[ItemIndex])));
    finally
      UnlockAfterWorkingWithList;
    end;

    if Assigned(LDev) then
    begin
      NameEd.Text       := LDev.dev_name;
      AddressEd.Text    := LDev.dev_address;
      CommentRiEd.Text  := LDev.dev_comment;
      Ch1ChkBox.Checked := LDev.dev_ch1_enabled;
      Ch2ChkBox.Checked := LDev.dev_ch2_enabled;
      Ch3ChkBox.Checked := LDev.dev_ch3_enabled;

      s := GetClassName(LDev.dev_class);
      if s <> '' then
      begin
        if CBCategory.Items.IndexOf(s) < 0 then
          CBCategory.Items.Add(s);
        CBCategory.ItemIndex := CBCategory.Items.IndexOf(s);
        CBCategory.Text := s;
      end;

      if LDev.dev_ch1_enabled then
      begin
        Ch1GasCoBox.ItemIndex := GetGasIndex(LDev.dev_ch1_gas);
        Ch1SpanEd.Text := floatToStr(LDev.dev_ch1_span);
        Ch1ZeroEd.Text := floatToStr(LDev.dev_ch1_zero);
      end
      else begin
        Ch1GasCoBox.ItemIndex := -1;
        Ch1SpanEd.Text := '';
        Ch1ZeroEd.Text := '';
      end;

      if LDev.dev_ch2_enabled then
      begin
        Ch2GasCoBox.ItemIndex := GetGasIndex(LDev.dev_ch2_gas);
        Ch2SpanEd.Text := floatToStr(LDev.dev_ch2_span);
        Ch2ZeroEd.Text := floatToStr(LDev.dev_ch2_zero);
      end
      else begin
        Ch2GasCoBox.ItemIndex := -1;
        Ch2SpanEd.Text := '';
        Ch2ZeroEd.Text := '';
      end;

      if LDev.dev_ch3_enabled then
      begin
        Ch3GasCoBox.ItemIndex := GetGasIndex(LDev.dev_ch3_gas);
        Ch3SpanEd.Text := floatToStr(LDev.dev_ch3_span);
        Ch3ZeroEd.Text := floatToStr(LDev.dev_ch3_zero);
      end
      else begin
        Ch3GasCoBox.ItemIndex := -1;
        Ch3SpanEd.Text := '';
        Ch3ZeroEd.Text := '';
      end;
    end
    else
      InitForm;
  end;
end;


procedure TIlmDeviceForm.CBCategoryChange(Sender: TObject);
begin
  EdNewCatName.Enabled := (FAddCategoryIndex > -1) and
                          (CBCategory.ItemIndex = FAddCategoryIndex);
end;

procedure TIlmDeviceForm.Ch1ChkBoxClick(Sender: TObject);
begin
  if not Ch1ChkBox.Checked then
  begin
    Ch1GasCoBox.ItemIndex := -1;
    Ch1SpanEd.Text        := '';
    Ch1ZeroEd.Text        := '';
  end;

  Ch1GasCoBox.Enabled := Ch1ChkBox.Checked;
  Ch1SpanEd.Enabled   := Ch1ChkBox.Checked;
  Ch1ZeroEd.Enabled   := Ch1ChkBox.Checked;
end;

procedure TIlmDeviceForm.Ch2ChkBoxClick(Sender: TObject);
begin
  if not Ch2ChkBox.Checked then
  begin
    Ch2GasCoBox.ItemIndex := -1;
    Ch2SpanEd.Text        := '';
    Ch2ZeroEd.Text        := '';
  end;

  Ch2GasCoBox.Enabled := Ch2ChkBox.Checked;
  Ch2SpanEd.Enabled   := Ch2ChkBox.Checked;
  Ch2ZeroEd.Enabled   := Ch2ChkBox.Checked;
end;

procedure TIlmDeviceForm.Ch3ChkBoxClick(Sender: TObject);
begin
  if not Ch3ChkBox.Checked then
  begin
    Ch3GasCoBox.ItemIndex := -1;
    Ch3SpanEd.Text        := '';
    Ch3ZeroEd.Text        := '';
  end;

  Ch3GasCoBox.Enabled := Ch3ChkBox.Checked;
  Ch3SpanEd.Enabled   := Ch3ChkBox.Checked;
  Ch3ZeroEd.Enabled   := Ch3ChkBox.Checked;
end;


procedure TIlmDeviceForm.SaveBtnClick(Sender: TObject);
var LVal: double;
    LDev: TCryostat;

  function GasChannelSettingsOK: boolean;
  begin
    Result := true;

    if Ch1ChkBox.Checked then
      if not (Ch1GasCoBox.ItemIndex in [0, 1]) or
         not TryStrToFloat(Ch1SpanEd.Text, LVal) or
         not TryStrToFloat(Ch1ZeroEd.Text, LVal) then
        Result := false;

    if Ch2ChkBox.Checked then
      if not (Ch2GasCoBox.ItemIndex in [0, 1]) or
         not TryStrToFloat(Ch2SpanEd.Text, LVal) or
         not TryStrToFloat(Ch2ZeroEd.Text, LVal) then
        Result := false;

    if Ch3ChkBox.Checked then
      if not (Ch3GasCoBox.ItemIndex in [0, 1]) or
         not TryStrToFloat(Ch3SpanEd.Text, LVal) or
         not TryStrToFloat(Ch3ZeroEd.Text, LVal) then
        Result := false;

    result := result and
              ((CBCategory.ItemIndex <> FAddCategoryIndex) or
              ((EdNewCatName.Text <> '') and (CBCategory.Items.IndexOf(EdNewCatName.Text) < 0)));
  end;

var lClassID: integer;
begin
  LDev := nil;
  if (ilmCoBox.ItemIndex   <> -1) then
    if (trim(NameEd.Text)    <> '') then
      if (trim(AddressEd.Text) <> '') then
        if GasChannelSettingsOK then
        begin
          if (CBCategory.ItemIndex = FAddCategoryIndex) then
            lClassID := FDataMgmt.InsertNewCryoClass(EdNewCatName.Text)
          else
            lClassID := Integer(FRegisteredClasses.Objects[FRegisteredClasses.IndexOf(CBCategory.Text)]);

          if (ilmCoBox.ItemIndex = 0) then
            if (MessageDlg('Do you really want to save the new cryostat?',
                           mtCustom, [mbYes, mbNo], 0) = mrYes) then
              with FDataMgmt do
              try
                LockWhileWorkingWithList;
                LDev := TCryostat.Create;
                LDev.dev_class       := lClassID;
                LDev.dev_id          := 0;
                LDev.dev_name        := NameEd.Text;
                LDev.dev_address     := AddressEd.Text;
                LDev.dev_comment     := CommentRiEd.Text;
                LDev.dev_ch1_enabled := Ch1ChkBox.Checked;
                LDev.dev_ch2_enabled := Ch2ChkBox.Checked;
                LDev.dev_ch3_enabled := Ch3ChkBox.Checked;

                if LDev.dev_ch1_enabled then
                begin
                  LDev.dev_ch1_gas  := Ch1GasCoBox.Text;
                  LDev.dev_ch1_span := strToFloat(Ch1SpanEd.Text);
                  LDev.dev_ch1_zero := strToFloat(Ch1ZeroEd.Text);
                end
                else begin
                  LDev.dev_ch1_gas  := '';
                  LDev.dev_ch1_span := 0;
                  LDev.dev_ch1_zero := 0;
                end;

                if LDev.dev_ch2_enabled then
                begin
                  LDev.dev_ch2_gas  := Ch2GasCoBox.Text;
                  LDev.dev_ch2_span := strToFloat(Ch2SpanEd.Text);
                  LDev.dev_ch2_zero := strToFloat(Ch2ZeroEd.Text);
                end
                else begin
                  LDev.dev_ch2_gas  := '';
                  LDev.dev_ch2_span := 0;
                  LDev.dev_ch2_zero := 0;
                end;

                if LDev.dev_ch3_enabled then
                begin
                  LDev.dev_ch3_gas  := Ch3GasCoBox.Text;
                  LDev.dev_ch3_span := strToFloat(Ch3SpanEd.Text);
                  LDev.dev_ch3_zero := strToFloat(Ch3ZeroEd.Text);
                end
                else begin
                  LDev.dev_ch3_gas  := '';
                  LDev.dev_ch3_span := 0;
                  LDev.dev_ch3_zero := 0;
                end;

                if InsertIlmDeviceToDb(LDev) then
                  HintLab.Caption := 'Creating new cryostat succeeded.'
                else
                  HintLab.Caption := 'Creating new cryostat failed.';
              finally
                UnlockAfterWorkingWithList;
                InitForm;
              end;

          if (ilmCoBox.ItemIndex > 0) then
            if (MessageDlg('Do you really want to change the cryostat settings?',
                           mtCustom, [mbYes, mbNo], 0) = mrYes) then
              with FDataMgmt, ilmCoBox do
              try
                LockWhileWorkingWithList;
                LDev := GetDevItem(integer(Items.Objects[ItemIndex]));

                if Assigned(LDev) then
                begin
                  LDev.dev_class       := lCLassID;
                  LDev.dev_name        := NameEd.Text;
                  LDev.dev_address     := AddressEd.Text;
                  LDev.dev_comment     := CommentRiEd.Text;
                  LDev.dev_ch1_enabled := Ch1ChkBox.Checked;
                  LDev.dev_ch2_enabled := Ch2ChkBox.Checked;
                  LDev.dev_ch3_enabled := Ch3ChkBox.Checked;

                  if LDev.dev_ch1_enabled then
                  begin
                    LDev.dev_ch1_gas  := Ch1GasCoBox.Text;
                    LDev.dev_ch1_span := strToFloat(Ch1SpanEd.Text);
                    LDev.dev_ch1_zero := strToFloat(Ch1ZeroEd.Text);
                  end
                  else begin
                    LDev.dev_ch1_gas  := '';
                    LDev.dev_ch1_span := 0;
                    LDev.dev_ch1_zero := 0;
                  end;

                  if LDev.dev_ch2_enabled then
                  begin
                    LDev.dev_ch2_gas  := Ch2GasCoBox.Text;
                    LDev.dev_ch2_span := strToFloat(Ch2SpanEd.Text);
                    LDev.dev_ch2_zero := strToFloat(Ch2ZeroEd.Text);
                  end
                  else begin
                    LDev.dev_ch2_gas  := '';
                    LDev.dev_ch2_span := 0;
                    LDev.dev_ch2_zero := 0;
                  end;

                  if LDev.dev_ch3_enabled then
                  begin
                    LDev.dev_ch3_gas  := Ch3GasCoBox.Text;
                    LDev.dev_ch3_span := strToFloat(Ch3SpanEd.Text);
                    LDev.dev_ch3_zero := strToFloat(Ch3ZeroEd.Text);
                  end
                  else begin
                    LDev.dev_ch3_gas  := '';
                    LDev.dev_ch3_span := 0;
                    LDev.dev_ch3_zero := 0;
                  end;
                                 
                  if UpdateIlmDeviceToDb(LDev) then
                    HintLab.Caption := 'Update on ' + ilmCoBox.Text + ' succeeded.'
                  else
                    HintLab.Caption := 'Update on ' + ilmCoBox.Text + ' failed.';
                end
                else
                  HintLab.Caption := 'Update on ' + ilmCoBox.Text + ' failed.';
              finally
                LDev := nil;
                UnlockAfterWorkingWithList;
                InitForm;
              end;
        end
        else ShowMessage('Invalid channel settings!')
      else showMessage('Please enter the XBee address of the cryostat!')
    else showMessage('Please enter a name for the cryostat!');

  if Assigned(LDev) then LDev.Free;
end;

procedure TIlmDeviceForm.DeleteBtnClick(Sender: TObject);
begin
  if (ilmCoBox.ItemIndex > 0) then
  begin
    if (MessageDlg('Do you really want to delete ' + ilmCoBox.Text + '?',
                   mtCustom, [mbYes, mbNo], 0) = mrYes) then
    with FDataMgmt, ilmCoBox do
    try
      LockWhileWorkingWithList;
      if DeleteIlmDeviceToDb(integer(Items.Objects[ItemIndex])) then
        HintLab.Caption := ilmCoBox.Text + ' successfully deleted.'
      else
        HintLab.Caption := 'Database operation failed.';
    finally
      UnlockAfterWorkingWithList;
      InitForm;
    end;
  end
  else
    MessageDLg('Select a cryostat to delete!', mtError, [mbOK], 0);
end;

end.
