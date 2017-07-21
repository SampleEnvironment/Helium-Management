unit Settings;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, ComCtrls, Dialogs, StdCtrls,
  Buttons, ExtCtrls, Registry, Graphics, Spin, PlugNRollCtrls,
  SQLClientInterfaces;

type
  TSettingsForm = class(TForm)
    cOkBtn: TButton;
    CancelBtn: TButton;
    ApplyBtn: TButton;
    Bevel2: TBevel;
    PageControl: TPageControl;
    PositionsTabSheet: TTabSheet;
    newLab: TLabel;
    posLab: TLabel;
    AddSpeedBtn: TSpeedButton;
    Bevel1: TBevel;
    DelSpeedBtn: TSpeedButton;
    Label1: TLabel;
    posLiBox: TListBox;
    newEd: TEdit;
    LevelmeterTabSheet: TTabSheet;
    sleepLab: TLabel;
    passwLab: TLabel;
    awakeLab: TLabel;
    sleepCoBox: TComboBox;
    passwCoBox: TComboBox;
    awakeCoBox: TComboBox;
    DatabaseTabSheet: TTabSheet;
    serverLab: TLabel;
    userLab: TLabel;
    pwLab: TLabel;
    dbuserEd: TEdit;
    dbpwEd: TEdit;
    AdvancedTabSheet: TTabSheet;
    debugChBox: TCheckBox;
    deleteCoBox: TComboBox;
    deleteLab: TLabel;
    RetryAttemptsSpinEdit: TSpinEdit;
    RetryAttemptsLab: TLabel;
    selectdbtypeCoBox: TComboBox;
    DatabaseTypeLab: TLabel;
    PanelDS: TPanel;
    EdDataSource: TEdit;
    BtnEditDS: TFlatGlyphBtn;
    procedure PageControlDrawTab(Control: TCustomTabControl; TabIndex: Integer;
      const Rect: TRect; Active: Boolean);
    procedure AddSpeedBtnClick(Sender: TObject);
    procedure DelSpeedBtnClick(Sender: TObject);
    procedure newEdKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure posLiBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cOkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure PropertyOnChange(Sender: TObject);
    procedure debugChBoxClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BtnEditDSClick(Sender: TObject);
  private
    FAwakeOld: byte;
    FAwakeNew: byte;
    FSleepOld: byte;
    FSleepNew: byte;
    FPasswOld: string;
    FPasswNew: string;

    FPosSaved:   boolean;
    FPosChanged: boolean;
    FSession: ISQLSession;

    procedure Init;
    procedure CleanUp;
    procedure PosString(const AString: string);
    procedure InsertPos(const AString: string);
    procedure AddNewPos;
    procedure DeletePos;
    procedure ApplyValues;
    procedure SavePositionList;
    procedure SaveSystemProperties;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(const ASession: ISQLSession): boolean;
  published
    property ListChanged:  boolean read FPosChanged;
    property AwakeTimeOld: byte    read FAwakeOld;
    property AwakeTimeNew: byte    read FAwakeNew;
    property SleepTimeOld: byte    read FSleepOld;
    property SleepTimeNew: byte    read FSleepNew;
    property PasswordOld:  string  read FPasswOld;
    property PasswordNew:  string  read FPasswNew;
  end;

implementation

uses
  HeliumFunctions, StrUtils, HeliumDataMgmt, IsaacStandardInterfaces, UtilIsaac;

{$R *.dfm}

constructor TSettingsForm.Create(AOwner: TComponent);
var i: integer;
begin
  inherited;
  FAwakeOld   := 0;
  FAwakeNew   := 0;
  FSleepOld   := 0;
  FSleepNew   := 0;
  FPasswNew   := '';
  FPasswNew   := '';
  FPosSaved   := true;
  FPosChanged := false;

  for i := 1   to 255 do
    awakeCoBox.Items.Add(intToStr(i) + ' sec');
  for i := 1   to 255 do
    sleepCoBox.Items.Add(intToStr(i) + ' min');
  for i := 0   to 9   do
    passwCoBox.Items.Add('00' + intToStr(i));
  for i := 10  to 99  do
    passwCoBox.Items.Add('0' + intToStr(i));
  for i := 100 to 999 do
    passwCoBox.Items.Add(intToStr(i));
  for i := 1   to 24  do
    deleteCoBox.Items.Add('older than ' + intToStr(i) + ' months.');
end;

function TSettingsForm.Execute(const ASession: ISQLSession): boolean;
begin
  FSession := ASession;
  try                  
    Init;
    ApplyBtn.Enabled := false;
    ShowModal;
    Result := ModalResult = mrOk;
  finally
    CleanUp;
    FSession := nil;
  end;
end;

procedure TSettingsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not ApplyBtn.Enabled;

  if not CanClose then
    CanClose := (MessageDlg('If you cancel your changes will get lost.' + #13
                            + 'Do you want to cancel anyway?',
                            mtConfirmation, [mbYes,mbNo], 0) = mrYes);
end;

procedure TSettingsForm.PageControlDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  LCaptionX: Integer;
  LCaptionY: Integer;
  LTabCaption: string;
begin
  with Control.Canvas do
  begin
    Font.Color  := clWindowText;
    Brush.Style := bsSolid;

    if (TabIndex = TPageControl(Control).TabIndex) then
      Brush.Color := clBtnHighLight
    else
      Brush.Color := clBtnFace;

    LTabCaption := PageControl.Pages[TabIndex].Caption;
    LCaptionX := Rect.Left + ((Rect.Right - Rect.Left - TextWidth(LTabCaption)) div 2);
    LCaptionY := Rect.Top + ((Rect.Bottom - Rect.Top - TextHeight('Gg')) div 2);
    FillRect(Rect);
    TextOut(LCaptionX, LCaptionY, LTabCaption);
  end;
end;

procedure TSettingsForm.posLiBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (shift=[]) then
    case key of
      46: DeletePos;
    end;
end;

procedure TSettingsForm.newEdKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (shift=[]) then
    case key of
      13: AddNewPos;
    end;
end;

procedure TSettingsForm.PropertyOnChange(Sender: TObject);
var
  lIndex: Integer;
  pID: PGUID;
begin
  lIndex := selectdbTypeCoBox.ItemIndex;
  ApplyBtn.Enabled := lIndex > -1;
end;

procedure TSettingsForm.debugChBoxClick(Sender: TObject);
begin
  PropertyOnChange(nil);
end;


procedure TSettingsForm.Init;
var i:    integer;
    LReg: TRegistry;
    LStr: TStringList;
    lFactories: IDataModule;
    pFactoryType: PGUID;
    pItem: PDataModuleItem;
    lIntf: IInterface;
    lFactory: ISQLImplementorFactory;
begin
  LStr         := TStringList.Create;
  LReg         := TRegistry.Create;
  try
    lFactories := GetSQLClientFactories;
    if Assigned(lFactories) then
    begin
      lFactories.Reset;
      while lFactories.MoveNext do
        try
          lFactories.GetCurrent(pItem);
          lFactories.GetInterfaceValue(pItem^.Key, lIntf);
          if supports(lIntf, ISQLImplementorFactory, lFactory) then
          begin
            new(pFactoryType);
            pFactoryType^ := lFactory.SupportedDataBaseType;
            selectdbTypeCoBox.Items.AddObject(lFactory.SupporteDataBaseIdentifier, TObject(pFactoryType));
          end;
        finally
          lFactory := nil;
          lIntf := nil;
        end;
    end;

    LReg.RootKey := HKEY_CURRENT_USER;
    with LReg do
    try
      if LReg.KeyExists('Software\HZB_Helium\Position\') then
      begin
        LReg.OpenKey('Software\HZB_Helium\Position\', false);
        LReg.GetValueNames(LStr);
        for i := 0 to LStr.Count - 1 do
          posString(LReg.ReadString(LStr[i]));
      end;
    finally
      CloseKey;
      LStr.Clear;
    end;

    with LReg do
    try
      if KeyExists('Software\HZB_Helium\') then
      begin
        OpenKey('Software\HZB_Helium\', false);

        if ValueExists('AwakeTime') then
        begin
          FAwakeOld := ReadInteger('AwakeTime');
          FAwakeNew := FAwakeOld;
          awakeCoBox.ItemIndex := FAwakeNew - 1;
        end;

        if ValueExists('SleepTime') then
        begin
          FSleepOld := ReadInteger('SleepTime');
          FSleepNew := FSleepOld;
          sleepCoBox.ItemIndex := FSleepNew - 1;
        end;

        if ValueExists('OptionsPW') then
        begin
          passwCoBox.ItemIndex := ReadInteger('OptionsPW');
          FPasswOld := passwCoBox.Text;
          FPasswNew := FPasswOld;
        end;

        if ValueExists('SQLClientId') then
          selectdbtypeCoBox.ItemIndex := selectdbTypeCoBox.Items.IndexOf(ReadString('SQLClientId'));
        if ValueExists('DbServer') then
          EdDataSource.Text := ReadString('DbServer');
        if ValueExists('DbUser') then
          dbuserEd.Text := ReadString('DbUser');
        if ValueExists('DbKey') then
          dbpwEd.Text := decrypt(ReadString('DbKey'));
        if ValueExists('DeleteOld') then
          deleteCoBox.ItemIndex := ReadInteger('DeleteOld') - 1;

        if ValueExists('DebugMode') then
          debugChBox.Checked := ReadBool('DebugMode');
        if ValueExists('CoordRetryAttmepts') then
          RetryAttemptsSpinEdit.Value := ReadInteger('CoordRetryAttmepts');
      end;
    finally
      CloseKey;
    end;
  finally
    lFactories := nil;
    LReg.Free;
    LStr.Free;
  end;
end;

procedure TSettingsForm.PosString(const AString: string);
var i, j: integer;
begin
  if (length(AString) <= 3) and (pos(';',AString) = 0) then
    posLiBox.Items.Add(AString)
  else begin
    if (AString[2] = ';') then
    begin
      i := 2;
      j := 3;
      while (i > 0) do
      begin
        i := posex(',', AString, j);
        if (i > 2) then
          posLiBox.Items.Add(AString[1] + copy(AString, j, i-j))
        else
          posLiBox.Items.Add(AString[1] + copy(AString,j));
        j := i + 1;
      end;
    end;
  end;
end;

procedure TSettingsForm.InsertPos(const AString: string);
var i, num: integer;
begin
  i := posLiBox.Count - 1;
  posLiBox.Items.Add(AString);

  case AString[2] of
  '0'..'9':
    begin
      num := strtoint(copy(AString,2));
      while (i >= 0) and (AString[1] <= posLiBox.Items.Strings[i][1]) do
        if (posLiBox.Items.Strings[i][2] > '9')
           or
           (num < strtoint(copy(posLiBox.Items.Strings[i],2))) then
        begin
          posLiBox.Items.Move(i + 1, i);
          dec(i);
        end
        else
          break;
    end;
  else
    while (i >= 0) and (AString < posLiBox.Items.Strings[i]) do
    begin
      posLiBox.Items.Move(i + 1, i);
      dec(i);
    end;
  end;
  newEd.Text := '';
end;

procedure TSettingsForm.AddNewPos;
var f: boolean;
begin
  f := false;
  if (length(newEd.Text) > 1) then
    if (ord(newEd.Text[1]) >= 65) and (ord(newEd.Text[1]) <= 90) then
      case newEd.Text[2] of
       'A'..'Z','a'..'z':
         begin
           f := (length(newEd.Text) = 2);
           if not f then
             f := ((ord(newEd.Text[3]) >= 65) and (ord(newEd.Text[3]) <= 90))
                  or
                  ((ord(newEd.Text[3]) >= 97) and (ord(newEd.Text[3]) <= 122));
           if not f then
             MessageDlg('The third character is invalid!', mtError, [mbOK], 0);
         end;
       '0'..'9':
         begin
           f := (length(newEd.Text) = 2);
           if not f then
             f := (ord(newEd.Text[3]) >= 48) and (ord(newEd.Text[3]) <= 57);
           if not f then
             MessageDlg('The third character is invalid!', mtError, [mbOK], 0);
         end;
       else
         MessageDlg('The second character is invalid!', mtError, [mbOK], 0);
      end
    else
      MessageDlg('Positions have to start with an uppercase letter!', mtError, [mbOK], 0)
  else
    MessageDlg('The position term is to short!', mtError, [mbOK], 0);

  if f then
  begin
    InsertPos(newEd.Text);
    ApplyBtn.Enabled := true;
    FPosSaved := false;
  end;
end;

procedure TSettingsForm.DeletePos;
begin
  if (posLiBox.ItemIndex > -1) then
  begin
    posLiBox.DeleteSelected;
    ApplyBtn.Enabled := true;
    FPosSaved := false;
  end;
end;

procedure TSettingsForm.ApplyValues;
begin
  if not (awakeCoBox.ItemIndex  = -1) then
   if not (sleepCoBox.ItemIndex  = -1) then
    if not (passwCoBox.ItemIndex  = -1) then
     if not (trim(EdDataSource.Text) = '') then
      if not (trim(dbuserEd.Text)   = '') then
       if not (trim(dbpwEd.Text)     = '') then
        if not (deleteCoBox.ItemIndex = -1) then
        begin
          if (MessageDlg('Do you really want to save?',
                         mtCustom, [mbYes, mbNo], 0) = mrYes) then
          begin
            if not FPosSaved then
              SavePositionList;
            SaveSystemProperties;

            ApplyBtn.Enabled := false;
          end;
        end
        else MessageDlg('Please select an age for deleting measurements!', mtError, [mbOK],0)
       else MessageDlg('Invalid Database Password!', mtError, [mbOK],0)
      else MessageDlg('Invalid Database Username!', mtError, [mbOK],0)
     else MessageDlg('Invalid Database Server!', mtError, [mbOK],0)
    else MessageDlg('Invalid Levelmeter Password!', mtError, [mbOK],0)
   else MessageDlg('Invalid XBee sleep time!', mtError, [mbOK],0)
  else MessageDlg('Invalid XBee awake time!', mtError, [mbOK],0);
end;

procedure TSettingsForm.BtnEditDSClick(Sender: TObject);
var
  lDataSource: WideString;
  lFactories: IDataModule;
  lIntf: IInterface;
  lFactoryID: TGUID;
  lFactory: ISQLImplementorFactory;
  lClient: ISQLClient;
begin
  lDataSource := EdDataSource.Text;
  lFactories := GetSQLClientFactories;
  if Assigned(lFactories) and (selectdbtypeCoBox.ItemIndex > -1) then
    try
      lFactoryID := PGUID(selectdbtypeCoBox.Items.Objects[selectdbtypeCoBox.ItemIndex])^;
      lFactories.GetInterfaceValue(PChar(GUIDToString(lFactoryID)), lIntf);      
      if supports(lIntf, ISQLImplementorFactory, lFactory) then 
      begin
        lClient := lFactory.CreateClient;
        if Assigned(lClient) and Assigned(lClient.GetSession) and
           lClient.GetSession.ShowDataSourceDialog(lDataSource) then
          EdDataSource.Text := lDataSource;
      end;
    finally
      lClient := nil;
      lFactory := nil;
      lIntf := nil;
      lFactories := nil;
    end;
end;

procedure TSettingsForm.SavePositionList;
var
  i, Nr: integer;
  LReg:  TRegistry;
  Pos:   string;

  function RegNr(const ANr: integer): string;
  begin
    Result := 'he_pos';

    if (ANr < 10) then
      Result := Result + '0' + intToStr(ANr)
    else
      Result := Result + intToStr(ANr);
  end;

begin
  LReg := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  try
    Nr  := 1;
    Pos := '';
    if LReg.KeyExists('Software\HZB_Helium\Position\')
      then LReg.DeleteKey('Software\HZB_Helium\Position\');
    LReg.OpenKey('Software\HZB_Helium\Position\', true);

    for i := 0 to posLiBox.Count - 1 do
      case posLiBox.Items.Strings[i][2] of
        'A'..'Z','a'..'z':
          begin
            if (Pos <> '') then
            begin
              LReg.WriteString(RegNr(Nr), Pos);
              Pos := '';
              inc(Nr);
            end;
            LReg.WriteString(RegNr(Nr), posLiBox.Items.Strings[i]);
            inc(Nr);
          end;

        '0'..'9':
          begin
            if (Pos <> '') then
              if (Pos[1] = posLiBox.Items.Strings[i][1]) then
                Pos := Pos + ',' + copy(posLiBox.Items.Strings[i],2)
              else begin
                LReg.WriteString(RegNr(Nr), Pos);
                inc(Nr);
                Pos := posLiBox.Items.Strings[i];
                insert(';', Pos, 2);
              end
            else begin
              Pos := posLiBox.Items.Strings[i];
              insert(';', Pos, 2);
            end;
          end;
      end;


      if (Pos <> '') then
        LReg.WriteString(RegNr(Nr), Pos);
  finally
    LReg.CloseKey;
  end;
  LReg.Free;
  
  FPosSaved   := true;
  FPosChanged := true;
end;

procedure TSettingsForm.SaveSystemProperties;
var
  LReg: TRegistry;
  lIndex: Integer;
begin
  LReg := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  try
    LReg.OpenKey('Software\HZB_Helium\',true);

    LReg.WriteInteger('AwakeTime', awakeCoBox.ItemIndex + 1);
    LReg.WriteInteger('SleepTime', sleepCoBox.ItemIndex + 1);
    LReg.WriteInteger('OptionsPW', strToInt(passwCoBox.Text));
    if True then
    
    LReg.WriteString( 'DbServer',  EdDataSource.Text);
    LReg.WriteString( 'DbUser',    dbuserEd.Text);
    LReg.WriteString( 'DbKey',     encrypt(dbpwEd.Text));
    LReg.WriteInteger('DeleteOld', deleteCoBox.ItemIndex + 1);
    LReg.WriteBool(   'DebugMode', debugChBox.Checked);
    LReg.WriteInteger('CoordRetryAttmepts', RetryAttemptsSpinEdit.Value);
    
    lIndex := selectdbTypeCoBox.ItemIndex;
    if lIndex > -1 then
    begin
      LReg.WriteString( 'SQLClientId', selectdbTypeCoBox.Text);
      LReg.WriteString( 'SQLClientClassId', GUIDToString(PGUID(selectdbTypeCoBox.Items.Objects[lIndex])^));
    end;
  finally
    LReg.CloseKey;
  end;
  LReg.Free;
end;


procedure TSettingsForm.AddSpeedBtnClick(Sender: TObject);
begin
  AddNewPos;
end;

procedure TSettingsForm.DelSpeedBtnClick(Sender: TObject);
begin
  DeletePos;
end;


procedure TSettingsForm.cOkBtnClick(Sender: TObject);
begin
  if ApplyBtn.Enabled then
    ApplyValues;

  if not ApplyBtn.Enabled then
    ModalResult := mrOK;
end;

procedure TSettingsForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  if ApplyBtn.Enabled then
    if (MessageDlg('If you cancel your changes will get lost.' + #13
                   + 'Do you want to cancel anyway?',
                   mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      ApplyBtn.Enabled := false
    else
      ModalResult := mrNone;   
end;

procedure TSettingsForm.CleanUp;
var
  I: Integer;
  pData: PGUID;
begin
  for I := 0 to selectdbTypeCoBox.Items.Count - 1 do
    if Assigned(selectdbTypeCoBox.Items.Objects[I]) then
    begin
      TObject(pData) := selectdbTypeCoBox.Items.Objects[I];
      dispose(pData);
    end;
end;

procedure TSettingsForm.ApplyBtnClick(Sender: TObject);
begin
  ApplyValues;
end;

end.

