unit HeliumMain;

interface

uses
  SysUtils, Windows, Classes, Graphics, Controls, Forms, StdCtrls, SyncObjs,
  Dialogs, Contnrs,  ExtCtrls, ComCtrls, Menus, Grids, StrUtils, Registry, jpeg,
  ShellAPI, CPort, PlotFormular, HeliumDataMgmt, SQLClientInterfaces;

type
  TMsgEvent = procedure(AMsg: string) of object;

  TCoordinatorJob = class(TObject)
    jobType:    byte;
    jobRequest: string;
    jobAddress: string;
  end;

  TMainFormJob = class(TObject)
    jobType: byte;
    jobId:   integer;
    jobText: string;
  end;

  TVirtualComPort = class(TObject)
  private
    FOnDataRx:  TMsgEvent;
    FComPort:   TComPort;
    
    procedure   SerialDataRxChar(ASender: TObject; Count: integer);
    procedure   DataReceived(const AData: string);
    function    GetPort: string;
    function    IsOpened: boolean;
  public
    constructor CreateWithProperties(const APort: string);
    destructor  Destroy; override;
    function    Reopen: boolean;
    procedure   Close;
    procedure   WriteData(const AString: string);
  published
    property    Port:     string    read GetPort;
    property    Opened:   boolean   read IsOpened;
    property    OnDataRx: TMsgEvent read FOnDataRx write FOnDataRx;
  end;

  TCoordinatorThread = class(TThread)
  private
    FDestroying: Integer;
    FLock:      TCriticalSection;
    FComPort:   TVirtualComPort;
    FDataMgmt:  TDataMgmt;
    FQueue:     TObjectList;
    FCooId,
    FRetryAttempts: integer;
    FFrame:     string;
    FFrameLen:  integer;
    FStartTime: TDateTime;

    procedure CreateMainFormJob(const AType: byte; const AText: string);
    procedure OnDataRxHandler(AData: string);
    procedure WorkThroughQueue;
    procedure ConfigComPort;
    procedure DataToModule(  AJob: TCoordinatorJob);
    procedure DataFromCoo(AJob: TCoordinatorJob);
    function CheckStatus: boolean;
    procedure CheckXBeeStatus(AJob: TCoordinatorJob);
    procedure DbDisconnect;

    function  CheckChkSum(const AFrame: string): boolean;
    function  CalcChkSum(const AString: string): char;

    procedure RXPacket(const AFrame: string);
    procedure ATResponse(const AFrame: string);

    procedure MsgLogin(      const AFrame: string);
    procedure MsgStatus(     const AFrame: string);
    procedure MsgOptions(    const AFrame: string);
    procedure MsgFillBegin(  const AFrame: string);
    procedure MsgActive(     const AFrame: string);
    procedure MsgError(      const AFrame: string);
    procedure MsgPositions(  const AFrame: string);
    procedure MsgPassword(   const AFrame: string);
    procedure MsgAwakeTime(  const AFrame: string);
    procedure MsgSleepTime(  const AFrame: string);
    procedure MsgDeviceLvl(  const AFrame: string);

    function  ReadAddress(const AFrame: string): string;
    function  AddressToHexString(const AAddress8: string): string;
    function  HexStringToAddress(const AAddress16: string): string;

    procedure TXRequest(const AMsg, AAddress: string);
    procedure ATCommand(const AMsg: string);

    function  HexStrToInt(const HexStr: string): integer;
    function  SqrX(const basis: real; const exponent: integer): real;

    function  ChangedVesselOptions(const AFrame: string; AVessel: TVessel): boolean;
    function  ChangedModuleOptions(const AFrame: string; AModule: TModule): boolean;
    function  GetPassword: string;
    function  GetAwakeTime: string;
    function  GetSleepTime: string;
    function  GetComPort(var AName: string): string;
    function  GetPositions: string;

    procedure LoginOriginal(AVessel: TVessel; AModule: TModule);
    procedure LogoutOriginal(AModule: TModule);

    function  CreateNewVessel(const AName: word): TVessel;

    function  ModNumTo1Byte(const AName: string): string;
    function  ModPosTo3Byte(const APosition: string): string;

    procedure MeasurementToReg(const ACode: char; AVessel: TVessel; AModule: TModule;
      const ALevel: double; const APressure: word; const AAccu: byte);
    procedure CryoMeasurementToReg(const ADevId: integer;
                                   const AHeLevel, AN2Level1, AN2Level2: double);

    procedure CheckRequestsAndStatusByte(AModule: TModule; const AAddress: string);

    procedure SetDefaultOptions(AVessel: TVessel);

    function  VesOptions(AVessel: TVessel): string;
    function  ModOptions(AModule: TModule): string;

    function  PositionListIsOk(const AFrame: string): boolean;
    function  PasswordIsOk(const APassword: word): boolean;

    function  AwakeTimeIsOk(const AAwakeTime: byte): boolean;
    function  SleepTimeIsOk(const ASleepTime: byte): boolean;
    function GetRetryAttempts: Integer;
    procedure SetRetryAttempts(const Value: Integer);
    function GetDestroying: boolean;
    procedure SetDestroying(const AValue: boolean);
  protected
    procedure Execute; override;
    property Destroying: boolean read GetDestroying;
  public
    constructor CreateWithProperties(const ACooId: integer; ADataMgmt: TDataMgmt);
    destructor  Destroy; override;
    procedure   CreateCoordinatorJob(const AType: byte; const ARequest, AAddress: string);

    property Id: integer          read FCooId;
    property StartTime: TDateTime read FStartTime;
    property RetryAttempts: Integer read GetRetryAttempts write SetRetryAttempts;
  end;

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    OptionMeIt: TMenuItem;
    CloseMeIt: TMenuItem;
    ModuleMeIt: TMenuItem;
    CoordinatorMeIt: TMenuItem;
    SettingsMeIt: TMenuItem;
    VesselMeIt: TMenuItem;
    Help1: TMenuItem;
    AboutMeIt: TMenuItem;
    PageControl: TPageControl;
    vesTabSh: TTabSheet;
    modTabSh: TTabSheet;
    cooTabSh: TTabSheet;
    cooStrGr: TStringGrid;
    cooImg: TImage;
    modImg: TImage;
    modStrGr: TStringGrid;
    vesImg: TImage;
    vesStrGr: TStringGrid;
    vesPopupMenu: TPopupMenu;
    modPopupMenu: TPopupMenu;
    VesselTypeMeIt: TMenuItem;
    VesselSettingsMeIt: TMenuItem;
    ModuleSettingsMeIt: TMenuItem;
    lvLog: TListView;
    workTimer: TTimer;
    N1: TMenuItem;
    ChangedVesOptionsMeIt: TMenuItem;
    XBeeMeIt: TMenuItem;
    LogPopupMenu: TPopupMenu;
    ClearHistoryMeIt: TMenuItem;
    ChangedModOptionsMeIt: TMenuItem;
    RefreshMeIt: TMenuItem;
    N2: TMenuItem;
    vesActiveRBtn: TRadioButton;
    vesAllRBtn: TRadioButton;
    modActiveRBtn: TRadioButton;
    modAllRBtn: TRadioButton;
    cooActiveRBtn: TRadioButton;
    cooAllRBtn: TRadioButton;
    N3: TMenuItem;
    DefaultsMeIt: TMenuItem;
    ErrorLogMeIt: TMenuItem;
    N6: TMenuItem;
    N4: TMenuItem;
    GetStatusMeIt: TMenuItem;
    GetOptionsMeIt: TMenuItem;
    SetOptionsMeIt: TMenuItem;
    GetPosMeIt: TMenuItem;
    SetPosMeIt: TMenuItem;
    GetPasswordMeIt: TMenuItem;
    SetPasswordMeIt: TMenuItem;
    GetSleepTimeMeIt: TMenuItem;
    SetSleepTimeMeIt: TMenuItem;
    SendTextMsgMeIt: TMenuItem;
    timeoutTimer: TTimer;
    vesHzbRBtn: TRadioButton;
    GetAwakeTimeMeIt: TMenuItem;
    SetAwakeTimeMeIt: TMenuItem;
    N5: TMenuItem;
    cooPopupMenu: TPopupMenu;
    CheckStatusMeIt: TMenuItem;
    VesselHistoryDMeIt: TMenuItem;
    N7: TMenuItem;
    VesselHistoryHMeIt: TMenuItem;
    devTabSh: TTabSheet;
    devStrGr: TStringGrid;
    devImg: TImage;
    CheckXBeeMeIt: TMenuItem;
    IlmDeviceMeIt: TMenuItem;
    cryPopupMenu: TPopupMenu;
    GasHistoryMeIt: TMenuItem;
    TimePanel: TPanel;
    TimeLab: TLabel;
    Shape1: TShape;
    N8: TMenuItem;
    ActivateMeIt: TMenuItem;
    DeactivateMeIt: TMenuItem;
    BtnConnectAll: TButton;
    BtnDisconnectAll: TButton;
    LabNoDbConnectionHint: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ModuleMeItClick(Sender: TObject);
    procedure CoordinatorMeItClick(Sender: TObject);
    procedure VesselMeItClick(Sender: TObject);
    procedure IlmDeviceMeItClick(Sender: TObject);
    procedure AboutMeItClick(Sender: TObject);
    procedure cooStrGrMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vesStrGrMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure modStrGrMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VesselTypeMeItClick(Sender: TObject);
    procedure ModuleSettingsMeItClick(Sender: TObject);
    procedure VesselSettingsMeItClick(Sender: TObject);
    procedure vesStrGrExit(Sender: TObject);
    procedure modStrGrExit(Sender: TObject);
    procedure cooStrGrExit(Sender: TObject);
    procedure devStrGrExit(Sender: TObject);
    procedure workTimerTimer(Sender: TObject);
    procedure ChangedVesOptionsMeItClick(Sender: TObject);
    procedure XBeeMeItClick(Sender: TObject);
    procedure StrGrMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ChangedModOptionsMeItClick(Sender: TObject);
    procedure vesTabShResize(Sender: TObject);
    procedure modTabShResize(Sender: TObject);
    procedure cooTabShResize(Sender: TObject);
    procedure devTabShResize(Sender: TObject);
    procedure vesRBtnClick(Sender: TObject);
    procedure modRBtnClick(Sender: TObject);
    procedure cooRBtnClick(Sender: TObject);
    procedure RefreshMeItClick(Sender: TObject);
    procedure CloseMeItClick(Sender: TObject);
    procedure DefaultsMeItClick(Sender: TObject);
    procedure ErrorLogMeItClick(Sender: TObject);
    procedure ClearHistoryMeItClick(Sender: TObject);
    procedure cooStrGrDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure modStrGrDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure vesStrGrDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure devStrGrDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);  
    procedure GetStatusMeItClick(Sender: TObject);
    procedure GetOptionsMeItClick(Sender: TObject);
    procedure SetOptionsMeItClick(Sender: TObject);
    procedure GetPosMeItClick(Sender: TObject);
    procedure SetPosMeItClick(Sender: TObject);
    procedure GetPasswordMeItClick(Sender: TObject);
    procedure SetPasswordMeItClick(Sender: TObject);
    procedure GetSleepTimeMeItClick(Sender: TObject);
    procedure SetSleepTimeMeItClick(Sender: TObject);
    procedure SendTextMsgMeItClick(Sender: TObject);
    procedure timeoutTimerTimer(Sender: TObject);
    procedure GetAwakeTimeMeItClick(Sender: TObject);
    procedure SetAwakeTimeMeItClick(Sender: TObject);
    procedure CheckStatusMeItClick(Sender: TObject);
    procedure VesselHistoryDMeItClick(Sender: TObject);
    procedure VesselHistoryHMeItClick(Sender: TObject);
    procedure CheckXBeeMeItClick(Sender: TObject);
    procedure devStrGrMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GasHistoryMeItClick(Sender: TObject);
    procedure cooStrGrMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TimeLabClick(Sender: TObject);
    procedure ActivateMeItClick(Sender: TObject);
    procedure DeactivateMeItClick(Sender: TObject);
    procedure PageControlDrawTab(Control: TCustomTabControl; TabIndex: Integer;
      const Rect: TRect; Active: Boolean);
    procedure MeItAdvancedDrawItem(Sender: TObject;
      ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    procedure BtnConnectAllClick(Sender: TObject);
    procedure BtnDisconnectAllClick(Sender: TObject);
    private
    FTimerCounter:  byte;
    FPlotForm:      TPlotForm;
    FMainLock:      TCriticalSection;
    FQueue:         TObjectList;
    FCooThreadList: TObjectList;
    FDataMgmt:      TDataMgmt;
    FStartTime:     TDateTime;

    function   vesOptionChangesExists: boolean;
    function   modOptionChangesExists: boolean;

    procedure  CreateMainFormJob(AType: byte; AId: integer; AText: string);
    procedure  WorkThroughQueue;

    procedure  ErrorMsg(const AMsg: string);
    procedure  DebugMsg(const AMsg: string);

    function   CreateCooThread(const AId: integer; const APort: string): boolean;
    procedure  KillCooThread(AJob: TMainFormJob);

    procedure  RefreshVesList;
    procedure  RefreshModList;
    procedure  RefreshCooList;
    procedure  RefreshDevList;

    procedure  RefreshVesTab;
    procedure  RefreshModTab;
    procedure  RefreshCooTab;
    procedure  RefreshDevTab;

    procedure  VesOptions(const AMsg: string);
    procedure  ModOptions(const AMsg: string);
                                
    procedure  LogoutVessel(const AVesId: integer);
    procedure  LogoutModule(const AModId: integer);

    procedure  CheckCommunicationTimeout;
    procedure  CheckDatabaseFunction;
    procedure  RetryMeasurementUpload;

    procedure  CheckStatus(const ACooId: integer);
    procedure  CheckStatusForAll;
    procedure  CheckXBee(const ACooId: integer);

    procedure  DbDisconnect;

    function   GetRowOfCooStrGr(const AId: string): integer;

    function   GetThreadListIndex(const AId: integer): integer;
    function   GetIdInActiveRow(AStringGrid: TStringGrid; const ACol: integer): integer;

    procedure  ShowVesselForm;
    procedure  ShowModuleForm;
    procedure  ShowCoordinatorForm;
    procedure  ShowIlmDeviceForm;

    procedure  LogListView(const S: string);
    procedure  LogErrFile(const S: string);

    procedure  GetMeasurements(const AQuery: string; var AFillList, AStayList: TObjectList); overload;
    procedure  GetMeasurements(const AQuery: string; var AFillList: TObjectList); overload;
    procedure  GetIlmMeasurements(const AQuery: string; var AList: TObjectList);

    procedure  GetPlotData(const AList: TObjectList; var AxVal, AyVal: Array of single); overload;
    procedure  GetPlotData(const AChannel: byte; const AList: TObjectList; var AxVal, AyVal: Array of single); overload;
    function   GetHeLoss(const AQuery: string): TObjectList;
    procedure ApplyCoordRetryAttempts(const ACoordIndex: Integer = -1);
    procedure SetFrontPanelLocked(const AValue: Boolean);
  public
    destructor Destroy; override;
    procedure  AddMainFormJob(AJob: TMainFormJob);
  end;

var MainForm: TMainForm;

implementation
uses IdGlobalProtocols, Vessel, XBeeModule, XBeeCoordinator, IlmDevice,
     Settings, Defaults, OptionsVessel, OptionsModule, About, TextMsg,
     HeliumFunctions, EvaluateDb, Plot, UtilDelph;
     
{$R *.dfm}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//   << T M a i n - F o r m >>   - - - - - - - - - - - - - - - - - - - - - - - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TMainForm.FormCreate(Sender: TObject);
var fMenuBrush: TBrush;
    MenuInfo:   TMenuInfo;
begin
  fMenuBrush       := TBrush.Create;
  fMenuBrush.Color := clMenuBar;//BtnHighLight;
  MenuInfo.cbSize  := SizeOf(MenuInfo);
  MenuInfo.hbrBack := fMenuBrush.Handle;
  MenuInfo.fMask   := MIM_BACKGROUND;
  SetMenuInfo(MainMenu.Handle, MenuInfo);

  FStartTime      := now;
 
  FTimerCounter  := 0;
  FMainLock      := TCriticalSection.Create;
  FQueue         := TObjectList.Create(true);
  FCooThreadList := TObjectList.Create(true);
  FDataMgmt      := TDataMgmt.Create(ErrorMsg);

  // Hide NotConnectedLabel
  LabNoDbConnectionHint.Visible := False;
  // Vessel StringGrid
  vesStrGr.Cells[0,0] := ' Active';
  vesStrGr.Cells[1,0] := 'Name';
  vesStrGr.Cells[2,0] := 'He Level';
  vesStrGr.Cells[3,0] := 'Pressure';
  vesStrGr.Cells[4,0] := 'Position';
  vesStrGr.Cells[5,0] := 'Status';
  vesStrGr.Cells[6,0] := 'Comment';

  // Module StringGrid
  modStrGr.Cells[0,0] := ' Active';
  modStrGr.Cells[1,0] := 'Name';
  modStrGr.Cells[2,0] := 'Vessel';
  modStrGr.Cells[3,0] := 'Position';
  modStrGr.Cells[4,0] := 'Battery';
  modStrGr.Cells[5,0] := 'Comment';

  // Coordinator StringGrid
  cooStrGr.Cells[0,0] := ' Active';
  cooStrGr.Cells[1,0] := 'Name';
  cooStrGr.Cells[2,0] := 'Position';
  cooStrGr.Cells[3,0] := 'COM Port';
  cooStrGr.Cells[4,0] := 'Comment';

  // Device StringGrid
  devStrGr.Cells[0,0] := ' Active';
  devStrGr.Cells[1,0] := 'Name';
  devStrGr.Cells[2,0] := 'Channel 1';
  devStrGr.Cells[3,0] := 'Channel 2';
  devStrGr.Cells[4,0] := 'Channel 3';
  devStrGr.Cells[5,0] := 'Last Message';
  devStrGr.Cells[6,0] := 'Position';
  devStrGr.Cells[7,0] := 'Comment';       

  // Changes in Options
  ChangedVesOptionsMeIt.Enabled := vesOptionChangesExists;
  ChangedModOptionsMeIt.Enabled := modOptionChangesExists;

  if FDataMgmt.Init then
  begin
    // Refresh DataMgmt
    CreateMainFormJob(4,0,'');
    CreateMainFormJob(5,0,'');
    CreateMainFormJob(6,0,'');
    CreateMainFormJob(21,0,'');

    // Refresh Table View
    CreateMainFormJob(7,0,'');
    CreateMainFormJob(8,0,'');
    CreateMainFormJob(9,0,'');
    CreateMainFormJob(20,0,'');
    SetFrontPanelLocked(False);
  end
  else
    SetFrontPanelLocked(True);

  FPlotForm         := TPlotForm.Create(self);
  FPlotForm.Caption := Caption;
  FPlotForm.Hide;
end;

destructor TMainForm.Destroy;
begin
  try
    FMainLock.Acquire;
    FPlotForm.Free;
    FQueue.Free;
    FCooThreadList.Free;
    if Assigned(FDataMgmt) then
      FDataMgmt.Free;
  finally
    FMainLock.Release;
    FMainLock.Free;
  end;
  
  inherited Destroy;
end;


function TMainForm.vesOptionChangesExists: boolean;
var LReg:   TRegistry;
    LNamen: TStringList;
begin
  LNamen       := nil;
  LReg         := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  Result       := LReg.KeyExists('Software\HZB_Helium\New_VesOptions\');
  if Result then
  try
    LNamen := TStringList.Create;
    LReg.OpenKey('Software\HZB_Helium\New_VesOptions\', false);
    LReg.GetKeyNames(LNamen);
    Result := (LNamen.Count > 0);
  finally
    LReg.CloseKey;
    if assigned(LNamen) then LNamen.Free;
  end;
  LReg.Free;
end;

function TMainForm.modOptionChangesExists: boolean;
var LReg:   TRegistry;
    LNamen: TStringList;
begin
  LNamen       := nil;
  LReg         := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  Result       := LReg.KeyExists('Software\HZB_Helium\New_ModOptions\');
  if Result then
  try
    LNamen := TStringList.Create;
    LReg.OpenKey('Software\HZB_Helium\New_ModOptions\', false);
    LReg.GetKeyNames(LNamen);
    Result := (LNamen.Count > 0);
  finally
    LReg.CloseKey;
    if assigned(LNamen) then LNamen.Free;
  end;
  LReg.Free;
end;


procedure TMainForm.CreateMainFormJob(AType: byte; AId: integer; AText: string);
var LJob: TMainFormJob;
begin
  LJob         := TMainFormJob.Create;
  LJob.jobType := AType;
  LJob.jobId   := AId;
  LJob.jobText := AText;
  AddMainFormJob(LJob);
end;

procedure TMainForm.ActivateMeItClick(Sender: TObject);
begin
  with cooStrGr do
    if (Cells[0, Row] = 'false') then
      if CreateCooThread(strToInt(Cells[5, Row]), Cells[3, Row]) then
        Cells[0, Row] := 'true';
end;

procedure TMainForm.AddMainFormJob(AJob: TMainFormJob);
begin
  try
    FMainLock.Acquire;
    FQueue.Add(AJob);
  finally
    FMainLock.Release;
  end;
end;

procedure TMainForm.ApplyCoordRetryAttempts(const ACoordIndex: Integer);
var
  I: Integer;
  R: TRegistry;
begin
  if ACoordIndex < 0 then
    for I := 0 to FCooThreadList.Count - 1 do
      ApplyCoordRetryAttempts(I)
  else
  begin
    R := TRegistry.Create;
    try
      FDataMgmt.LockWhileWorkingWithList;
      if (ACoordIndex < FCooThreadList.Count) and R.KeyExists('Software\HZB_Helium\') then
      begin
        R.OpenKey('Software\HZB_Helium\', false);
        if R.ValueExists('CoordRetryAttmepts') then
          TCoordinatorThread(FCooThreadList[ACoordIndex]).RetryAttempts := R.ReadInteger('CoordRetryAttmepts');
      end;
    finally
      FDataMgmt.UnlockAfterWorkingWithList;
      R.Free;
    end;
  end;
end;

procedure TMainForm.BtnConnectAllClick(Sender: TObject);
var
  I: Integer;
begin
  FDataMgmt.LockWhileWorkingWithList;
  try
    for I := 0 to FDataMgmt.CoordinatorList.Count - 1 do
      with TCoordinator(FDataMgmt.CoordinatorList[I]) do
        if coo_status = 0 then
          CreateCooThread(coo_id, coo_comport);
    CreateMainFormJob(9,0,'');
  finally
    FDataMgmt.UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.BtnDisconnectAllClick(Sender: TObject);
var
  I: Integer;
begin
  FDataMgmt.LockWhileWorkingWithList;
  try
    for I := 0 to FDataMgmt.CoordinatorList.Count - 1 do
      with TCoordinator(FDataMgmt.CoordinatorList[I]) do
        if coo_status <> 0 then
        begin
          CreateMainFormJob(3,coo_id ,'');
          CreateMainFormJob(0, 0, coo_name + ' disabled.');
        end;
    CreateMainFormJob(9,0,'');
  finally
    FDataMgmt.UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.workTimerTimer(Sender: TObject);

  function QueueCount: Integer;
  begin
    FMainLock.Enter;
    try
      result := FQueue.Count;
    finally
      FMainLock.Leave;
    end;
  end;

begin
  workTimer.Enabled := false;
  if (QueueCount > 0) then WorkThroughQueue;
  workTimer.Enabled := true;
end;

procedure TMainForm.WorkThroughQueue;              

  function QueueCount: Integer;
  begin
    FMainLock.Enter;
    try
      result := FQueue.Count;
    finally
      FMainLock.Leave;
    end;
  end;

  function QueuePop: TMainFormJob;
  begin
    result := nil;
    FMainLock.Enter;
    try
      if FQueue.Count > 0 then
      begin
        TObject(result) := FQueue[0];
        FQueue.OwnsObjects := false;
        FQueue.Delete(0);            
        FQueue.OwnsObjects := true;
      end;
    finally
      FMainLock.Leave;
    end;
  end;

var
  lJob: TMainFormJob;
begin
  while (QueueCount > 0) do
  begin
    lJob := QueuePop;
    try
      case lJob.jobType of
      { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
       0 : LogListView(lJob.jobText); // Info  Msg
       1 : ErrorMsg(lJob.jobText);    // Error Msg
       2 : DebugMsg(lJob.jobText);    // Debug Msg
      { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
       3 : KillCooThread(lJob);       // Kill Coord. Thread
      { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
       4 : RefreshVesList;                               // Refresh DataMgmt
       5 : RefreshModList;                               // Refresh DataMgmt
       6 : RefreshCooList;                               // Refresh DataMgmt
      { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
       7 : RefreshVesTab;                                // Refresh StringGrid
       8 : RefreshModTab;                                // Refresh StringGrid
       9 : RefreshCooTab;                                // Refresh StringGrid
      { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
       10: VesOptions(lJob.jobText);  // Changed Options
       11: ModOptions(lJob.jobText);  // Changed Options
      { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
       12: LogoutVessel(lJob.jobId);  // Logout Module
       13: LogoutModule(lJob.jobId);  // Logout Module
      { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
       14: CheckCommunicationTimeout;                    // Check Timeout
       15: CheckDatabaseFunction;                        // Check Database
       16: RetryMeasurementUpload;                       // Check Registry
      { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
       17: CheckStatus(lJob.jobId);   // Check Thread by Id
       18: CheckStatusForAll;                            // Check all Threads
       19: CheckXBee(lJob.jobId);     // Check XBee by Id
      { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
       20: RefreshDevTab;                                // Refresh StringGrid
       21: RefreshDevList;                               // Refresh DataMgmt
      else ErrorMsg('Unknown job type ('
                    + intToStr(lJob.jobType)
                    + ') in MainQueue!');
      end;
    finally
      lJob.Free;
      if (QueueCount = 0) then DbDisconnect;
    end;
  end;
end;


procedure TMainForm.ErrorMsg(const AMsg: string);
begin
  LogErrFile(AMsg);
  LogListView(AMsg);
end;

procedure TMainForm.DeactivateMeItClick(Sender: TObject);
begin
  with cooStrGr do
    if (Cells[0, Row] = 'true') then
      if (MessageDlg('Do you really want to stop ' + Cells[1, Row] + '?',
                     mtCustom, [mbYes, mbNo], 0) = mrYes) then
      begin
        CreateMainFormJob(3, strToInt(Cells[5, Row]), '');
        CreateMainFormJob(0, 0, Cells[1, Row] + ' disabled.');
      end;
end;

procedure TMainForm.DebugMsg(const AMsg: string);
begin
  if getRegBoolean('','DebugMode')
   then LogListView('+++ Debug Msg +++ ' + #39 + AMsg + #39);
end;


function TMainForm.CreateCooThread(const AId: integer; const APort: string): boolean;
var LCoo: TCoordinator;
begin
  // Coordinator Port
  Result := (AId > 0) and portExists(APort);

  // Coordinator Thread List
  if Result then
    ApplyCoordRetryAttempts(FCooThreadList.Add(TCoordinatorThread.CreateWithProperties(AId, FDataMgmt)))
  else
    LogListView(APort + ' is not configured on this machine!');

  // Coordinator Data Management
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LCoo := GetCooItem(AId);
    if Assigned(LCoo) then
    begin
      if Result then
        LCoo.coo_status := 1
      else
        LCoo.coo_status := 0;

      LCoo.coo_xbeestatus := 0;
    end;
  finally
    FDataMgmt.UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.KillCooThread(AJob: TMainFormJob);
var LId, i: integer;
    LCoo:   TCoordinator;
begin
  ErrorMsg(AJob.jobText);
  LId := AJob.jobId;

  // Coordinator Thread List
  i := GetThreadListIndex(LId);
  if (i > -1) then FCooThreadList.Delete(i);

  // Coordinator String Grid
  i := GetRowOfCooStrGr(intToStr(LId));
  if (i > -1) then
  begin
    if cooActiveRBtn.Checked then deleteRow(cooStrGr, i);
    if cooAllRBtn.Checked then cooStrGr.Cells[0,i] := 'false';
  end;

  // Coordinator Data Management
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LCoo :=  GetCooItem(LId);

    if Assigned(LCoo) then
    begin
      if (LCoo.coo_status = 2) then CreateMainFormJob(9,0,'');
      LCoo.coo_status     := 0;
      LCoo.coo_xbeestatus := 0;
    end;
  finally
    FDataMgmt.UnlockAfterWorkingWithList;
  end;
end;


procedure TMainForm.RefreshVesList;
begin
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    RefreshVesListByDB;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.RefreshModList;
begin
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    RefreshModListByDB;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.RefreshCooList;
begin
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    RefreshCooListByDB;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.RefreshDevList;
begin
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    RefreshDevListByDB;
  finally
    UnlockAfterWorkingWithList;
  end;
end;


procedure TMainForm.RefreshVesTab;
var i, j, LCount: integer;
    LModule:      TModule;
    LVessel:      TVessel;
begin
  LCount := 0;

  with FDataMgmt, vesStrGr do
  try
    for i := 1 to RowCount do Rows[i].Clear;
    RowCount := 2;

    LockWhileWorkingWithList;
    for i := 0 to VesselList.Count - 1 do
    begin
      LVessel := TVessel(VesselList[i]);
      LModule := nil;

      for j := 0 to ModuleList.Count - 1 do
        if (TModule(ModuleList[j]).mod_vessel = LVessel) then
        begin
          LModule := TModule(ModuleList[j]);
          break;
        end;

      if (vesActiveRBtn.Checked and Assigned(LModule) and (LModule.mod_status > 0))
         or (vesHzbRBtn.Checked and not (LVessel.ves_at_hzb_since = ''))
         or (vesAllRBtn.Checked) then
      begin
        if not (LCount = 0) then RowCount := RowCount + 1;
        inc(LCount);

        Cells[1,RowCount-1] := intToStr(LVessel.ves_name);
        if (LVessel.ves_he_level < 200) then
          Cells[2,RowCount-1] := floatToStr(LVessel.ves_he_level) + ' %';
        if (LVessel.ves_pressure > 0) then
          Cells[3,RowCount-1] := intToStr(LVessel.ves_pressure) + ' mbar';
        Cells[6,RowCount-1] := LVessel.ves_comment2;
        Cells[7,RowCount-1] := intToStr(LVessel.ves_id);

        if (Assigned(LModule) and (LModule.mod_status > 0)) then
        begin
          Cells[0,RowCount-1] := 'true';
          if (LVessel.ves_position = '') then
            Cells[4,RowCount-1] := LModule.mod_coordinator.coo_position
          else
            Cells[4,RowCount-1] := LVessel.ves_position;
          Cells[5,RowCount-1] := GetModStatus(LModule.mod_status);
        end
        else begin
          Cells[0,RowCount-1] := 'false';
          if (LVessel.ves_at_hzb_since = '') then
            Cells[4,RowCount-1] := '- FU -'
          else
            Cells[4,RowCount-1] := '- HZB -';
        end;
      end;
    end;
  finally
    UnlockAfterWorkingWithList;
    Selection := TGridRect(Rect(-1,-1,-1,-1));
  end;
end;

procedure TMainForm.RefreshModTab;
var i, LCount: integer;
    LModule:   TModule;
begin
  LCount := 0;

  with FDataMgmt, modStrGr do
  try
    for i := 1 to RowCount do Rows[i].Clear;
    RowCount := 2;

    LockWhileWorkingWithList;
    for i := 0 to ModuleList.Count - 1 do
    begin
      LModule := TModule(ModuleList[i]);
      
      if (modActiveRBtn.Checked and (LModule.mod_status > 0))
         or (modAllRBtn.Checked) then
      begin
        if not (LCount = 0) then RowCount := RowCount + 1;
        inc(LCount);

        Cells[1,RowCount-1] := LModule.mod_name;
        if (LModule.mod_accumulator < 200) then
          Cells[4,RowCount-1] := intToStr(LModule.mod_accumulator) + ' %';
        Cells[5,RowCount-1] := LModule.mod_comment;
        Cells[6,RowCount-1] := intToStr(LModule.mod_id);

        if not (LModule.mod_status = 0) then
        begin
          Cells[0,RowCount-1] := 'true';
          if Assigned(LModule.mod_vessel) then
            Cells[2,RowCount-1] := intToStr(LModule.mod_vessel.ves_name);

          if (LModule.mod_position = '') then
            Cells[3,RowCount-1] := LModule.mod_coordinator.coo_position
          else
            Cells[3,RowCount-1] := LModule.mod_position;

          Cells[7,RowCount-1] := GetModStatus(LModule.mod_status);
        end
        else
          Cells[0,RowCount-1] := 'false';
      end;
    end;
  finally
    UnlockAfterWorkingWithList;
    Selection := TGridRect(Rect(-1,-1,-1,-1));
  end;
end;

procedure TMainForm.RefreshCooTab;
var i, LCount: integer;
begin
  LCount := 0;

  with FDataMgmt, cooStrGr do
  try
    for i := 1 to RowCount do Rows[i].Clear;
    RowCount := 2;

    LockWhileWorkingWithList;
    for i := 0 to CoordinatorList.Count - 1 do
      if (cooActiveRBtn.Checked and not (TCoordinator(CoordinatorList[i]).coo_status = 0))
         or (cooAllRBtn.Checked) then
      begin
        if not (LCount = 0) then RowCount := RowCount +1;
        inc(LCount);

        Cells[1,RowCount-1] := TCoordinator(CoordinatorList[i]).coo_name;
        Cells[2,RowCount-1] := TCoordinator(CoordinatorList[i]).coo_position;
        Cells[3,RowCount-1] := TCoordinator(CoordinatorList[i]).coo_comport;
        Cells[4,RowCount-1] := TCoordinator(CoordinatorList[i]).coo_comment;
        Cells[5,RowCount-1] := intToStr(TCoordinator(CoordinatorList[i]).coo_id);

        if (TCoordinator(CoordinatorList[i]).coo_status = 0) then
          Cells[0,RowCount-1] := 'false'
        else
          Cells[0,RowCount-1] := 'true';

        if (TCoordinator(CoordinatorList[i]).coo_status = 2) then
          Cells[6,RowCount-1] := 'failure'
        else
          Cells[6,RowCount-1] := ''; 
      end;
  finally
    UnlockAfterWorkingWithList;
    Selection := TGridRect(Rect(-1,-1,-1,-1));
  end;
end;

procedure TMainForm.RefreshDevTab;
var i: integer;
begin
  with FDataMgmt, devStrGr do
  try
    for i := 1 to RowCount do Rows[i].Clear;
    if (DeviceList.Count = 0) then
      RowCount := 2
    else
      RowCount := DeviceList.Count + 1;

    LockWhileWorkingWithList;
    for i := 0 to DeviceList.Count - 1 do
    begin
      if not (TCryostat(DeviceList[i]).dev_lastactive = '') and
         ((now - StrToDateTime(TCryostat(DeviceList[i]).dev_lastactive)) < 1) then
        Cells[0,i+1] := 'true'
      else
        Cells[0,i+1] := 'false';
      Cells[1,i+1] := TCryostat(DeviceList[i]).dev_name;
      if (TCryostat(DeviceList[i]).dev_ch1_level < 200) then
        Cells[2,i+1] := TCryostat(DeviceList[i]).dev_ch1_gas + ':  '
                        + floatToStr(sciDecimals(TCryostat(DeviceList[i]).dev_ch1_level, 1)) + ' %'
      else
        Cells[2,i+1] := '';

      if (TCryostat(DeviceList[i]).dev_ch2_level < 200) then
        Cells[3,i+1] := TCryostat(DeviceList[i]).dev_ch2_gas + ':  '
                        + floatToStr(sciDecimals(TCryostat(DeviceList[i]).dev_ch2_level, 1)) + ' %'
      else
        Cells[3,i+1] := '';

      if (TCryostat(DeviceList[i]).dev_ch3_level < 200) then
        Cells[4,i+1] := TCryostat(DeviceList[i]).dev_ch3_gas + ':  '
                        + floatToStr(sciDecimals(TCryostat(DeviceList[i]).dev_ch3_level, 1)) + ' %'
      else
        Cells[4,i+1] := '';
                                  
      Cells[5,i+1] := TCryostat(DeviceList[i]).dev_lastactive;
      if Assigned(TCryostat(DeviceList[i]).dev_coordinator) then
        Cells[6,i+1] := TCryostat(DeviceList[i]).dev_coordinator.coo_position;
      Cells[7,i+1] := TCryostat(DeviceList[i]).dev_comment;
      Cells[8,i+1] := intToStr(TCryostat(DeviceList[i]).dev_id);
    end;
  finally
    UnlockAfterWorkingWithList;
    Selection := TGridRect(Rect(-1,-1,-1,-1));
  end;
end;


procedure TMainForm.VesOptions(const AMsg: string);
begin
  ChangedVesOptionsMeIt.Enabled := true;
  LogListView(AMsg);
end;

procedure TMainForm.ModOptions(const AMsg: string);
begin
  ChangedModOptionsMeIt.Enabled := true;
  LogListView(AMsg);
end;


procedure TMainForm.LogoutVessel(const AVesId: Integer);
var i:       integer;
    LVessel: TVessel;
begin
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVessel := GetVesItem(AVesId);

    if Assigned(LVessel) then
      for i := 0 to ModuleList.Count - 1 do
       if (TModule(ModuleList[i]).mod_vessel = LVessel) then
       begin
         TModule(ModuleList[i]).mod_status      := 0;
         TModule(ModuleList[i]).mod_coordinator := nil;
         TModule(ModuleList[i]).mod_vessel      := nil;
         break;
       end;
  finally
    UnlockAfterWorkingWithList;
  end;
  CreateMainFormJob(7,0,'');
  CreateMainFormJob(8,0,'');
end;

procedure TMainForm.LogoutModule(const AModId: Integer);
var LModule: TModule;
begin
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LModule := GetModItem(AModId);

    if Assigned(LModule) then
    begin
      LModule.mod_status      := 0;
      LModule.mod_coordinator := nil;
      LModule.mod_vessel      := nil;
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
  CreateMainFormJob(7,0,'');
  CreateMainFormJob(8,0,'');
end;


procedure TMainForm.CheckCommunicationTimeout;
var i:     integer;
    LFlag: boolean;
begin
  LFlag := false;

  with FDataMgmt do
  try
    LockWhileWorkingWithList;

    for i := 0 to ModuleList.Count - 1 do
      if (TModule(ModuleList[i]).mod_status > 0) then

       if (1440 * (now - StrToDateTime(TModule(ModuleList[i]).mod_lastactive))
            > (0.5 + getRegInteger('','SleepTime') + ((getRegInteger('','AwakeTime')) / 60))) then
        if not (TModule(ModuleList[i]).mod_status = 5) then
        begin
          TModule(ModuleList[i]).mod_status := 5;
          LFlag := true;
        end;

    if LFlag then
    begin
      CreateMainFormJob(7,0,'');
      CreateMainFormJob(8,0,'');
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.CheckDatabaseFunction;
begin
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    ConfigureDbConnection;
  finally
    UnlockAfterWorkingWithList;
  end;
end;


procedure TMainForm.RetryMeasurementUpload;
var LReg:    TRegistry;
    LNames:  TStringList;
    LVessel: TVessel;
    LId, i:  integer;
const
  REG_VESSEL:   string = 'Software\HZB_Helium\Measurements\';
  REG_CRYOSTAT: string = 'Software\HZB_Helium\CryoMeasurements\';

  function AsInt(const AName: string): integer;
  begin
    Result := 0;
    if LReg.ValueExists(AName) then
      Result := LReg.ReadInteger(AName);
  end;

  function AsFloat(const AName: string): double;
  begin
    Result := 0.0;
    if LReg.ValueExists(AName) then
      Result := LReg.ReadFloat(AName); 
  end;

  function AsStr(const AName: string): string;
  begin
    Result := '';
    if LReg.ValueExists(AName) then
      Result := LReg.ReadString(AName);
  end;

begin
  LReg         := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  LNames       := nil;

  if LReg.KeyExists(REG_VESSEL) then
  try
    LNames := TStringList.Create;
    LReg.OpenKey(REG_VESSEL, false);
    LReg.GetKeyNames(LNames);
    LReg.CloseKey;

    if not (LNames.Count = 0) then
      with FDataMgmt do
      try
        LockWhileWorkingWithList;

        for i := 0 to LNames.Count - 1 do
        try
          LReg.OpenKey(REG_VESSEL + LNames[0], false);

          LId := AsInt('ves_id');

          if not (LId > 0) then
          begin
            LVessel := GetVesItemByName(AsInt('ves_name'));
            if Assigned(LVessel) then LId := LVessel.ves_id;
          end;

          if (LId > 0) then
          begin
            if MeasurementToDb(LId,
                               AsInt('mod_id'),
                               AsInt('coo_id'),
                               AsStr('position'),
                               AsFloat('he_level'),
                               AsInt('pressure'),
                               AsInt('battery'),
                               AsStr('date'),
                               AsInt('status')) then
            begin
              LReg.CloseKey;
              LReg.DeleteKey(REG_VESSEL + LNames[0]);
              LNames.Delete(0);
            end
            else begin
              ErrorMsg('Upload measurements from registry to database failed!');
              break;
            end;
          end;
        finally
          LReg.CloseKey;
        end;
      finally
        UnlockAfterWorkingWithList;
      end;

    if (LNames.Count = 0) then
      LReg.DeleteKey(REG_VESSEL);
  finally
    LNames.Clear;
  end;

  if LReg.KeyExists(REG_CRYOSTAT) then
  try
    if not Assigned(LNames) then LNames := TStringList.Create;
    LReg.OpenKey(REG_CRYOSTAT, false);
    LReg.GetKeyNames(LNames);
    LReg.CloseKey;

    if not (LNames.Count = 0) then
      with FDataMgmt do
      try
        LockWhileWorkingWithList;

        for i := 0 to LNames.Count - 1 do
        try
          LReg.OpenKey(REG_CRYOSTAT + LNames[0], false);

          LId := AsInt('dev_id');

          if (LId > 0) then
          begin
            if CryoMeasurementToDb(LId,
                                   AsInt('coo_id'),
                                   AsFloat('he_level'),
                                   AsFloat('n2_level1'),
                                   AsFloat('n2_level2'),
                                   AsStr('date')) then
            begin
              LReg.CloseKey;
              LReg.DeleteKey(REG_CRYOSTAT + LNames[0]);
              LNames.Delete(0);
            end
            else begin
              ErrorMsg('Upload cryo measurements from registry to database failed!');
              break;
            end;
          end;
        finally
          LReg.CloseKey;
        end;
      finally
        UnlockAfterWorkingWithList;
      end;

    if (LNames.Count = 0) then
      LReg.DeleteKey(REG_CRYOSTAT);
  finally
  end;

  if Assigned(LNames) then LNames.Free;
  LReg.Free;
end;


procedure TMainForm.CheckStatus(const ACooId: integer);
var i: integer;
begin
  i := GetThreadListIndex(ACooId);
  if not (i = -1) then
    TCoordinatorThread(FCooThreadList[i]).CreateCoordinatorJob(3,'','');
end;

procedure TMainForm.CheckStatusForAll;
var LId, i, LRow: integer;
    LCoo:         TCoordinator;
    LStatus:      byte;
    LFlag:        boolean;
    LName, LPort: string;
begin
  i       := 0;
  LId     := 0;
  LStatus := 0;
  LPort   := '';
  LName   := '';

  while (i < FCooThreadList.Count) do
  begin
    with FDataMgmt do
    try
      LockWhileWorkingWithList;
      LCoo := GetCooItem(TCoordinatorThread(FCooThreadList[i]).Id);
      LFlag := Assigned(LCoo);
      if LFlag then
      begin
        LId     := LCoo.coo_id;
        LName   := LCoo.coo_name;
        LPort   := LCoo.coo_comport;
        LStatus := LCoo.coo_xbeestatus;
        inc(LCoo.coo_xbeestatus);
       end;
    finally
      UnlockAfterWorkingWithList;
    end;

    if LFlag then
      if (LStatus in [0, 1]) then
        TCoordinatorThread(FCooThreadList[i]).CreateCoordinatorJob(4,'CE','')
      else begin
        FCooThreadList.Delete(i);
        dec(i);

        if CreateCooThread(LId, LPort) then
          ErrorMsg('Lost XBee connection, restart of ' + LName + ' succeed.')
        else begin
          ErrorMsg('Lost XBee connection, restart of ' + LName + ' failed.');
          LRow := GetRowOfCooStrGr(intToStr(LId));

          if not (LRow = -1) then
            if cooActiveRBtn.Checked then
              deleteRow(cooStrGr, LRow)
            else
              cooStrGr.Cells[0,LRow] := 'false';
        end;
      end;

    inc(i);
  end;
end;

procedure TMainForm.CheckXBee(const ACooId: Integer);
var i:    integer;
begin
  i := GetThreadListIndex(ACooId);
  if not (i = -1) then
    TCoordinatorThread(FCooThreadList[i]).CreateCoordinatorJob(4,'CE','');
end;


procedure TMainForm.DbDisconnect;
begin
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    DisconnectDbConnection;
  finally
    UnlockAfterWorkingWithList;
  end;
end;


function TMainForm.GetRowOfCooStrGr(const AId: string): integer;
var i:   integer;
begin
  Result := -1;

  for i := 1 to cooStrGr.RowCount - 1 do
    if (cooStrGr.Cells[5,i] = AId) then
    begin
      Result := i;
      break;
    end;
end;

function TMainForm.GetThreadListIndex(const AId: integer): integer;
var i: integer;
begin
  Result := -1;

  for i := 0 to FCooThreadList.Count - 1 do
    if (TCoordinatorThread(FCooThreadList[i]).Id = AId) then
    begin
      Result := i;
      break;
    end;
end;


function TMainForm.GetIdInActiveRow(AStringGrid: TStringGrid; const ACol: Integer): integer;
begin
  Result := -1;

  if (AStringGrid.Row > 0) then
    TryStrToInt(AStringGrid.Cells[ACol, AStringGrid.Row], Result);
end;


procedure TMainForm.ShowVesselForm;
begin
  with TVesselForm.CreateWithProperties(FDataMgmt, nil) do
  try
    ShowModal;
    CreateMainFormJob(7,0,'');
  finally
    Free;
  end;
end;

procedure TMainForm.ShowModuleForm;
begin
  with TModuleForm.CreateWithProperties(FDataMgmt, nil) do
  try
    ShowModal;
    CreateMainFormJob(8,0,'');
  finally
    Free;
  end;
end;

procedure TMainForm.ShowCoordinatorForm;
begin
  with TCoordinatorForm.CreateWithProperties(FDataMgmt, nil) do
  try
    ShowModal;
    CreateMainFormJob(9,0,'');
  finally
    Free;
  end;
end;

procedure TMainForm.ShowIlmDeviceForm;
begin
  with TIlmDeviceForm.CreateWithProperties(FDataMgmt, nil) do
  try
    ShowModal;
    CreateMainFormJob(20,0,'');
  finally
    Free;
  end;
end;


procedure TMainForm.LogListView(const S : string);
var LItem : TListItem;
begin
  if (S <> '') then
  begin
    while (lvLog.Items.Count >= 100) do lvLog.Items.Delete(0);

    LItem := lvLog.Items.Add;
    LItem.Caption := dateToStr(now) + '   ' + timeToStr(now);
    LItem.SubItems.Add(S);
    LItem.MakeVisible(false);
  end;
end;

procedure TMainForm.LogErrFile(const S : string);
var f: TextFile;
begin
  if (S <> '') then
  try
    AssignFile(f, ERROR_LOGFILE);
    if FileExists(ERROR_LOGFILE) then
      Append(f)
    else
      Rewrite(f);
    WriteLn(f, DateTimeToStr(Now) + #9 + S);
  finally
    closeFile(f);
  end;
end;


procedure TMainForm.GetMeasurements(const AQuery: string; var AFillList, AStayList: TObjectList);
var
  lDbClient: ISQLClient; 
  LMea:     TMeasurement;
begin
  if not Assigned(AFillList) then
    AFillList := TObjectList.Create(true);
  if not Assigned(AStayList) then
    AStayList := TObjectList.Create(true);
  lDbClient := nil;

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    if GetDbConnection then lDbClient := GetDbClient;
    if Assigned(lDbClient) then
    begin
      setDbQuery(lDbClient, AQuery);
      if lDbClient.Open then
      begin
        if lDbClient.Active then with lDbClient do
          while not Eof do
          begin
            LMea := TMeasurement.Create;
            LMea.mea_he_level := FieldByName('MEA_HE_LEVEL').AsFloat;
            LMea.mea_date     := FieldByName('MEA_DATE').AsDateTime;
            LMea.mea_status   := FieldByName('MEA_STATUS').AsInteger;

            if LMea.mea_status in [3,4,5] then
              AFillList.Add(LMea)
            else
              AStayList.Add(LMea);
            Next;
          end;
      end
      else
        ErrorMsg(Format(EOpenClient, [lDbClient.GetErrorBuffer.Message]));
    end;
  finally
    if Assigned(lDbClient) then lDbClient.Close;
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.GetMeasurements(const AQuery: string; var AFillList: TObjectList);
var
  lDbClient: ISQLClient;
  LMea:     TMeasurement;
begin
  if not Assigned(AFillList) then
    AFillList := TObjectList.Create(true);
  lDbClient := nil;

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    if GetDbConnection then lDbClient := GetDbClient;
    if Assigned(lDbClient) then
    begin
      setDbQuery(lDbClient, AQuery);
      if lDbClient.Open then
      begin
        if lDbClient.Active then with lDbClient do
          while not Eof do
          begin
            LMea := TMeasurement.Create;
            LMea.mea_he_level := FieldByName('MEA_HE_LEVEL').AsFloat;
            LMea.mea_date     := FieldByName('MEA_DATE').AsDateTime;
            LMea.mea_status   := FieldByName('MEA_STATUS').AsInteger;
            AFillList.Add(LMea);
            Next;
          end;
      end
      else
        ErrorMsg(Format(EOpenClient, [lDbClient.GetErrorBuffer.Message]));
    end;
  finally
    if Assigned(lDbClient) then lDbClient.Close;
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.GetIlmMeasurements(const AQuery: string; var AList: TObjectList);
var lDbClient: ISQLClient;
    LMea:     TIlmMeasurement;
begin
  if not Assigned(AList) then
    AList := TObjectList.Create(true);
  lDbClient := nil;

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    if GetDbConnection then lDbClient := GetDbClient;
    if Assigned(lDbClient) then
    begin
      setDbQuery(lDbClient, AQuery);
      if lDbClient.Open then
      begin
        if lDbClient.Active then with lDbClient do
          while not Eof do
          begin
            LMea := TIlmMeasurement.Create;
            LMea.mea_ch1_level := FieldByName('ILM_CH1_LEVEL').AsFloat;
            LMea.mea_ch2_level := FieldByName('ILM_CH2_LEVEL').AsFloat;
            LMea.mea_ch3_level := FieldByName('ILM_CH3_LEVEL').AsFloat;
            LMea.mea_date      := FieldByName('ILM_DATE').AsDateTime;

            AList.Add(LMea);
            Next;
          end;
      end
      else
        ErrorMsg(Format(EOpenClient, [lDbClient.GetErrorBuffer.Message]));
    end;
  finally
    if Assigned(lDbClient) then lDbClient.Close;
    UnlockAfterWorkingWithList;
  end;
end;


procedure TMainForm.GetPlotData(const AList: TObjectList; var AxVal: array of Single; var AyVal: array of Single);
var i: integer;
begin
  for i := 0 to AList.Count - 1 do
  begin
    AxVal[i] := (TMeasurement(AList[i]).mea_date - now); // X: date [days]
    AyVal[i] := TMeasurement(AList[i]).mea_he_level;     // Y: he level [%]
  end;
end;

procedure TMainForm.GetPlotData(const AChannel: byte; const AList: TObjectList; var AxVal: array of Single; var AyVal: array of Single);
var i: integer;
begin
  for i := 0 to AList.Count - 1 do
  begin
    AxVal[i] := (TIlmMeasurement(AList[i]).mea_date - now); // X: date [days]

    case AChannel of
    1: AyVal[i] := TIlmMeasurement(AList[i]).mea_ch1_level; // Y: he level [%]
    2: AyVal[i] := TIlmMeasurement(AList[i]).mea_ch2_level; // Y: he level [%]
    3: AyVal[i] := TIlmMeasurement(AList[i]).mea_ch3_level; // Y: he level [%]
    end;
  end;
end;


function TMainForm.GetHeLoss(const AQuery: string): TObjectList;
var LMea1, LMea2, LTemp: TMeasurement;
    LMeaList:            TObjectList;
    i:                   integer;
    LTimeDiff:           real;
    LLevelDiff:          real;
begin
  LMeaList := nil;
  Result   := TObjectList.Create(true);
  GetMeasurements(AQuery, LMeaList);

  if (LMeaList.Count > 1) then
  try
    for i := 1 to LMeaList.Count - 1 do
    begin
      LMea1 := TMeasurement(LMeaList[i - 1]);
      LMea2 := TMeasurement(LMeaList[i]);

      if ((LMea1.mea_status in [2]) and (LMea2.mea_status in [2])) then
      begin
        LTimeDiff  := abs(LMea1.mea_date - LMea2.mea_date);
        LLevelDiff := LMea1.mea_he_level - LMea2.mea_he_level;

        // nicht kleiner als 1/4h  und nicht größer als 1Tag
        if (LTimeDiff > 0.01) and (LTimeDiff < 1) and (LLevelDiff >= 0) then
        begin
          LTemp := TMeasurement.Create;
          LTemp.mea_he_level := LLevelDiff / LTimeDiff;
          LTemp.mea_date     := LMea1.mea_date + (LTimeDiff / 2);
          Result.Add(LTemp);
        end;
      end;
    end;
  finally
  end;

  LMeaList.Free;
end;


procedure TMainForm.timeoutTimerTimer(Sender: TObject);
begin
  inc(FTimerCounter);
                                       //  1 min interval
  CreateMainFormJob(14,0,'');

  case FTimerCounter of
    5: CreateMainFormJob(18,0,'');     //  5 min interval
   10: begin                           // 10 min interval
         CreateMainFormJob(18,0,'');
         CreateMainFormJob(20,0,'');
         CreateMainFormJob(16,0,'');
         FTimerCounter := 0;
       end;
  end;
end;

//   G U I - E v e n t s   - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (MessageDlg('Do you really want to close the HZB - Helium Management?',
               mtConfirmation, [mbYes,mbNo], 0) = mrYes);
end;


procedure TMainForm.RefreshMeItClick(Sender: TObject);
begin
  // Reconfigure DB
  CreateMainFormJob(15,0,'');

  // Refresh DataMgmt
  CreateMainFormJob(4,0,'');
  CreateMainFormJob(5,0,'');
  CreateMainFormJob(6,0,'');
  CreateMainFormJob(21,0,'');

  // Refresh Table View
  CreateMainFormJob(7,0,'');
  CreateMainFormJob(8,0,'');
  CreateMainFormJob(9,0,'');
  CreateMainFormJob(20,0,'');
end;

procedure TMainForm.TimeLabClick(Sender: TObject);
begin
  if TimeLab.Caption = '<<' then
    TimeLab.Caption := '>>     Started: ' + DateTimeToStr(FStartTime)
  else
    TimeLab.Caption := '<<';
end;

procedure TMainForm.CloseMeItClick(Sender: TObject);
begin
  Close;
end;


procedure TMainForm.VesselMeItClick(Sender: TObject);
begin
  ShowVesselForm;
end;

procedure TMainForm.ModuleMeItClick(Sender: TObject);
begin
  ShowModuleForm;
end;

procedure TMainForm.CoordinatorMeItClick(Sender: TObject);
begin
  ShowCoordinatorForm;
end;

procedure TMainForm.IlmDeviceMeItClick(Sender: TObject);
begin
  ShowIlmDeviceForm;
end;

procedure TMainForm.XBeeMeItClick(Sender: TObject);
var i: integer;
begin
  with TSettingsForm.Create(nil) do
  try
    if Execute(FDataMgmt.GetDbClient.GetSession) then
    begin
      // init SQLClientFactory
      if FDataMgmt.Init then
      begin
        // Position List
        if ListChanged then
        try
          FDataMgmt.LockWhileWorkingWithList;
          for i := 0 to FDataMgmt.ModuleList.Count - 1 do
            if (TModule(FDataMgmt.ModuleList[i]).mod_status > 0) then
              TModule(FDataMgmt.ModuleList[i]).mod_setPositions := true;
        finally
          FDataMgmt.UnlockAfterWorkingWithList;
        end;

        // Password
        if not (PasswordOld = PasswordNew) then
        try
          FDataMgmt.LockWhileWorkingWithList;
          for i := 0 to FDataMgmt.ModuleList.Count - 1 do
            if (TModule(FDataMgmt.ModuleList[i]).mod_status > 0) then
              TModule(FDataMgmt.ModuleList[i]).mod_setPassword := true;
        finally
          FDataMgmt.UnlockAfterWorkingWithList;
        end;

        // Awake Time
        if not (AwakeTimeOld = AwakeTimeNew) then
        try
          FDataMgmt.LockWhileWorkingWithList;
          for i := 0 to FDataMgmt.ModuleList.Count - 1 do
            if (TModule(FDataMgmt.ModuleList[i]).mod_status > 0) then
              TModule(FDataMgmt.ModuleList[i]).mod_setAwakeTime := true;
        finally
          FDataMgmt.UnlockAfterWorkingWithList;
        end;

        // Sleep Time
        if not (SleepTimeOld = SleepTimeNew) then
        try
          FDataMgmt.LockWhileWorkingWithList;
          for i := 0 to FDataMgmt.ModuleList.Count - 1 do
            if (TModule(FDataMgmt.ModuleList[i]).mod_status > 0) then
              TModule(FDataMgmt.ModuleList[i]).mod_setSleepTime := true;
        finally
          FDataMgmt.UnlockAfterWorkingWithList;
        end;

        ApplyCoordRetryAttempts;
        SetFrontPanelLocked(False);
      end
      else
        SetFrontPanelLocked(True);
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.PageControlDrawTab(Control: TCustomTabControl;
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

procedure TMainForm.DefaultsMeItClick(Sender: TObject);
begin
  with TDefaultsForm.CreateWithProperties(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.ChangedVesOptionsMeItClick(Sender: TObject);
begin
  with TOptionsForm.CreateWithProperties(FDataMgmt, nil) do
  try
    ShowModal;
    CreateMainFormJob(7,0,'');
    ChangedVesOptionsMeIt.Enabled := vesOptionChangesExists;
  finally
    Free;
  end;
end;

procedure TMainForm.ChangedModOptionsMeItClick(Sender: TObject);
begin
  with TModuleOptionForm.CreateWithProperties(FDataMgmt, nil) do
  try
    ShowModal;
    CreateMainFormJob(8,0,'');
    ChangedModOptionsMeIt.Enabled := modOptionChangesExists;
  finally
    Free;
  end;
end;
           

procedure TMainForm.ErrorLogMeItClick(Sender: TObject);
begin
  if not FileExists(ERROR_LOGFILE) then
    MessageDlg('There have been no errors so far.', mtInformation, [mbOk], 0)
  else
    ShellExecute(Application.Handle, 'open', ERROR_LOGFILE, nil, nil, SW_ShowNormal);
end;

procedure TMainForm.AboutMeItClick(Sender: TObject);
begin
  with TAboutForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


procedure TMainForm.vesTabShResize(Sender: TObject);
begin
  // set width of the last column
  setLastColSize(vesStrGr);
end;

procedure TMainForm.modTabShResize(Sender: TObject);
begin
  // set width of the last column
  setLastColSize(modStrGr);
end;

procedure TMainForm.cooTabShResize(Sender: TObject);
begin
  // set width of the last column
  setLastColSize(cooStrGr);
end;

procedure TMainForm.devTabShResize(Sender: TObject);
begin
  // set width of the last column
  setLastColSize(devStrGr);
end;


procedure TMainForm.vesRBtnClick(Sender: TObject);
begin
  CreateMainFormJob(7,0,'');
end;

procedure TMainForm.modRBtnClick(Sender: TObject);
begin
  CreateMainFormJob(8,0,'');
end;

procedure TMainForm.cooRBtnClick(Sender: TObject);
begin
 CreateMainFormJob(9,0,'');
end;


procedure TMainForm.vesStrGrExit(Sender: TObject);
begin
  vesStrGr.Selection := TGridRect(Rect(-1,-1,-1,-1));
end;

procedure TMainForm.modStrGrExit(Sender: TObject);
begin
  modStrGr.Selection := TGridRect(Rect(-1,-1,-1,-1));
end;

procedure TMainForm.cooStrGrExit(Sender: TObject);
begin
  cooStrGr.Selection := TGridRect(Rect(-1,-1,-1,-1));
end;

procedure TMainForm.devStrGrExit(Sender: TObject);
begin
  devStrGr.Selection := TGridRect(Rect(-1,-1,-1,-1));
end;


procedure TMainForm.vesStrGrDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);

  function CheckBox(Value: String): Cardinal;
  begin
    if Value = 'true' then
      Result:= DFCS_BUTTONCHECK or DFCS_CHECKED
    else
      Result:= DFCS_BUTTONCHECK;
  end;

begin
  with TStringGrid(Sender) do
  begin
    if (ARow = 0) then
    begin
      Canvas.Font.Style := [fsBold];
      Canvas.FillRect(Rect);
      DrawText(Canvas.Handle, PChar(Cells[ACol,ARow]), StrLen(PChar(Cells[ACol,ARow])),Rect,DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    end
    else begin
      if (Cells[5,ARow] = 'timeout') then Canvas.Brush.Color := clRed;

      if ACol in [0] then
        if ((Cells[ACol, ARow] = 'true') or (Cells[ACol, ARow] = 'false')) then
        begin
          Canvas.FillRect(Rect);
          InflateRect(Rect, -4, -4);
          DrawFrameControl(Canvas.Handle, Rect,DFC_Button,CheckBox(Cells[ACol, ARow]));
        end;

      if ACol in [1..6] then
      begin
        Canvas.FillRect(Rect);
        DrawText(Canvas.Handle, PChar(' ' + Cells[ACol,ARow]),1 + StrLen(PChar(Cells[ACol,ARow])),Rect,DT_VCENTER or DT_SINGLELINE);
      end;
    end;
  end;
end;

procedure TMainForm.modStrGrDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);

  function CheckBox(Value: String): Cardinal;
  begin
    if Value = 'true' then
      Result:= DFCS_BUTTONCHECK or DFCS_CHECKED
    else
      Result:= DFCS_BUTTONCHECK;
  end;

begin
  with TStringGrid(Sender) do
  begin
    if (ARow = 0) then
    begin
      Canvas.Font.Style := [fsBold];
      Canvas.FillRect(Rect);
      DrawText(Canvas.Handle, PChar(Cells[ACol,ARow]), StrLen(PChar(Cells[ACol,ARow])),Rect,DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    end
    else begin
      if (Cells[7,ARow] = 'timeout') then Canvas.Brush.Color := clRed;

      if ACol in [0] then
        if ((Cells[ACol, ARow] = 'true') or (Cells[ACol, ARow] = 'false')) then
        begin
          Canvas.FillRect(Rect);
          InflateRect(Rect, -4, -4);
          DrawFrameControl(Canvas.Handle, Rect,DFC_Button,CheckBox(Cells[ACol, ARow]));
        end;

      if ACol in [1..5] then
      begin
        Canvas.FillRect(Rect);
        DrawText(Canvas.Handle, PChar(' ' + Cells[ACol,ARow]),1 + StrLen(PChar(Cells[ACol,ARow])),Rect,DT_VCENTER or DT_SINGLELINE);
      end;
    end;
  end; 
end;

procedure TMainForm.cooStrGrDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);

  function CheckBox(Value: String): Cardinal;
  begin
    if Value = 'true' then
      Result:= DFCS_BUTTONCHECK or DFCS_CHECKED
    else
      Result:= DFCS_BUTTONCHECK;
  end;

begin
  with TStringGrid(Sender) do
  begin
    if (ARow = 0) then
    begin
      Canvas.Font.Style := [fsBold];
      Canvas.FillRect(Rect);
      DrawText(Canvas.Handle, PChar(Cells[ACol,ARow]), StrLen(PChar(Cells[ACol,ARow])),Rect,DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    end
    else begin
      if (Cells[6,ARow] = 'failure') then Canvas.Brush.Color := clRed;
                                                 
      if ACol in [0] then
        if ((Cells[ACol, ARow] = 'true') or (Cells[ACol, ARow] = 'false')) then
        begin
          Canvas.FillRect(Rect);
          InflateRect(Rect, -4, -4);
          DrawFrameControl(Canvas.Handle, Rect,DFC_Button,CheckBox(Cells[ACol, ARow]));
        end;

      if ACol in [1..4] then
      begin
        Canvas.FillRect(Rect);
        DrawText(Canvas.Handle, PChar(' ' + Cells[ACol,ARow]),1 + StrLen(PChar(Cells[ACol,ARow])),Rect,DT_VCENTER or DT_SINGLELINE);
      end;
    end;
  end;
end;

procedure TMainForm.devStrGrDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);

  function CheckBox(Value: String): Cardinal;
  begin
    if Value = 'true' then
      Result:= DFCS_BUTTONCHECK or DFCS_CHECKED
    else
      Result:= DFCS_BUTTONCHECK;
  end;

begin
  with TStringGrid(Sender) do
  begin
    if (ARow = 0) then
    begin
      Canvas.Font.Style := [fsBold];
      Canvas.FillRect(Rect);
      DrawText(Canvas.Handle, PChar(Cells[ACol,ARow]), StrLen(PChar(Cells[ACol,ARow])),Rect,DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    end
    else begin
     if ACol in [0] then
        if ((Cells[ACol, ARow] = 'true') or (Cells[ACol, ARow] = 'false')) then
        begin
          Canvas.FillRect(Rect);
          InflateRect(Rect, -4, -4);
          DrawFrameControl(Canvas.Handle, Rect,DFC_Button,CheckBox(Cells[ACol, ARow]));
        end;

      if ACol in [1..6] then
      begin
        Canvas.FillRect(Rect);
        DrawText(Canvas.Handle, PChar(' ' + Cells[ACol,ARow]),1 + StrLen(PChar(Cells[ACol,ARow])),Rect,DT_VCENTER or DT_SINGLELINE);
      end;
    end;
  end;
end;


procedure TMainForm.StrGrMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  Handled := true;
end;


procedure TMainForm.vesStrGrMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var LCol, LRow: integer;
begin
  with TStringGrid(Sender) do
  begin
    if (Button = mbLeft) then
    begin
      MouseToCell(x, y, LCol, LRow);
      if (LCol = 0) and not (LRow = 0) and (Cells[5, LRow] = 'timeout') then
        if (MessageDlg('Do you really want to logout this vessel?',
                       mtCustom, [mbYes, mbNo], 0) = mrYes) then
          CreateMainFormJob(12, strToInt(Cells[7, LRow]), '');
    end;

    if (Button = mbRight) then
      if (Cells[0,1] = 'true') or (Cells[0,1] = 'false') then
      begin
        MouseToCell(X,Y,LCol,LRow);
        if (LRow >= FixedRows) then
        begin
          Selection := TGridRect(Rect(LCol, LRow, LCol, LRow));
          X := MainForm.Left + PageControl.Left + vesTabSh.Left + Left + X + 4;
          Y := MainForm.Top  + PageControl.Top  + vesTabSh.Top  + Top  + Y + 42;
          vesPopUpMenu.Popup(X,Y);
        end;
      end;
  end;
end;

procedure TMainForm.modStrGrMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var LCol, LRow: integer;
begin
  with TStringGrid(Sender) do
  begin
    if (Button = mbLeft) then
    begin
      MouseToCell(x, y, LCol, LRow);
      if (LCol = 0) and not (LRow = 0) and (Cells[7, LRow] = 'timeout') then
        if (MessageDlg('Do you really want to logout this module?',
                       mtCustom, [mbYes, mbNo], 0) = mrYes) then
          CreateMainFormJob(13, strToInt(Cells[6, LRow]), '');
    end;

    if (Button = mbRight) then
      if (Cells[0,1] = 'true') or (Cells[0,1] = 'false') then
      begin
        MouseToCell(X,Y,LCol,LRow);
        if (LRow >= FixedRows) then
        begin
          Selection := TGridRect(Rect(LCol,LRow,LCol,LRow));
          X := MainForm.Left + PageControl.Left + modTabSh.Left + Left + X + 4;
          Y := MainForm.Top  + PageControl.Top  + modTabSh.Top  + Top  + Y + 42;
          GetStatusMeIt.Enabled    := (Cells[0,LRow] = 'true');
          GetOptionsMeIt.Enabled   := (Cells[0,LRow] = 'true');
          SetOptionsMeIt.Enabled   := (Cells[0,LRow] = 'true');
          GetPosMeIt.Enabled       := (Cells[0,LRow] = 'true');
          SetPosMeIt.Enabled       := (Cells[0,LRow] = 'true');
          GetPasswordMeIt.Enabled  := (Cells[0,LRow] = 'true');
          SetPasswordMeIt.Enabled  := (Cells[0,LRow] = 'true');
          GetAwakeTimeMeIt.Enabled := (Cells[0,LRow] = 'true');
          SetAwakeTimeMeIt.Enabled := (Cells[0,LRow] = 'true');
          GetSleepTimeMeIt.Enabled := (Cells[0,LRow] = 'true');
          SetSleepTimeMeIt.Enabled := (Cells[0,LRow] = 'true');
          SendTextMsgMeIt.Enabled  := (Cells[0,LRow] = 'true');
          modPopUpMenu.Popup(X,Y);
        end;
      end;
  end;
end;

procedure TMainForm.cooStrGrMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var LCol, LRow: Integer;
begin
  with TStringGrid(Sender) do
  begin
    if (Button = mbLeft) then
    begin
      MouseToCell(x, y, LCol, LRow);
      if (LCol = 0) and (LRow > 0) then
      try
        if (Cells[LCol, LRow] = 'true') then
          if (MessageDlg('Do you really want to stop ' + Cells[1, LRow] + '?',
                         mtCustom, [mbYes, mbNo], 0) = mrYes) then
          begin
            CreateMainFormJob(3,strToInt(cooStrGr.Cells[5,LRow]),'');
            CreateMainFormJob(0, 0, cooStrGr.Cells[1,LRow] + ' disabled.');
          end;

        if (Cells[0, LRow] = 'false') then
          if CreateCooThread(strToInt(Cells[5,LRow]), Cells[3,LRow]) then
            Cells[0, LRow] := 'true';
      finally
      end;
    end;

    if (Button = mbRight) then
    begin
      MouseToCell(x, y, LCol, LRow);
      if (LRow >= FixedRows) and
         ((Cells[0,LRow] = 'true') or (Cells[0,LRow] = 'false')) then
      begin
        Selection := TGridRect(Rect(LCol,LRow,LCol,LRow));
        X := MainForm.Left + PageControl.Left + modTabSh.Left + Left + X + 4;
        Y := MainForm.Top  + PageControl.Top  + modTabSh.Top  + Top  + Y + 42;
        ActivateMeIt.Enabled    := (Cells[0,LRow] = 'false');
        DeactivateMeIt.Enabled  := (Cells[0,LRow] = 'true');
        CheckStatusMeIt.Enabled := (Cells[0,LRow] = 'true');
        CheckXBeeMeIt.Enabled   := (Cells[0,LRow] = 'true');                  
        cooPopUpMenu.Popup(X,Y);                                              
      end;
    end;
  end;
end;

procedure TMainForm.cooStrGrMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var LCol, LRow: Integer;
begin
  with TStringGrid(Sender) do
  begin
    MouseToCell(x, y, LCol, LRow);
    if (LCol = 0) and (LRow > 0) then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
  end;
end;

procedure TMainForm.devStrGrMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var LCol, LRow: Integer;
begin
  with TStringGrid(Sender) do
  begin
    if (Button = mbRight) then
    begin
      MouseToCell(x, y, LCol, LRow);
      if (LRow >= FixedRows) then
      begin
        Selection := TGridRect(Rect(LCol,LRow,LCol,LRow));
        X := MainForm.Left + PageControl.Left + modTabSh.Left + Left + X + 4;
        Y := MainForm.Top  + PageControl.Top  + modTabSh.Top  + Top  + Y + 42;
        cryPopUpMenu.Popup(X,Y);
      end;
    end;
  end;
end;

       
procedure TMainForm.MeItAdvancedDrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
var lCaption: string;
    lRect   : TRect;
begin
  with Sender as TMenuItem do
    lCaption := Caption;
  lRect := ARect;
  with ACanvas do
  begin
    if (TMenuItem(Sender).Parent.Owner is TMainMenu) then
      if (odSelected in State) then
        Brush.Color := clBtnHighLight
      else
        Brush.Color := clMenuBar
    else
      if (odSelected in State) then
        Brush.Color := clMenuBar
      else
        Brush.Color := clBtnHighLight;


  {  if (odSelected in State) then
      Brush.Color := clMenuBar
    else
      if (TMenuItem(Sender).Parent.Owner is TMainMenu) then
        Brush.Color := clMenuBar
      else
        Brush.Color := clBtnHighLight;
                                             }


    if (odGrayed in State) then
      Font.Color := clGrayText
    else
      Font.Color := clWindowText;


    FillRect(lRect);
    lRect.Left := lRect.Left + 5;

    if (lCaption = cLineCaption) then
    begin
      Inc(lRect.Top, 4);
      dec(lrect.left, 5);
      Font.Color := clBtnFace;
      DrawEdge(Handle, lRect, EDGE_ETCHED, BF_TOP);     
    end
    else
      DrawText(Handle, PChar(lCaption), Length(lCaption),
               lRect, DT_SINGLELINE or DT_VCENTER);
                 

  end;
end;

procedure TMainForm.VesselTypeMeItClick(Sender: TObject);
var LText: string;
    LId:   integer;
    LVes:  TVessel;
begin
  LText := '';
  LId   := GetIdInActiveRow(vesStrGr, 7);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVes := GetVesItem(LId);

    if Assigned(LVes) then
    begin
      LText := 'Vessel ' + intToStr(LVes.ves_name) + #13
               + '========================' + #13 + #13
               + 'Type:'   + #9 + #9 + LVes.ves_type + #13
               + 'Volume:' + #9 + #9 + floatToStr(LVes.ves_volume) + ' l' + #13
               + 'Tare:'   + #9 + #9 + floatToStr(LVes.ves_tare)   + ' kg' + #13
               + '- - - - - - - - - - - - - - - - - - - - - - - - - - -' + #13;
      if (LVes.ves_at_hzb_since = '') then
        LText := LText + 'Delivered:' + #9 + 'Currently not at HZB.'
      else
        LText := LText + 'Delivered:' + #9 + LVes.ves_at_hzb_since;
    end;
  finally
    UnlockAfterWorkingWithList;
  end;

  if not (LText = '') then MessageDlg(LText, mtInformation, [mbOK], 0);
end;

procedure TMainForm.VesselSettingsMeItClick(Sender: TObject);
var LText: string;
    LId:   integer;
    LVes:  TVessel;
begin
  LText := '';
  LId   := GetIdInActiveRow(vesStrGr, 7);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVes := GetVesItem(LId);

    if Assigned(LVes) then
    begin
      LText := 'Vessel ' + intToStr(LVes.ves_name) + #13
               + '==================' + #13 + #13;

      if LVes.ves_options_in_db then
      begin
        LText := LText
                 + 'Short Interval:' + #9 + intToStr(LVes.ves_shortinterval)   + ' sec'       + #13
                 + 'Long Interval:'  + #9 + intToStr(LVes.ves_longinterval)    + ' min'       + #13
                 + 'Min Resistance:' + #9 + floatToStr(LVes.ves_minresistance) + ' ohm'       + #13
                 + 'Max Resistance:' + #9 + floatToStr(LVes.ves_maxresistance) + ' ohm'       + #13
                 + 'Heat Time:'      + #9 + intToStr(LVes.ves_heattime)        + ' msec'      + #13
                 + 'ADC Loop:'       + #9 + intToStr(LVes.ves_adcloops)        + ' cycles'    + #13
                 + 'Fill Timeout:'   + #9 + intToStr(LVes.ves_filltimeout)     + ' min'       + #13
                 + 'Span:'      + #9 + #9 + intToStr(LVes.ves_span)            + ' mbar/volt' + #13
                 + 'Zero:'      + #9 + #9 + intToStr(LVes.ves_zero)            + ' mbar';
      end
      else
        LText := LText + 'There are no options for this' + #13
                       + 'vessel in the database.';
    end;
  finally
    UnlockAfterWorkingWithList;
  end;

  if not (LText = '') then MessageDlg(LText, mtInformation, [mbOK], 0);
end;

procedure TMainForm.VesselHistoryHMeItClick(Sender: TObject);
var LQuery, LName:  string;
    LId, LDays:     integer;
    LVes:           TVessel;
    LxFill, LyFill: Array of single;
    LxStay, LyStay: Array of single;
    LFillList, LStayList: TObjectList;
begin
  LFillList := nil;
  LStayList := nil;
  LName     := '';
  LId       := GetIdInActiveRow(vesStrGr, 7);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVes := GetVesItem(LId);
    if Assigned(LVes) then LName := intToStr(LVes.ves_name);
  finally
    UnlockAfterWorkingWithList;
  end;

  if (LName = '') then
    MessageDlg('Vessel not found!', mtError, [mbOK], 0)
  else begin
    if InputQuery('Helium Management',
                  'Vessel ' + LName + #13 + '========================' + #13 + #13
                  + 'Please enter the duration in days you want to plot (max. 100 days).',
                  LQuery) then
    try
      LDays := strToInt(LQuery);
      if (LDays > 100) then LDays := 100;
      if (LDays > 0) then
      begin
        GetMeasurements('SELECT MEA_HE_LEVEL, MEA_DATE, MEA_STATUS FROM '
                        + db_t_measurement
                        + ' WHERE MEA_VES_ID = ' + intToSTr(LId)
                        + ' AND MEA_DATE >= sysdate - ' + intToStr(LDays)
                        + ' ORDER BY MEA_DATE', LFillList, LStayList);
        setlength(LxFill, LFillList.Count);
        setlength(LyFill, LFillList.Count);
        setlength(LxStay, LStayList.Count);
        setlength(LyStay, LStayList.Count);

        GetPlotData(LFillList, LxFill, LyFill);
        GetPlotData(LStayList, LxStay, LyStay);

        with FPlotForm.Plot do
        begin
          TextTitel     := 'Vessel ' + LName;
          TextLinks     := 'He Level [ % ]';
          TextUnten     := 'Time [ day ]';
          SymbolForm    := plQuadrat;
          SymbolDicke   := 0;
          SymbolFarbe   := clBlack;
          SymbolGroesse := 5;
          PlotSingle(LxStay, LyStay);
          OverlaySingle(LxFill, LyFill, true);
          AutoScale;
          PKurvenEintrag(Kurven.Items[0])^.Symbol.Color := clGreen;
          PKurvenEintrag(Kurven.Items[1])^.Symbol.Color := clRed;
          BereichSetzen(Scale.MinWertX,Scale.MinWertY,Scale.MaxWertX,Scale.MaxWertY);
          ReOverSingle;
          FPlotForm.Show;
        end;
      end
      else
        MessageDlg('Please enter here only positive integers!', mtWarning, [mbOK], 0);
    except
    end;
  end;

  if Assigned(LFillList) then LFillList.Free;
  if Assigned(LStayList) then LStayList.Free;
end;

procedure TMainForm.VesselHistoryDMeItClick(Sender: TObject);
var LQuery, LName: string;
    LId, LDays:    integer;
    LVes:          TVessel;
    LxVal, LyVal:  Array of single;
    LMeaList:      TObjectList;
begin
  LMeaList := nil;
  LName    := '';
  LId      := GetIdInActiveRow(vesStrGr, 7);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVes := GetVesItem(LId);
    if Assigned(LVes) then LName := intToStr(LVes.ves_name);
  finally
    UnlockAfterWorkingWithList;
  end;

  if (LName = '') then
    MessageDlg('Vessel not found!', mtError, [mbOK], 0)
  else begin
    if InputQuery('Helium Management',
                  'Vessel ' + LName + #13 + '========================' + #13 + #13
                  + 'Please enter the duration in days you want to plot (max. 100 days).',
                  LQuery) then
    try
      LDays := strToInt(LQuery);
      if (LDays > 100) then LDays := 100;
      if (LDays > 0) then
      begin
        LMeaList := GetHeLoss('SELECT MEA_HE_LEVEL, MEA_DATE, MEA_STATUS FROM '
                              + db_t_measurement
                              + ' WHERE MEA_VES_ID = ' + intToSTr(LId)
                              + ' AND MEA_DATE >= sysdate - ' + intToStr(LDays)
                              + ' ORDER BY MEA_DATE');
        setlength(LxVal, LMeaList.Count);
        setlength(LyVal, LMeaList.Count);
        GetPlotData(LMeaList, LxVal, LyVal);

        FPlotForm.Plot.TextTitel     := 'Vessel ' + LName;
        FPlotForm.Plot.TextLinks     := 'He Loss [ % / day ]';
        FPlotForm.Plot.TextUnten     := 'Time [ day ]';
        FPlotForm.Plot.SymbolForm    := plQuadrat;
        FPlotForm.Plot.SymbolDicke   := 0;
        FPlotForm.Plot.SymbolFarbe   := clRed;
        FPlotForm.Plot.SymbolGroesse := 5;
        FPlotForm.Plot.PlotSingle(LxVal, LyVal);
        FPlotForm.Show;
      end
      else
        MessageDlg('Please enter here only positive integers!', mtError, [mbOK], 0);
    except
    end;
  end;

  if Assigned(LMeaList) then LMeaList.Free;
end;


procedure TMainForm.ModuleSettingsMeItClick(Sender: TObject);
var LText: string;
    LId:   integer;
    LMod:  TModule;
begin
  LText := '';
  LId   := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LMod := GetModItem(LId);

    if Assigned(LMod) then
    begin
      LText := LMod.mod_name  + #13
               + '============================' + #13 + #13
               + 'Address (XBee):'       + #9 + #9 + LMod.mod_address + #13
               + 'Min Battery voltage:'       + #9 + floatToStr(LMod.mod_minvoltage) + ' volt' + #13
               + 'Max Battery voltage:'       + #9 + floatToStr(LMod.mod_maxvoltage) + ' volt' + #13
               + 'Critical Battery capacity:' + #9 + intToStr(LMod.mod_criticvoltage) + ' %' +#13
               + '- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + #13
               + 'Status:'          + #9 + #9 + #9 + GetModStatus(LMod.mod_status) + #13
               + 'Last Message:'         + #9 + #9 + LMod.mod_lastactive;

      if (LMod.mod_status > 0) and Assigned(LMod.mod_coordinator) then
        LText := LText + #13
                 + 'Communication via:' + #9 + LMod.mod_coordinator.coo_name;
    end;
  finally
    UnlockAfterWorkingWithList;
  end;

  if not (LText = '') then MessageDlg(LText, mtInformation, [mbOK], 0);
end;

procedure TMainForm.GetStatusMeItClick(Sender: TObject);
var LId:  integer;
    LMod: TModule;
begin
  LId := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LMod := GetModItem(LId);        
    if Assigned(LMod) then LMod.mod_getStatus := true;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.GetOptionsMeItClick(Sender: TObject);
var LId:  integer;
    LMod: TModule;
begin
  LId := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LMod := GetModItem(LId);        
    if Assigned(LMod) then LMod.mod_getOptions := true;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.SetOptionsMeItClick(Sender: TObject);
var LId:  integer;
    LMod: TModule;
begin
  LId := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LMod := GetModItem(LId);        
    if Assigned(LMod) then LMod.mod_setOptions := true;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.GetPosMeItClick(Sender: TObject);
var LId:  integer;
    LMod: TModule;
begin
  LId := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LMod := GetModItem(LId);        
    if Assigned(LMod) then LMod.mod_getPositions := true;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.SetPosMeItClick(Sender: TObject);
var LId:  integer;
    LMod: TModule;
begin
  LId := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LMod := GetModItem(LId);        
    if Assigned(LMod) then LMod.mod_setPositions := true;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.GetPasswordMeItClick(Sender: TObject);
var LId:  integer;
    LMod: TModule;
begin
  LId := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LMod := GetModItem(LId);
    if Assigned(LMod) then LMod.mod_getPassword := true;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.SetPasswordMeItClick(Sender: TObject);
var LId:  integer;
    LMod: TModule;
begin
  LId := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LMod := GetModItem(LId);        
    if Assigned(LMod) then LMod.mod_setPassword := true;
  finally
    UnlockAfterWorkingWithList;
  end;
end;


procedure TMainForm.GetAwakeTimeMeItClick(Sender: TObject);
var LId:  integer;
    LMod: TModule;
begin
  LId := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LMod := GetModItem(LId);
    if Assigned(LMod) then LMod.mod_getAwakeTime := true;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.SetAwakeTimeMeItClick(Sender: TObject);
var LId:  integer;
    LMod: TModule;
begin
  LId := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LMod := GetModItem(LId);
    if Assigned(LMod) then LMod.mod_setAwakeTime := true;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.SetFrontPanelLocked(const AValue: Boolean);
begin
    // there is no database, so disable frontpanel to avoid errors
  PageControl.Enabled := not(AValue);
  RefreshMeIt.Enabled := not(AValue);
  VesselmeIt.Enabled  := not(AValue);
  ModuleMeIt.Enabled  := not(AValue);
  CoordinatorMeIt.Enabled := not(AValue);
  IlmDeviceMeIt.Enabled   := not(AValue);
  ChangedVesOptionsMeIt.Enabled := not(AValue);
  ChangedModOptionsMeIt.Enabled := not(AValue);
  LabNoDbConnectionHint.Visible :=
  AValue;
end;

procedure TMainForm.GetSleepTimeMeItClick(Sender: TObject);
var LId:  integer;
    LMod: TModule;
begin
  LId := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LMod := GetModItem(LId);        
    if Assigned(LMod) then LMod.mod_getSleepTime := true;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.SetSleepTimeMeItClick(Sender: TObject);
var LId:  integer;
    LMod: TModule;
begin
  LId := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LMod := GetModItem(LId);        
    if Assigned(LMod) then LMod.mod_setSleepTime := true;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TMainForm.SendTextMsgMeItClick(Sender: TObject);
var LId:  integer;
    LMod: TModule;
begin
  LId := GetIdInActiveRow(modStrGr, 6);

  if not (LId = -1) then
    with TTextMsgForm.CreateWithProperties(nil) do
    try
      ShowModal;

      if (ModalResult = mrOk) then
      try
        FDataMgmt.LockWhileWorkingWithList;
        LMod := FDataMgmt.GetModItem(LId);
        if Assigned(LMod) then
          LMod.mod_displayText := Text;
      finally
        FDataMgmt.UnlockAfterWorkingWithList;
      end;
    finally
      Free;
    end;
end;


procedure TMainForm.CheckStatusMeItClick(Sender: TObject);
begin
  with cooStrGr do CreateMainFormJob(17,strToInt(Cells[5,Row]),'');
end;

procedure TMainForm.CheckXBeeMeItClick(Sender: TObject);
begin
  with cooStrGr do CreateMainFormJob(19,strToInt(Cells[5,Row]),'');
end;


procedure TMainForm.GasHistoryMeItClick(Sender: TObject);
var LQuery, LName:    string;
    LCh1, LCh2, LCh3: string;
    LId, LDays:       integer;
    LCryostat:        TCryostat;
    LxCh1, LyCh1:     Array of single;
    LxCh2, LyCh2:     Array of single;
    LxCh3, LyCh3:     Array of single;
    LMeaList:         TObjectList;
begin
  LMeaList := nil;
  LName    := '';
  LCh1     := '';
  LCh2     := '';
  LCh3     := '';
  LDays    := 10;
  LId      := GetIdInActiveRow(devStrGr, 8);

  if not (LId = -1) then with FDataMgmt do
    try
      LockWhileWorkingWithList;
      LCryostat := GetDevItem(LId);
      if Assigned(LCryostat) then
      begin
        LName := LCryostat.dev_name;
        if LCryostat.dev_ch1_enabled then
          LCh1 := LCryostat.dev_ch1_gas;
        if LCryostat.dev_ch2_enabled then
          LCh2 := LCryostat.dev_ch2_gas;
        if LCryostat.dev_ch3_enabled then
          LCh3 := LCryostat.dev_ch3_gas;
      end;
    finally
      UnlockAfterWorkingWithList;
    end;

  if (LName = '') then
    MessageDlg('Cryostat not found!', mtError, [mbOK], 0)
  else begin
    if InputQuery('Helium Management',
                  LName + #13 + '========================' + #13 + #13
                  + 'Please enter the duration in days you want to plot (max. 100 days).',
                  LQuery) then
    try
      TryStrToInt(LQuery, LDays);
      if (LDays > 100) then LDays := 100;
      if (LDays > 0) then
      begin
        GetIlmMeasurements('SELECT ILM_CH1_LEVEL, ILM_CH2_LEVEL, ILM_CH3_LEVEL, ILM_DATE FROM '
                        + db_t_ilmmeasurement
                        + ' WHERE ILM_DEV_ID = ' + intToSTr(LId)
                        + ' AND ILM_DATE >= sysdate - ' + intToStr(LDays)
                        + ' ORDER BY ILM_DATE', LMeaList);

        if not (LCh1 = '') then
        begin
          setlength(LxCh1, LMeaList.Count);
          setlength(LyCh1, LMeaList.Count);
          GetPlotData(1, LMeaList, LxCh1, LyCh1);
        end;

        if not (LCh2 = '') then
        begin
          setlength(LxCh2, LMeaList.Count);
          setlength(LyCh2, LMeaList.Count);
          GetPlotData(2, LMeaList, LxCh2, LyCh2);
        end;

        if not (LCh3 = '') then
        begin
          setlength(LxCh3, LMeaList.Count);
          setlength(LyCh3, LMeaList.Count);
          GetPlotData(3, LMeaList, LxCh3, LyCh3);
        end;

        with FPlotForm.Plot do
        begin
          TextTitel     := LName;
          TextLinks     := '';
          TextUnten     := 'Time [ day ]';
          SymbolForm    := plQuadrat;
          SymbolDicke   := 0;
          SymbolFarbe   := clBlack;
          SymbolGroesse := 5;

          //Plot Channel 1
          if not (LCh1 = '') then
          begin
            TextLinks := 'GREEN: Ch1 (' + LCh1 + ') [ % ]';
            PlotSingle(LxCh1, LyCh1);
            PKurvenEintrag(Kurven.Items[0])^.Symbol.Color := clGreen;
          end;

          //Plot Channel 2
          if not (LCh2 = '') then
          begin
            if (TextLinks = '') then
            begin
              TextLinks := 'RED: Ch2 (' + LCh2 + ') [ % ]';
              PlotSingle(LxCh2, LyCh2);
            end
            else begin
              TextLinks := TextLinks + '    RED: Ch2 (' + LCh2 + ') [ % ]';
              OverlaySingle(LxCh2, LyCh2, true);                  
            end;
            PKurvenEintrag(Kurven.Items[Kurven.Count - 1])^.Symbol.Color := clRed;
          end;

          //Plot Channel 3
          if not (LCh3 = '') then
          begin
            if (TextLinks = '') then
            begin
              TextLinks := 'BLUE: Ch3 (' + LCh3 + ') [ % ]';
              PlotSingle(LxCh3, LyCh3);
            end
            else begin
              TextLinks := TextLinks + '    BLUE: Ch3 (' + LCh3 + ') [ % ]';
              OverlaySingle(LxCh3, LyCh3, true);
            end;
            PKurvenEintrag(Kurven.Items[Kurven.Count - 1])^.Symbol.Color := clBlue;
          end;

          AutoScale;

          BereichSetzen(Scale.MinWertX,Scale.MinWertY,Scale.MaxWertX,Scale.MaxWertY);
          ReOverSingle;
          FPlotForm.Show;
        end;
      end
      else
        MessageDlg('Please enter here only positive integers!', mtWarning, [mbOk], 0);
    except
    end;
  end;

  if Assigned(LMeaList) then LMeaList.Free;
end;


procedure TMainForm.ClearHistoryMeItClick(Sender: TObject);
begin
  lvLog.Clear;
end;



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//   << T C o o r d i n a t o r - T h r e a d >>   - - - - - - - - - - - - - - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCoordinatorThread.CreateWithProperties(const ACooId: integer; ADataMgmt: TDataMgmt);
begin
  inherited Create(true);

  FFrame     := '';
  FFrameLen  := 0;
  FComPort   := nil;
  FCooId     := ACooId;
  FDataMgmt  := ADataMgmt;
  FLock      := TCriticalSection.Create;
  FQueue     := TObjectList.Create(true);
  FStartTime := now;
  FRetryAttempts := 0;
  FDestroying := Integer(false);          
  FreeOnterminate := false;

  CreateCoordinatorJob(0,'','');
end;

destructor TCoordinatorThread.Destroy;
begin
  SetDestroying(true);
  Terminate;
  if Suspended then
    Resume;
  WaitFor;

  FLock.Acquire;
  try
    FDataMgmt := nil;
    if Assigned(FComPort) then FComPort.Free;
    FQueue.Free;
  finally
    FLock.Release;
    FLock.Free;
  end;
  
  inherited Destroy;
end;


function TCoordinatorThread.GetComPort(var AName: string): string;
var LCoordinator: TCoordinator;
begin
  Result := '';
  AName  := '';

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LCoordinator := GetCooItem(FCooId);
    if Assigned(LCoordinator) then
    begin
      Result := LCoordinator.coo_comport;
      AName  := LCoordinator.coo_name;
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;


function TCoordinatorThread.GetDestroying: boolean;
var
  temp: Integer;
begin
  InterlockedExchange(temp, FDestroying);
  result := boolean(temp);
end;

procedure TCoordinatorThread.OnDataRxHandler(AData: string);
var i: integer;

  procedure rxd(c: char);
  begin
    if (length(FFrame) > 0) then
    begin
      if (length(FFrame) <= 3) then
      begin
        FFrame := FFrame + c;
        if (length(FFrame) = 3)
         then FFrameLen := ord(FFrame[2])*256 + ord(FFrame[3]) + 4;
      end
      else if (length(FFrame) < FFrameLen) then FFrame := FFrame + c;
    end
    else begin
      if (c = #126) then FFrame := #126;
      FFrameLen := 0;
    end;

    if (length(FFrame) = FFrameLen) and not (FFrameLen = 0) then
    begin
      CreateCoordinatorJob(2, FFrame, '');
      FFrame    := '';
      FFrameLen := 0;
    end;
  end;

begin
  try for i := 1 to length(AData) do rxd(AData[i]);
  finally
  end;
end;


procedure TCoordinatorThread.CreateCoordinatorJob(const AType: Byte; const ARequest, AAddress: string);
var LJob: TCoordinatorJob;
begin
  LJob            := TCoordinatorJob.Create;
  LJob.jobType    := AType;
  LJob.jobRequest := ARequest;
  LJob.jobAddress := AAddress;
  
  try
    FLock.Acquire;
    FQueue.Add(LJob);
  finally
    FLock.Release;
  end;

  Resume;
end;

procedure TCoordinatorThread.CreateMainFormJob(const AType: byte; const AText: string);
var LMsg: TMainFormJob;
begin
  LMsg         := TMainFormJob.Create;
  LMsg.jobType := AType;
  LMsg.jobId   := FCooId;
  LMsg.jobText := AText;

  MainForm.AddMainFormJob(LMsg);
end;

procedure TCoordinatorThread.Execute;
begin
  while not Terminated do WorkThroughQueue;
end;

procedure TCoordinatorThread.WorkThroughQueue;
type TCoordJobIdent = (cjiConfigComPort = 0, cjiDataSend, cjiDataReceive, cjiConStatus, cjiXBeeStatus, cjiUnknown);
const JobIdents : Array[TCoordJobIdent] of string =
                    ('Configuration',
                     'Send Frame',
                     'ReceiveFrame',
                     'Connection Status',
                     'XBee Status',
                     'Unknown');

  function CoordName: string;
  var lCoordinator: TCoordinator;
  begin
    LCoordinator := FDataMgmt.GetCooItem(FCooId);
    if Assigned(LCoordinator) then
      Result := LCoordinator.coo_name;
  end;                       

  function QueueCount: Integer;
  begin
    FLock.Enter;
    try
      result := FQueue.Count;
    finally
      FLock.Leave;
    end;
  end;

  function QueuePop: TCoordinatorJob;
  begin
    result := nil;
    FLock.Enter;
    try
      if FQueue.Count > 0 then
      begin
        TObject(result) := FQueue[0];
        FQueue.OwnsObjects := false;
        FQueue.Delete(0);
        FQueue.OwnsObjects := true;
      end;
    finally
      FLock.Leave;
    end;
  end;

var lJobType, lRetriesDone : Integer;
    Err: boolean;
    lJob: TCoordinatorJob;
begin
  while (QueueCount > 0) and not Destroying do
  try
    lJob := QueuePop;
    try
      lRetriesDone := 0;
      Err := false;
      lJobType := Integer(cjiUnknown);
      repeat
        try
          lJobType := lJob.jobType;
          case ljobType of
          { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
           0 : ConfigComPort;                                // Configuration
          { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
           1 : DataToModule(lJob);   // Send Frame
          { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
           2 : DataFromCoo(lJob);      // Receive Frame
          { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
           3 : CheckStatus;                                  // Connection Status
          { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
           4 : CheckXBeeStatus(lJob);  // XBee Status
          { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
          else
            begin
              CreateMainFormJob(1,'Unknown job in Coordinator Queue!');
              lJobType := Integer(cjiUnknown);
            end;
          end;
          Err := false;
        except
          on E: Exception do
          begin
            Err := true;
            lRetriesDone := lRetriesDone + 1;
            CreateMainFormJob(1, Format('Unexpected error in coordinator ''%s'', job ''%s'', Msg: %s, remaining attempts: %d',
                                        [CoordName, JobIdents[TCoordJobIdent(lJobType)], E.Message, RetryAttempts-lRetriesDone]));
          end;
        end;
      until (not Err) or (RetryAttempts <= lRetriesDone);
    finally
      lJob.Free;
      if (QueueCount = 0) or Destroying then DbDisconnect;
    end;
  except
    on E: Exception do
    begin
      CreateMainFormJob(1, Format('Unexpected error in coordinator ''%s'', Msg: %s.',
                                  [CoordName, E.Message]));
    end;
  end;
  if not Destroying then Suspend;
end;


procedure TCoordinatorThread.ConfigComPort;
var LName, LPort: string;
begin
  LPort := GetComPort(LName);
  if (LPort = '') then
    CreateMainFormJob(3,'No COM port found for this coordinator!')
  else begin
    FComPort := TVirtualComPort.CreateWithProperties(LPort);
    if FComPort.IsOpened then
    begin
      CreateMainFormJob(0,LName + ' successfully activated.');
      FComPort.OnDataRx := OnDataRxHandler;
    end
    else begin
      if (FComPort.GetPort = '') then
        CreateMainFormJob(3,LPort + ' does not exist (' + LName + ')!')
      else
        CreateMainFormJob(3,'Opening ' + LPort + ' failed (' + LName + ')!');
    end;
  end;
end;

procedure TCoordinatorThread.DataToModule(AJob: TCoordinatorJob);
begin
  TXRequest(AJob.jobRequest, HexStringToAddress(AJob.jobAddress));
end;

procedure TCoordinatorThread.DataFromCoo(AJob: TCoordinatorJob);
begin
  with AJob do
  try
    if CheckChkSum(jobRequest) then
    begin
      case jobRequest[4] of
       #0  : ;                         // TX Request 64-bit Address
       #1  : ;                         // TX Request 16-bit Address
       #8  : ;                         // AT Command
       #9  : ;                         // AT Request
       #128: RXPacket(jobRequest);     // RX Packet 64-bit Address
       #179: ;                         // RX Packet 16-bit Address
       #136: ATResponse(jobRequest);   // AT Response
       #137: ;                         // TX Status
       #138: ;                         // Modem Status
      else CreateMainFormJob(1,'Bad Frame received (API identifier)!');
      end;
    end
    else CreateMainFormJob(1,'Bad Frame received (CheckSum)!');
  finally
  end;
end;

function TCoordinatorThread.CheckStatus: boolean;
var LNewStatus:   byte;
    LCoordinator: TCoordinator;
begin
  Result := FComPort.Reopen;

  if Result then
    LNewStatus := 1
  else
    LNewStatus := 2;

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LCoordinator := GetCooItem(FCooId);
    if Assigned(LCoordinator) then
      with LCoordinator do
      begin
        if (coo_status = LNewStatus) then
        begin
          if (LNewStatus = 1) then
            CreateMainFormJob(2, coo_name + ' current status: ' + GetCooStatus(LNewStatus) + '!')
          else
            CreateMainFormJob(0, coo_name + ' current status: ' + GetCooStatus(LNewStatus) + '!')
        end
        else begin
          if (LNewStatus = 1) then
            CreateMainFormJob(2, coo_name + ' current status: ' + GetCooStatus(LNewStatus) + '!')
          else
            CreateMainFormJob(1, coo_name + ' current status: ' + GetCooStatus(LNewStatus) + '!');

          CreateMainFormJob(9,'');
        end;

        coo_status := LNewStatus;
      end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TCoordinatorThread.CheckXBeeStatus(AJob: TCoordinatorJob);
begin
  if (length(AJob.jobRequest) = 2) then
  begin
    if CheckStatus then
      ATCommand(AJob.jobRequest);
  end
  else
    CreateMainFormJob(1, 'Invalid AT Command: ' + #39 + AJob.jobRequest + #39);
end;


procedure TCoordinatorThread.DbDisconnect;
begin
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    DisconnectDbConnection;
  finally
    UnlockAfterWorkingWithList;
  end;
end;


procedure TCoordinatorThread.TXRequest(const AMsg, AAddress: string);
var LFrameData: string;

  function DataLength(const ALength: byte): string;
  begin
    Result := chr(ALength div 256) + chr(ALength mod 256);
  end;

begin
  try
    LFrameData := #0         // API Identifier
                + #1         // Frame ID
                + AAddress   // Destination Address
                + #1         // Options
                + AMsg;      // RF Data

    FComPort.WriteData(#126                           // Start Delimiter
                     + DataLength(length(LFrameData)) // Length
                     + LFrameData                     // Frame Data
                     + CalcChkSum(LFrameData));       // Checksum
  finally
  end;    
end;

procedure TCoordinatorThread.ATCommand(const AMsg: string);
var LFrameData: string;

  function DataLength(const ALength: byte): string;
  begin
    Result := chr(ALength div 256) + chr(ALength mod 256);
  end;

begin
  try
    LFrameData := #8       // API Identifier
                + #1       // Frame ID
                + AMsg;    // AT Command (incl. Parameters)
                
    FComPort.WriteData(#126                           // Start Delimiter
                     + DataLength(length(LFrameData)) // Length
                     + LFrameData                     // Frame Data
                     + CalcChkSum(LFrameData));       // Checksum
  finally
  end;
end;


function TCoordinatorThread.HexStrToInt(const HexStr: string): integer;
var x, y: Byte;
begin
  Result := 0;
  try
    y := 0;
    for x := Length(HexStr) downto 1 do
    begin
      case StrToIntDef(HexStr[x], 99) of
        0..9: Result := Result + Trunc(SqrX(16, y)) * StrToInt(HexStr[x]);
      end;
      case HexStr[x] of
        'A', 'a' : Result := Result + Trunc(SqrX(16, y)) * 10;
        'B', 'b' : Result := Result + Trunc(SqrX(16, y)) * 11;
        'C', 'c' : Result := Result + Trunc(SqrX(16, y)) * 12;
        'D', 'd' : Result := Result + Trunc(SqrX(16, y)) * 13;
        'E', 'e' : Result := Result + Trunc(SqrX(16, y)) * 14;
        'F', 'f' : Result := Result + Trunc(SqrX(16, y)) * 15;
        '0'..'9' : ;
        else begin
          Result := -1;
          exit;
        end;
      end;
      Inc(y);
    end;
  except
  end;
end;

function TCoordinatorThread.SqrX(const basis: real; const exponent: integer): real;
var x: Byte;
begin
  Result := Basis;
  try
    case Exponent of
      0 : Result := 1;
      1 : Result := Basis
      else for x := 2 to Exponent do Result := Result * Basis;
    end;
  except
  end;
end;


function TCoordinatorThread.CheckChkSum(const AFrame: string): boolean;
begin
  Result := false;
  if (length(AFrame) >= 4)
   then Result := (AFrame[length(AFrame)] = CalcChkSum(copy(AFrame,4,length(AFrame)-4)));
end;

function TCoordinatorThread.CalcChkSum(const AString: string): char;
var i, tmp: integer;
begin
  Result := #0;
  try
    tmp := 0;
    for i := 1 to length(AString) do tmp := tmp + ord(AString[i]);
    tmp := tmp mod 256;
    Result := chr(255 - tmp);
  except
  end;
end;


procedure TCoordinatorThread.RXPacket(const AFrame: string);
begin
  case AFrame[15] of
  #1 : if (length(AFrame) > 17) then MsgLogin(    AFrame);  // Login
  #2 : if (length(AFrame) > 17) then MsgLogin(    AFrame);  // Login (create)
  #3 : if (length(AFrame) > 23) then MsgStatus(   AFrame);  // Status (long)
  #4 : if (length(AFrame) > 39) then MsgOptions(  AFrame);  // Options (user)
  #5 : if (length(AFrame) > 21) then MsgFillBegin(AFrame);  // Fill begin
  #6 : if (length(AFrame) > 23) then MsgStatus(   AFrame);  // Status (short)
  #7 : if (length(AFrame) > 23) then MsgStatus(   AFrame);  // Fill end
  #8 : if (length(AFrame) > 22) then MsgStatus(   AFrame);  // Logout
  #9 : if (length(AFrame) > 18) then MsgActive(   AFrame);  // Awake
  #10: if (length(AFrame) > 15) then MsgError(    AFrame);  // Error
  #11: if (length(AFrame) > 23) then MsgStatus(   AFrame);  // Status (Resonse)
  #12: if (length(AFrame) > 39) then MsgOptions(  AFrame);  // Options (Response)
  #14: if (length(AFrame) > 18) then MsgPositions(AFrame);  // Positions (Response)
  #16: if (length(AFrame) > 20) then MsgPassword( AFrame);  // Password (Response)
  #18: if (length(AFrame) > 19) then MsgSleepTime(AFrame);  // Sleep time (Response)
  #21: if (length(AFrame) > 19) then MsgAwakeTime(AFrame);  // Awake time (Response)
  #99: if (length(AFrame) > 21) then MsgDeviceLvl(AFrame);  // Device Level
  else CreateMainFormJob(1,'Bad Frame received (MsgCode:' + intToStr(ord(AFrame[15])) + ')!');
  end;
end;

procedure TCoordinatorThread.ATResponse(const AFrame: string);
var LCoo: TCoordinator;
    i: integer;
    s: string;
begin
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LCoo := GetCooItem(FCooId);
    if Assigned(LCoo) then
    begin
      LCoo.coo_xbeestatus := 0;

      if (length(AFrame) > 8) then
      begin
        if (AFrame[8] = #0) then
        begin
          s := '';
          for i := 9 to length(AFrame) - 1 do
            s := s + ' ' + inttostr(ord(AFrame[i]));

          CreateMainFormJob(2, LCoo.coo_name + ' received AT RESPONSE (Command: '
                               + AFrame[6] + AFrame[7] + '):' + s);
        end
        else
          CreateMainFormJob(1, LCoo.coo_name + ' received AT RESPONSE (Command: '
                               + AFrame[6] + AFrame[7] + '): ERROR');
      end
      else
        CreateMainFormJob(1, LCoo.coo_name
                             + ' received AT RESPONSE: ERROR (Frame to short)');
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;


procedure TCoordinatorThread.MsgLogin(const AFrame: string);
var LResponse:  string;
    LModAddr8:  string;
    LModAddr16: string;
    LVesName:   word;
    LVessel:    TVessel;
    LModule:    TModule;
    LDbState:   boolean;
    LLoginOk:   boolean;

  function VesselIsActive: boolean;
  var i: integer;
  begin
    Result := false;

    with FDataMgmt do
    for i := 0 to ModuleList.Count - 1 do
      if (TModule(ModuleList[i]).mod_vessel = LVessel) then
      begin
        if not (TModule(ModuleList[i]).mod_status = 5) then
        begin
          Result    := true;
          LResponse := ModNumTo1Byte(TModule(ModuleList[i]).mod_name)
                     + ModPosTo3Byte(TModule(ModuleList[i]).mod_position);
        end
        else LogoutOriginal(TModule(ModuleList[i]));
        break;
      end;
  end;

begin
  LLoginOk   := false;
  LResponse  := '';
  LVesName   := ord(AFrame[16])*256 + ord(AFrame[17]);
  LModAddr8  := ReadAddress(AFrame);
  LModAddr16 := AddressToHexString(LModAddr8);

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LDbState := RefreshVesListByDB and RefreshModListByDb;
    LVessel  := GetVesItemByName(LVesName);
    LModule  := GetModItemByAddr(LModAddr16);

    if Assigned(LModule) then
     if LDbState          then
      if Assigned(LVessel) then
       if not VesselIsActive then
        if not (LVessel.ves_at_hzb_since = '') then
         if LVessel.ves_options_in_db or (AFrame[15] = #2) then
         begin
           LLoginOk := true;
           if (AFrame[15] = #2) and not LVessel.ves_options_in_db
            then SetDefaultOptions(LVessel);
           LoginOriginal(LVessel, LModule);
           TXRequest(AFrame[15] + #0 + 'L' + VesOptions(LVessel) + ModOptions(LModule), LModAddr8);
           LResponse := 'Vessel ' + intToStr(LVesName) + ' is connecting via ' + LModule.mod_name + '.';
         end
         else TXRequest(AFrame[15] + #0 + 'V', LModAddr8)
        else TXRequest(AFrame[15] + #0 + 'W', LModAddr8)
       else TXRequest(AFrame[15] + #0 + 'S' + LResponse, LModAddr8)
      else TXRequest(AFrame[15] + #0 + 'W', LModAddr8)
     else begin
       LLoginOk := true;
       CreateMainFormJob(1,'Database connection failed!');
       if not Assigned(LVessel) then LVessel := CreateNewVessel(LVesName);
       if not LVessel.ves_options_in_db then SetDefaultOptions(LVessel);
       LoginOriginal(LVessel, LModule);
       TXRequest(AFrame[15] + #0 + 'D' + VesOptions(LVessel) + ModOptions(LModule), LModAddr8);
       LResponse := 'Vessel ' + intToStr(LVesName) + ' is connecting via ' + LModule.mod_name + '.';
     end
    else begin
      TXRequest(AFrame[15] + #0 + 'M', LModAddr8);
      CreateMainFormJob(1,'Unknown Module with address ' + LModAddr16 + ' tried to login vessel ' + intToStr(LVesName) + '!');
    end;

    if LLoginOk then
      with LModule do
      begin
        mod_getPositions := true;
        mod_getPassword  := true;
        mod_getSleepTime := true;
        mod_getAwakeTime := true;
      end;
  finally
    UnlockAfterWorkingWithList;
  end;

  if LLoginOk then
  begin
    CreateMainFormJob(2, LResponse);
    CreateMainFormJob(7,'');
    CreateMainFormJob(8,'');
    beep(3000, 80);
    TXRequest(#15 + #0 + GetPositions, LModAddr8);
    TXRequest(#17 + #0 + GetPassword,  LModAddr8);
    TXRequest(#19 + #0 + GetSleepTime, LModAddr8);
    TXRequest(#22 + #0 + GetAwakeTime, LModAddr8);
  end;
end;

procedure TCoordinatorThread.MsgStatus(const AFrame: string);
var LResponse:  string;
    LModAddr8:  string;
    LModAddr16: string;
    LVesName:   word;
    LVessel:    TVessel;
    LModule:    TModule;
    LLevel:     double;
    LAccu:      byte;
    LPressure:  word;

begin
  LResponse  := '';
  LVesName   := ord(AFrame[16])*256 + ord(AFrame[17]);
  LModAddr8  := ReadAddress(AFrame);
  LModAddr16 := AddressToHexString(LModAddr8);

  if (AFrame[15] in [#3, #7, #8])
   then TXRequest(AFrame[15] + #0, LModAddr8);

  LLevel := (ord(AFrame[18]) * 256 + ord(AFrame[19]));
  if (LLevel <= 0)
   then
     LLevel := 0
   else
     if (LLevel >= 2000) then
        LLevel := 200
     else
       LLevel := LLevel / 10;

  LAccu := ord(AFrame[20]);
  if (LAccu > 200) then LAccu := 200;

  LPressure := ord(AFrame[21]) * 256 + ord(AFrame[22]);

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVessel := GetVesItemByName(LVesName);
    LModule := GetModItemByAddr(LModAddr16);

    if Assigned(LVessel) and Assigned(LModule) then
    begin
      if MeasurementToDb(AFrame[15], LVessel, LModule, FCooId, DateTimeToStr(Now), LLevel, LPressure, LAccu) then
        LastActiveToDb(LModule.mod_id)
      else
        MeasurementToReg(AFrame[15], LVessel, LModule, LLevel, LPressure, LAccu);

      with LVessel, LModule do
      begin
        if (LAccu <= mod_criticvoltage)
          then CreateMainFormJob(0, 'WARNING: '
                                    + mod_name
                                    + ' has only '
                                    + intToStr(LAccu)
                                    + '% battery capacity!');

        if (((ves_pressure + 100) < LPressure)
          and not (ves_pressure = 0)
          and not (mod_status in [3, 4])
          and not (AFrame[15] in [#6, #7]))
          then CreateMainFormJob(0, 'WARNING: '
                                    + mod_name
                                    + ' detected an increasing pressure in Vessel '
                                    + intToStr(ves_name)
                                    + ' (even though vessel is not filling)!');
      end;

      LVessel.ves_he_level    := LLevel;
      LVessel.ves_pressure    := LPressure;
      LModule.mod_accumulator := LAccu;
      LModule.mod_lastactive  := DateTimeToStr(Now);

      if (AFrame[15] = #8) then
      begin
        LogoutOriginal(LModule);
        LResponse :=  'Vessel ' + intToStr(LVesName) + ' has disconnected via ' + LModule.mod_name + '.';
        LModule.mod_position := '';
        LVessel.ves_position := '';
      end
      else with LModule do
      begin
        case AFrame[15] of
         #3 : if mod_status in [0, 1, 5] then mod_status := 2;
         #6 : mod_status := 4;
         #7 : begin
                mod_status           := 2;
                mod_position         := '';
                LVessel.ves_position := '';
              end;
         #11: if mod_status in [0, 1, 5] then mod_status := 2;
        end;

        mod_statusbyte  := AFrame[23];
        mod_coordinator := GetCooItem(FCooId);
        mod_vessel      := LVessel;
        mod_getStatus   := false;
        LResponse       := 'Vessel ' + intToStr(LVesName)
                           + ' sent status message via '
                           + LModule.mod_name + '.';

        CheckRequestsAndStatusByte(LModule, LModAddr8);
      end;
    end
    else CreateMainFormJob(1, 'Module or Vessel not found!');;
  finally
    UnlockAfterWorkingWithList;
  end;

  if not (LResponse = '') then
  begin
    CreateMainFormJob(2, LResponse);
    CreateMainFormJob(7,'');
    CreateMainFormJob(8,'');
  end;
end;

procedure TCoordinatorThread.MsgOptions(const AFrame: string);
var LModAddr8:  string;
    LModAddr16: string;
    LVesName:   word;
    LVessel:    TVessel;
    LModule:    TModule;
begin
  LVesName   := ord(AFrame[16])*256 + ord(AFrame[17]);
  LModAddr8  := ReadAddress(AFrame);
  LModAddr16 := AddressToHexString(LModAddr8);

  if (AFrame[15] = #4) then TXRequest(AFrame[15] + #0, LModAddr8);
                                                    
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVessel := GetVesItemByName(LVesName);
    LModule := GetModItemByAddr(LModAddr16);

    if Assigned(LVessel) and Assigned(LModule) then
    begin
      if (LModule.mod_status = 5) then LModule.mod_status := 2;
      LModule.mod_lastactive  := DateTimeToStr(Now);
      LModule.mod_statusbyte  := AFrame[39];
      LModule.mod_coordinator := GetCooItem(FCooId);
      LModule.mod_vessel      := LVessel;
      LModule.mod_getOptions  := false;

      CreateMainFormJob(2, 'Vessel ' + intToStr(LVesName) + ' sent options via ' + LModule.mod_name + '.');
      if ChangedVesselOptions(AFrame, LVessel)
       then CreateMainFormJob(10,'Vessel ' + intToStr(LVesName) + ' sent changed vessel options via ' + LModule.mod_name + '.');
      if ChangedModuleOptions(AFrame, LModule)
       then CreateMainFormJob(11,'Vessel ' + intToStr(LVesName) + ' sent changed module options via ' + LModule.mod_name + '.');

      CheckRequestsAndStatusByte(LModule, LModAddr8);
      CreateMainFormJob(7,'');
      CreateMainFormJob(8,'');
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TCoordinatorThread.MsgFillBegin(const AFrame: string);
var LModAddr8:  string;
    LModAddr16: string;
    LVesName:   word;
    LVessel:    TVessel;
    LModule:    TModule;
begin
  LVesName   := ord(AFrame[16])*256 + ord(AFrame[17]);
  LModAddr8  := ReadAddress(AFrame);
  LModAddr16 := AddressToHexString(LModAddr8);

  TXRequest(AFrame[15] + #0, LModAddr8);

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVessel := GetVesItemByName(LVesName);
    LModule := GetModItemByAddr(LModAddr16);

    if Assigned(LVessel) and Assigned(LModule) then
    begin
      LModule.mod_status      := 3;
      LModule.mod_lastactive  := DateTimeToStr(Now);
      LModule.mod_statusbyte  := AFrame[21];
      LModule.mod_coordinator := GetCooItem(FCooId);
      LModule.mod_vessel      := LVessel;
      LModule.mod_position    := AFrame[18]+AFrame[19]+AFrame[20];
      LVessel.ves_position    := AFrame[18]+AFrame[19]+AFrame[20];

      CreateMainFormJob(2, 'Vessel ' + intToStr(LVesName) + ' is filling now via ' + LModule.mod_name + '.');

      CheckRequestsAndStatusByte(LModule, LModAddr8);
      CreateMainFormJob(7,'');
      CreateMainFormJob(8,'');
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TCoordinatorThread.MsgActive(const AFrame: string);
var LModAddr8:  string;
    LModAddr16: string;
    LVesName:   word;
    LVessel:    TVessel;
    LModule:    TModule;
begin
  LVesName   := ord(AFrame[16])*256 + ord(AFrame[17]);
  LModAddr8  := ReadAddress(AFrame);
  LModAddr16 := AddressToHexString(LModAddr8);

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVessel := GetVesItemByName(LVesName);
    LModule := GetModItemByAddr(LModAddr16);

    if Assigned(LVessel) and Assigned(LModule) then
    begin
      if (LModule.mod_status = 0) or (LModule.mod_status = 5) then
      begin
        LModule.mod_status := 2;
        CreateMainFormJob(7,'');
        CreateMainFormJob(8,'');
      end;
      LModule.mod_lastactive  := DateTimeToStr(Now);
      LModule.mod_statusbyte  := AFrame[18];
      LModule.mod_coordinator := GetCooItem(FCooId);
      LModule.mod_vessel      := LVessel;

      CreateMainFormJob(2, 'Vessel ' + intToStr(LVesName) + ' sent awake message via ' + LModule.mod_name + '.');

      CheckRequestsAndStatusByte(LModule, LModAddr8);
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TCoordinatorThread.MsgError(const AFrame: string);
var LModule:    TModule;
begin
  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LModule := GetModItemByAddr(AddressToHexString(ReadAddress(AFrame)));

    if Assigned(LModule) then
    begin
      if (LModule.mod_status = 5) then
      begin
        LModule.mod_status := 2;
        CreateMainFormJob(7,'');
        CreateMainFormJob(8,'');
      end;
      LModule.mod_lastactive  := DateTimeToStr(Now);
      LModule.mod_coordinator := GetCooItem(FCooId);

      CreateMainFormJob(1, LModule.mod_name + ' got an incomplete message!');
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TCoordinatorThread.MsgPositions(const AFrame: string);
var LModAddr8:  string;
    LModAddr16: string;
    LVesName:   word;
    LVessel:    TVessel;
    LModule:    TModule;
    LFlag:      boolean;
begin
  LVesName   := ord(AFrame[16])*256 + ord(AFrame[17]);
  LModAddr8  := ReadAddress(AFrame);
  LModAddr16 := AddressToHexString(LModAddr8);

  LFlag      := PositionListIsOk(AFrame);

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVessel := GetVesItemByName(LVesName);
    LModule := GetModItemByAddr(LModAddr16);

    if Assigned(LVessel) and Assigned(LModule) then
    begin
      if (LModule.mod_status = 5) then
      begin
        LModule.mod_status := 2;
        CreateMainFormJob(7,'');
        CreateMainFormJob(8,'');
      end;
      LModule.mod_lastactive   := DateTimeToStr(Now);
      LModule.mod_statusbyte   := AFrame[length(AFrame)-1];
      LModule.mod_coordinator  := GetCooItem(FCooId);
      LModule.mod_vessel       := LVessel;
      LModule.mod_getPositions := false;
      LModule.mod_setPositions := not LFlag;
      
      CreateMainFormJob(2, 'Vessel ' + intToStr(LVesName) + ' sent position list via ' + LModule.mod_name + '.');

      CheckRequestsAndStatusByte(LModule, LModAddr8);
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TCoordinatorThread.MsgPassword(const AFrame: string);
var LModAddr8:  string;
    LModAddr16: string;
    LVesName:   word;
    LVessel:    TVessel;
    LModule:    TModule;
    LFlag:      boolean;
begin
  LVesName   := ord(AFrame[16])*256 + ord(AFrame[17]);
  LModAddr8  := ReadAddress(AFrame);
  LModAddr16 := AddressToHexString(LModAddr8);

  LFlag      := PasswordIsOk(ord(AFrame[18])*256 + ord(AFrame[19]));

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVessel := GetVesItemByName(LVesName);
    LModule := GetModItemByAddr(LModAddr16);

    if Assigned(LVessel) and Assigned(LModule) then
    begin
      if (LModule.mod_status = 5) then
      begin
        LModule.mod_status := 2;
        CreateMainFormJob(7,'');
        CreateMainFormJob(8,'');
      end;
      LModule.mod_lastactive  := DateTimeToStr(Now);
      LModule.mod_statusbyte  := AFrame[20];
      LModule.mod_coordinator := GetCooItem(FCooId);
      LModule.mod_vessel      := LVessel;
      LModule.mod_getPassword := false;
      LModule.mod_setPassword := not LFlag;
      
      CreateMainFormJob(2, 'Vessel ' + intToStr(LVesName) + ' sent password via ' + LModule.mod_name + '.');

      CheckRequestsAndStatusByte(LModule, LModAddr8);
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TCoordinatorThread.MsgAwakeTime(const AFrame: string);
var LModAddr8:  string;
    LModAddr16: string;
    LVesName:   word;
    LVessel:    TVessel;
    LModule:    TModule;
    LFlag:      boolean;
begin
  LVesName   := ord(AFrame[16])*256 + ord(AFrame[17]);
  LModAddr8  := ReadAddress(AFrame);
  LModAddr16 := AddressToHexString(LModAddr8);

  LFlag      := AwakeTimeIsOk(ord(AFrame[18]));

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVessel := GetVesItemByName(LVesName);
    LModule := GetModItemByAddr(LModAddr16);

    if Assigned(LVessel) and Assigned(LModule) then
    begin
      if (LModule.mod_status = 5) then
      begin
        LModule.mod_status := 2;
        CreateMainFormJob(7,'');
        CreateMainFormJob(8,'');
      end;
      LModule.mod_lastactive   := DateTimeToStr(Now);
      LModule.mod_statusbyte   := AFrame[19];
      LModule.mod_coordinator  := GetCooItem(FCooId);
      LModule.mod_vessel       := LVessel;
      LModule.mod_getAwakeTime := false;
      LModule.mod_setAwakeTime := not LFlag;
      
      CreateMainFormJob(2, 'Vessel ' + intToStr(LVesName) + ' sent awake time via ' + LModule.mod_name + '.');

      CheckRequestsAndStatusByte(LModule, LModAddr8);
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TCoordinatorThread.MsgSleepTime(const AFrame: string);
var LModAddr8:  string;
    LModAddr16: string;
    LVesName:   word;
    LVessel:    TVessel;
    LModule:    TModule;
    LFlag:      boolean;
begin
  LVesName   := ord(AFrame[16])*256 + ord(AFrame[17]);
  LModAddr8  := ReadAddress(AFrame);
  LModAddr16 := AddressToHexString(LModAddr8);

  LFlag      := SleepTimeIsOk(ord(AFrame[18]));

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LVessel := GetVesItemByName(LVesName);
    LModule := GetModItemByAddr(LModAddr16);

    if Assigned(LVessel) and Assigned(LModule) then
    begin
      if (LModule.mod_status = 5) then
      begin
        LModule.mod_status := 2;
        CreateMainFormJob(7,'');
        CreateMainFormJob(8,'');
      end;
      LModule.mod_lastactive   := DateTimeToStr(Now);
      LModule.mod_statusbyte   := AFrame[19];
      LModule.mod_coordinator  := GetCooItem(FCooId);
      LModule.mod_vessel       := LVessel;
      LModule.mod_getSleepTime := false;
      LModule.mod_setSleepTime := not LFlag;
      
      CreateMainFormJob(2, 'Vessel ' + intToStr(LVesName) + ' sent sleep time via ' + LModule.mod_name + '.');

      CheckRequestsAndStatusByte(LModule, LModAddr8);
    end;
  finally
    UnlockAfterWorkingWithList;
  end;
end;

procedure TCoordinatorThread.MsgDeviceLvl(const AFrame: string);
var LModAddr8:  string;
    LModAddr16: string;
    LCryo:      TCryostat;
    LCh1Level:  integer;
    LCh2Level:  integer;
    LCh3Level:  integer;

begin
  LModAddr8  := ReadAddress(AFrame);
  LModAddr16 := AddressToHexString(LModAddr8);

  LCh1Level := (ord(AFrame[16]) * 256 + ord(AFrame[17]));
  LCh2Level := (ord(AFrame[18]) * 256 + ord(AFrame[19]));
  LCh3Level := (ord(AFrame[20]) * 256 + ord(AFrame[21]));

  with FDataMgmt do
  try
    LockWhileWorkingWithList;
    LCryo := GetDevItemByAddr(LModAddr16);

    if not Assigned(LCryo) then
    begin
      LCryo             := TCryostat.Create;
      LCryo.dev_id      := 0;
      LCryo.dev_name    := 'New Device ' + LModAddr8;
      LCryo.dev_address := LModAddr16;
      LCryo.dev_comment := 'Created by system: ' + DateTimeToStr(now);
      if InsertIlmDeviceToDb(LCryo) then
      begin
        LCryo.Free;
        RefreshDevListByDB;
        LCryo := GetDevItemByAddr(LModAddr16);
      end
      else
        FreeAndNil(LCryo);
    end;

    if Assigned(LCryo) then
    begin
      if LCryo.dev_ch1_enabled then
        LCryo.dev_ch1_level := LCryo.dev_ch1_span * LCh1Level + LCryo.dev_ch1_zero
      else
        LCryo.dev_ch1_level := 255;

      if LCryo.dev_ch2_enabled then
        LCryo.dev_ch2_level := LCryo.dev_ch2_span * LCh2Level + LCryo.dev_ch2_zero
      else
        LCryo.dev_ch2_level := 255;

      if LCryo.dev_ch3_enabled then
        LCryo.dev_ch3_level := LCryo.dev_ch3_span * LCh3Level + LCryo.dev_ch3_zero
      else
        LCryo.dev_ch3_level := 255;

      if not CryoMeasurementToDb(LCryo.dev_id, FCooId, LCryo.dev_ch1_level, LCryo.dev_ch2_level, LCryo.dev_ch3_level, DateTimeToStr(Now)) then
        CryoMeasurementToReg(LCryo.dev_id, LCryo.dev_ch1_level, LCryo.dev_ch2_level, LCryo.dev_ch3_level);

      LCryo.dev_lastactive  := DateTimeToStr(Now);
      LCryo.dev_coordinator := GetCooItem(FCooId);
    end;
  finally
    UnlockAfterWorkingWithList;
  end;                       
  CreateMainFormJob(20,'');
end;


function TCoordinatorThread.ReadAddress(const AFrame: string): string;
begin
  if (length(AFrame) >= 12)
   then Result := copy(AFrame, 5, 8)
   else Result := '00000000';
end;

function TCoordinatorThread.AddressToHexstring(const AAddress8: string): string;
var i: integer;
begin
  Result := '';
  if (length(AAddress8) > 0) then
    for i := 1 to length(AAddress8) do
      Result := Result + IntToHex(ord(AAddress8[i]),2);
end;

function TCoordinatorThread.HexStringToAddress(const AAddress16: string): string;
begin
  Result := AAddress16;

  if (length(AAddress16) = 16) then
    Result := chr(HexStrToInt(AAddress16[1]  + AAddress16[2]))
            + chr(HexStrToInt(AAddress16[3]  + AAddress16[4]))
            + chr(HexStrToInt(AAddress16[5]  + AAddress16[6]))
            + chr(HexStrToInt(AAddress16[7]  + AAddress16[8]))
            + chr(HexStrToInt(AAddress16[9]  + AAddress16[10]))
            + chr(HexStrToInt(AAddress16[11] + AAddress16[12]))
            + chr(HexStrToInt(AAddress16[13] + AAddress16[14]))
            + chr(HexStrToInt(AAddress16[15] + AAddress16[16]));
end;


procedure TCoordinatorThread.LoginOriginal(AVessel: TVessel; AModule: TModule);
begin
  AVessel.ves_position     := '';

  AModule.mod_lastactive   := DateTimeToStr(Now);
  AModule.mod_status       := 1;
  AModule.mod_coordinator  := FDataMgmt.GetCooItem(FCooId);
  AModule.mod_vessel       := AVessel;
  AModule.mod_position     := '';
  AModule.mod_statusbyte   := #0;

  AModule.mod_getStatus    := false;
  AModule.mod_getOptions   := false;
  AModule.mod_setOptions   := false;
  AModule.mod_getPositions := false;
  AModule.mod_setPositions := false;
  AModule.mod_getPassword  := false;
  AModule.mod_setPassword  := false;
  AModule.mod_getAwakeTime := false;
  AModule.mod_setAwakeTime := false;
  AModule.mod_getSleepTime := false;
  AModule.mod_setSleepTime := false;
  AModule.mod_displayText  := '';
end;

procedure TCoordinatorThread.LogoutOriginal(AModule: TModule);
begin
  AModule.mod_status      := 0;
  AModule.mod_coordinator := nil;
  AModule.mod_vessel      := nil;
end;


function TCoordinatorThread.ModNumTo1Byte(const AName: string): string;
var i:   integer;
    LNr: string;

  function IsNumber: boolean;
  begin
    Result := AName[i] in ['0'..'9'];
  end;

begin
  Result := #0;
  LNr    := '';

  if length(AName) > 3
   then for i := length(AName) downto length(AName) - 2 do
          if IsNumber
           then LNr := AName[i] + LNr
           else break
   else for i := length(AName) downto 0 do
          if IsNumber
           then LNr := AName[i] + LNr
           else break;

  if not (LNr = '') then Result := chr(strToInt(LNr) mod 256);
end;

function TCoordinatorThread.ModPosTo3Byte(const APosition: string): string;
begin
  Result := APosition;

  if (length(Result) > 3)
   then Result := Copy(APosition, 1, 3);

  while (length(Result) < 3)
   do Result := ' ' + Result;
end;


function TCoordinatorThread.CreateNewVessel(const AName: word): TVessel;
var i: integer;
begin
  // DB ERROR -> create dummy vessel with negative id
  Result := TVessel.Create;

  with FDataMgmt do
  for i := -2 downto -2 - VesselList.Count do
    if (GetVesIndex(i) = -1) then
    begin
      Result.ves_id := i;
      break;
    end;

  Result.ves_type_id       := -1;
  Result.ves_name          := AName;
  Result.ves_type          := 'dummy';
  Result.ves_volume        := 0;
  Result.ves_tare          := 0;
  Result.ves_comment1      := 'Created by system (' + dateToStr(now) + ').';
  Result.ves_at_hzb_since  := DateTimeToStr(Now);
  Result.ves_options_in_db := false;

  FDataMgmt.VesselList.Add(Result);
  CreateMainFormJob(1,'New Vessel (Nr: ' + intToStr(AName) + ') was created by system!');
end;


function TCoordinatorThread.VesOptions(AVessel: TVessel): string;
begin
  with AVessel do
  begin
    Result := chr((      ves_longinterval        div 256) mod 256)
            + chr(       ves_longinterval                 mod 256)
            + chr((      ves_shortinterval       div 256) mod 256)
            + chr(       ves_shortinterval                mod 256)
            + chr((Round(ves_minresistance * 10) div 256) mod 256)
            + chr( Round(ves_minresistance * 10)          mod 256)
            + chr((Round(ves_maxresistance * 10) div 256) mod 256)
            + chr( Round(ves_maxresistance * 10)          mod 256)
            + chr((      ves_heattime            div 256) mod 256)
            + chr(       ves_heattime                     mod 256)
            + chr(       ves_adcloops                     mod 256)
            + chr(       ves_filltimeout                  mod 256)
            + chr((      ves_span                div 256) mod 256)
            + chr(       ves_span                         mod 256)
            + chr((     (ves_zero + ZERO_OFFSET) div 256) mod 256)
            + chr(      (ves_zero + ZERO_OFFSET)          mod 256);
  end;
end;

function TCoordinatorThread.ModOptions(AModule: TModule): string;
begin
  with AModule do
  begin
    Result := chr((Round(mod_minvoltage * 10) div 256) mod 256)
            + chr( Round(mod_minvoltage * 10)          mod 256)
            + chr((Round(mod_maxvoltage * 10) div 256) mod 256)
            + chr( Round(mod_maxvoltage * 10)          mod 256)
            + chr(       mod_criticvoltage             mod 256); 
  end;
end;


procedure TCoordinatorThread.SetDefaultOptions(AVessel: TVessel);
begin
  with AVessel do
  begin
    ves_shortinterval := GetRegInteger('Def_Options\','interval_short');
    ves_longinterval  := GetRegInteger('Def_Options\','interval_long');
    ves_minresistance := GetRegFloat(  'Def_Options\','resistance_min');
    ves_maxresistance := GetRegFloat(  'Def_Options\','resistance_max');
    ves_heattime      := GetRegInteger('Def_Options\','heattime');
    ves_adcloops      := GetRegInteger('Def_Options\','adc_cycles');
    ves_filltimeout   := GetRegInteger('Def_Options\','fill_timeout');
    ves_span          := GetRegInteger('Def_Options\','span');
    ves_zero          := GetRegInteger('Def_Options\','zero');
    ves_comment2      := 'Default Options set by system...';

    if (ves_id > -1) and not ves_options_in_db
      then FDataMgmt.InsertVesOptionsToDb(AVessel);

    ves_options_in_db := true;
    CreateMainFormJob(0, 'System set default options for vessel ' + intToStr(ves_name) + '.');
  end;
end;

procedure TCoordinatorThread.SetDestroying(const AValue: boolean);
begin
  InterlockedExchange(FDestroying, Integer(AValue));
end;

function TCoordinatorThread.GetPositions: string;
var z:      integer;
    LReg:   TRegistry;
    LNamen: TStringList;

  function posString(AString: string): string;
  var i,j: integer;
  begin
    Result := '';
    if (length(AString) <= 3) and (pos(';',AString) = 0)
      then Result := AString + ';/'
      else begin
        if (AString[2] = ';') then
        begin
          Result := AString[1] + ';';
          i := 2;
          j := 3;
          while (i > 0) do
          begin
            i := posex(',',AString,j);
            if (i > 2)
              then Result := Result + chr(strtoint(copy(AString,j,i-j)))
              else Result := Result + chr(strtoint(copy(AString,j)));
            j := i + 1;
          end;
          Result := Result + '/';
        end;
      end;
  end;

begin
  Result       := 'HZB;/';
  LReg         := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  LNamen        := TStringList.Create;
  if LReg.KeyExists('Software\HZB_Helium\Position\') then
  try
    LReg.OpenKey('Software\HZB_Helium\Position\', false);
    LReg.GetValueNames(LNamen);
    if (LNamen.Count > 0) then Result := '';
    for z := 0 to LNamen.Count - 1
      do Result := Result + posString(LReg.ReadString(LNamen[z]));
  finally LNamen.Clear;
  end;
  LNamen.Free;
  LReg.CloseKey;
  LReg.Free;
end;

function TCoordinatorThread.GetRetryAttempts: Integer;
begin
  InterlockedExchange(result, FRetryAttempts);
end;

procedure TCoordinatorThread.SetRetryAttempts(const Value: Integer);
begin
  InterlockedExchange(FRetryAttempts, Value);
end;

function TCoordinatorThread.GetPassword: string;
var i: integer;
begin
  i := getRegInteger('','OptionsPW');
  Result := chr((i div 256) mod 256) + chr(i mod 256);
end;

function TCoordinatorThread.GetAwakeTime: string;
begin
  Result := chr(getRegInteger('','AwakeTime') mod 256);
end;

function TCoordinatorThread.GetSleepTime: string;
begin
  Result := chr(getRegInteger('','SleepTime') mod 256);
end;


function TCoordinatorThread.ChangedVesselOptions(const AFrame: string; AVessel: TVessel): boolean;
var LReg:  TRegistry;
    LName: string;
    f1,f2: boolean;
    i:     integer;

    procedure WriteIntValue(AName: string; AValue: integer);
    begin
      LReg.WriteInteger(AName, AValue);
      f1 := true;
    end;

    procedure WriteFloatValue(AName: string; AValue: double);
    begin
      LReg.WriteFloat(AName, AValue);
      f1 := true;
    end;

begin
  f1           := false;
  LReg         := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  LName        := inttostr(ord(AFrame[16])*256 + ord(AFrame[17]));
  f2           := LReg.KeyExists('Software\HZB_Helium\New_VesOptions\' + LName + '\');
  LReg.OpenKey('Software\HZB_Helium\New_VesOptions\' + LName + '\', true);

// Long Interval
  i := ord(AFrame[18]) * 256 + ord(AFrame[19]);
  if not (AVessel.ves_longinterval = i) then
   if LReg.ValueExists('interval_long') then
   begin
     if not (i = LReg.ReadInteger('interval_long'))
      then WriteIntValue('interval_long', i);
   end
   else WriteIntValue('interval_long', i);

// Short Interval
  i := ord(AFrame[20]) * 256 + ord(AFrame[21]);
  if not (AVessel.ves_shortinterval = i) then
   if LReg.ValueExists('interval_short') then
   begin
     if not (i = LReg.ReadInteger('interval_short'))
      then WriteIntValue('interval_short', i);
   end
   else WriteIntValue('interval_short', i);

// Min Resistance
  i := ord(AFrame[22]) * 256 + ord(AFrame[23]);
  if not (round(AVessel.ves_minresistance * 10) = i) then
   if LReg.ValueExists('resistance_min') then
   begin
     if not (i = round(LReg.ReadFloat('resistance_min') * 10))
      then WriteFloatValue('resistance_min', i / 10);
   end
   else WriteFloatValue('resistance_min', i / 10);

// Max Resistance
  i := ord(AFrame[24]) * 256 + ord(AFrame[25]);
  if not (round(AVessel.ves_maxresistance * 10) = i) then
   if LReg.ValueExists('resistance_max') then
   begin
     if not (i = round(LReg.ReadFloat('resistance_max') * 10))
      then WriteFloatValue('resistance_max', i / 10);
   end
   else WriteFloatValue('resistance_max', i / 10);

// Heattime
  i :=  ord(AFrame[26]) * 256 + ord(AFrame[27]);
  if not (AVessel.ves_heattime = i) then
   if LReg.ValueExists('heattime') then
   begin
     if not (i = LReg.ReadInteger('heattime'))
      then WriteIntValue('heattime', i);
   end
   else WriteIntValue('heattime', i);

// ADC Loops
  i := ord(AFrame[28]);
  if not (AVessel.ves_adcloops = i) then
   if LReg.ValueExists('adc_cycles') then
   begin
     if not (i = LReg.ReadInteger('adc_cycles'))
      then WriteIntValue('adc_cycles', i);
   end
   else WriteIntValue('adc_cycles', i);

// Fill Timeout
  i := ord(AFrame[29]);
  if not (AVessel.ves_filltimeout = i) then
   if LReg.ValueExists('fill_timeout') then
   begin
     if not (i = LReg.ReadInteger('fill_timeout'))
      then WriteIntValue('fill_timeout', i);
   end
   else WriteIntValue('fill_timeout', i);

// Span
  i := ord(AFrame[30]) * 256 + ord(AFrame[31]);
  if not (AVessel.ves_span = i) then
   if LReg.ValueExists('span') then
   begin
     if not (i = LReg.ReadInteger('span'))
      then WriteIntValue('span', i);
   end
   else WriteIntValue('span', i);

// Zero
  i := (ord(AFrame[32]) * 256 + ord(AFrame[33])) - ZERO_OFFSET;
  if not (AVessel.ves_zero = i) then
   if LReg.ValueExists('zero') then
   begin
     if not (i = LReg.ReadInteger('zero'))
      then WriteIntValue('zero', i);
   end
   else WriteIntValue('zero', i);

  LReg.CloseKey;
  if not (f1 or f2) then LReg.DeleteKey('Software\HZB_Helium\New_VesOptions\' + LName + '\');

  LReg.Free;
  Result := f1;
end;

function TCoordinatorThread.ChangedModuleOptions(const AFrame: string; AModule: TModule): boolean;
var LReg:     TRegistry;
    LAddress: string;
    f1,f2:    boolean;
    i:        integer;

    procedure WriteIntValue(AName: string; AValue: integer);
    begin
      LReg.WriteInteger(AName, AValue);
      f1 := true;
    end;

    procedure WriteFloatValue(AName: string; AValue: double);
    begin
      LReg.WriteFloat(AName, AValue);
      f1 := true;
    end;

begin
  f1           := false;
  LReg         := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  LAddress     := AddressToHexString(ReadAddress(AFrame));
  f2           := LReg.KeyExists('Software\HZB_Helium\New_ModOptions\' + LAddress + '\');
  LReg.OpenKey('Software\HZB_Helium\New_ModOptions\' + LAddress + '\', true);

  // Min Voltage
  i := ord(AFrame[34]) * 256 + ord(AFrame[35]);
  if not (round(AModule.mod_minvoltage * 10) = i) then
   if LReg.ValueExists('voltage_min') then
   begin
     if not (i = round(LReg.ReadFloat('voltage_min') * 10))
      then WriteFloatValue('voltage_min', i / 10);
   end
   else WriteFloatValue('voltage_min', i / 10);

  // Max Voltage
  i := ord(AFrame[36]) * 256 + ord(AFrame[37]);
  if not (round(AModule.mod_maxvoltage * 10) = i) then
   if LReg.ValueExists('voltage_max') then
   begin
     if not (i = round(LReg.ReadFloat('voltage_max') * 10))
      then WriteFloatValue('voltage_max', i / 10);
   end
   else WriteFloatValue('voltage_max', i / 10);

  // Critical Voltage
  i := ord(AFrame[38]);
  if not (AModule.mod_criticvoltage = i) then
   if LReg.ValueExists('voltage_alarm') then
   begin
     if not (i = LReg.ReadInteger('voltage_alarm'))
       then WriteIntValue('voltage_alarm', i);
   end
   else WriteIntValue('voltage_alarm', i);

  LReg.CloseKey;
  if not (f1 or f2) then LReg.DeleteKey('Software\HZB_Helium\New_ModOptions\' + LAddress + '\');

  LReg.Free;
  Result := f1;
end;


procedure TCoordinatorThread.MeasurementToReg(const ACode: char; AVessel: TVessel; AModule: TModule; const ALevel: double; const APressure: word; const AAccu: byte);
var LReg:          TRegistry;
    LMeaStatus, i: integer;
    LPosition:     string;
begin
  LReg         := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  LMeaStatus   := 1;
  LPosition    := '';

  try
    i := 1;
    LReg.OpenKey('Software\HZB_Helium\Measurements',true);
    while LReg.KeyExists('#' + intToStr(i)) do inc(i);
    LReg.OpenKey('#' + intToStr(i),true);

    LReg.WriteInteger('ves_id',   AVessel.ves_id);
    LReg.WriteInteger('ves_name', AVessel.ves_name);
    LReg.WriteInteger('mod_id',   AModule.mod_id);
    LReg.WriteString( 'mod_addr', AModule.mod_address);
    LReg.WriteInteger('coo_id',   FCooId);
    LReg.WriteFloat( 'he_level',  ALevel);
    LReg.WriteInteger('pressure', APressure);
    LReg.WriteInteger('battery',  AAccu);
    LReg.WriteString('date',      DateTimeToStr(Now));

    if not (AModule.mod_status in [0,1]) then
    case ACode of
     #3 : LMeaStatus := 2;
     #6 : begin
            if (AModule.mod_status = 3)
             then LMeaStatus := 3
             else LMeaStatus := 4;
            LPosition := AVessel.ves_position;
          end;
     #7 : begin
            LMeaStatus := 5;
            LPosition  := AVessel.ves_position;
          end;
     #8 : LMeaStatus := 6;
     #11: LMeaStatus := 7;
    end;
    LReg.WriteString( 'position', LPosition);
    LReg.WriteInteger('status',   LMeaStatus);
  finally
    LReg.CloseKey;
    LReg.Free;
  end;
end;

procedure TCoordinatorThread.CryoMeasurementToReg(const ADevId: integer;
  const AHeLevel, AN2Level1, AN2Level2: double);
var LReg: TRegistry;
    i:    integer;
begin
  LReg         := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;

  try
    i := 1;
    LReg.OpenKey('Software\HZB_Helium\CryoMeasurements',true);
    while LReg.KeyExists('#' + intToStr(i)) do inc(i);
    LReg.OpenKey('#' + intToStr(i),true);

    LReg.WriteInteger('dev_id',    ADevId);
    LReg.WriteInteger('coo_id',    FCooId);
    LReg.WriteFloat(  'he_level',  AHeLevel);
    LReg.WriteFloat(  'n2_level1', AN2Level1);
    LReg.WriteFloat(  'n2_level2', AN2Level2);
    LReg.WriteString( 'date',      DateTimeToStr(Now));
  finally
    LReg.CloseKey;
    LReg.Free;
  end;
end;


procedure TCoordinatorThread.CheckRequestsAndStatusByte(AModule: TModule; const AAddress: string);
var i: Byte;

  procedure SetStatusFB;
  begin
    if not (AModule.mod_status = 4) then AModule.mod_status := 3;
  end;

begin
  with AModule do
  begin

  // Status Byte
    if not (mod_statusbyte = #0) then
    begin                                                 // ESCAPED MESSAGE
      i := ord(mod_statusbyte);                           //-----------------
      if ((i and 4)  = 4)  then SetStatusFB;              // Filling Start
      if ((i and 8)  = 8)  then mod_status     := 2;      // Filling End
      if ((i and 16) = 16) then mod_getOptions := true;   // Changed Options
      if ((i and 32) = 32) then mod_getStatus  := true;   // Status Msg

      mod_statusbyte := #0;
      CreateMainFormJob(2, mod_name + ' sent status byte with the value: ' + copy(intToBin(i), 25, 8) + '.');
    end;

  // Send Requests
    if mod_getStatus    then TXRequest(#11 + #0, AAddress);

    if mod_setOptions   then
    begin
      TXRequest(#13 + #0 + VesOptions(mod_vessel) + ModOptions(AModule), AAddress);
      mod_getOptions := true;
      mod_setOptions := false;
    end;
    if mod_getOptions   then TXRequest(#12 + #0, AAddress);

    if mod_setPositions then
    begin
      TXRequest(#15 + #0 + GetPositions, AAddress);
      mod_getPositions := true;
      mod_setPositions := false;
    end;
    if mod_getPositions then TXRequest(#14 + #0, AAddress);

    if mod_setPassword  then
    begin
      TXRequest(#17 + #0 + GetPassword, AAddress);
      mod_getPassword := true;
      mod_setPassword := false;
    end;
    if mod_getPassword  then TXRequest(#16 + #0, AAddress);

    if mod_setAwakeTime then
    begin
      TXRequest(#22 + #0 + GetAwakeTime, AAddress);
      mod_getAwakeTime := true;
      mod_setAwakeTime := false;
    end;
    if mod_getAwakeTime then TXRequest(#21 + #0, AAddress);

    if mod_setSleepTime then
    begin
      TXRequest(#19 + #0 + GetSleepTime, AAddress);
      mod_getSleepTime := true;
      mod_setSleepTime := false;
    end;
    if mod_getSleepTime then TXRequest(#18 + #0, AAddress);

    if not (mod_displayText = '') then
    begin
      TXRequest(#20 + #0 + mod_displayText, AAddress);
      mod_displayText := '';
    end;
  end;
end;


function TCoordinatorThread.PositionListIsOk(const AFrame: string): boolean;
begin
  Result := (GetPositions = copy(AFrame,18,length(AFrame)-19));
end;

function TCoordinatorThread.PasswordIsOk(const APassword: word): boolean;
begin
  Result := (getRegInteger('','OptionsPW') = APassword);
end;

function TCoordinatorThread.AwakeTimeIsOk(const AAwakeTime: byte): boolean;
begin
  Result := (getRegInteger('','AwakeTime') = AAwakeTime);
end;

function TCoordinatorThread.SleepTimeIsOk(const ASleepTime: byte): boolean;
begin
  Result := (getRegInteger('','SleepTime') = ASleepTime);
end;



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//   << T V i r t C o m P o r t >>   - - - - - - - - - - - - - - - - - - - - - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


constructor TVirtualComPort.CreateWithProperties(const APort: string);
begin
  FComPort := createComPort(APort);
  if Assigned(FComPort) then
    if OpenComPort(FComPort) then
      FComPort.OnRxChar := SerialDataRxChar;
end;

destructor TVirtualComPort.Destroy;
begin
  FComPort.Close;
  FreeAndNil(FComPort);
  inherited Destroy;
end;

procedure TVirtualComPort.SerialDataRxChar(ASender: TObject; Count: integer);
var LRxBuffer:  string;
begin
  try
    LRxBuffer := '';
    FComPort.ReadStr(LRxBuffer, Count);
    DataReceived(LRxBuffer);
  finally LRxBuffer := '';
  end;
end;


function TVirtualComPort.GetPort: string;
begin
  Result := '';
  if Assigned(FComPort) then Result := FComPort.Port;
end;

function TVirtualComPort.IsOpened: boolean;
begin
  Result := false;
  if Assigned(FComPort) then
  begin
    Result := FComPort.Connected;
    if not Result then Result := Reopen;
  end;
end;

function TVirtualComPort.Reopen: boolean;
begin
  Result := false;
  if Assigned(FComPort) then
  try
    FComPort.Close;
    Result := OpenComPort(FComPort);
  except  
  end;
end;

procedure TVirtualComPort.Close;
begin
  FComPort.Close;
end;

procedure TVirtualComPort.WriteData(const AString: string);
begin
  if IsOpened then
  try FComPort.WriteStr(AString);
  except
  end;
end;

procedure TVirtualComPort.DataReceived(const AData: string);
begin
  if Assigned(OnDataRx) then OnDataRx(AData);
end;

end.

