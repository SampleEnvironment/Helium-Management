unit HeliumDataMgmt;

interface

uses
  SysUtils, Classes, Contnrs, Math, SyncObjs, EvaluateDb, IsaacStandardInterfaces,
  SQLClientInterfaces;

type
  TLogErrorProc = procedure(const AMsg: string) of object;
  
  TVessel      = class(TObject)
    ves_id:            integer;
    ves_type_id:       integer;
    ves_name:          word;
    ves_type:          string;
    ves_volume:        double;
    ves_tare:          double;
    ves_comment1:      string;
    ves_at_hzb_since:  string;
   { - - - - - - - - - - - - - - }
    ves_shortinterval: integer;
    ves_longinterval:  integer;
    ves_minresistance: double;
    ves_maxresistance: double;
    ves_heattime:      integer;
    ves_adcloops:      integer;
    ves_filltimeout:   integer;
    ves_span:          integer;
    ves_zero:          integer;
    ves_comment2:      string;
    ves_options_in_db: boolean;
   { - - - - - - - - - - - - - - }
    ves_he_level:      double;
    ves_pressure:      word;
    ves_position:      string;
  end;

  TCoordinator = class(TObject)
    coo_id:         integer;
    coo_name:       string;
    coo_comport:    string;
    coo_ip:         string;
    coo_address:    string;
    coo_position:   string;
    coo_comment:    string;
   { - - - - - - - - - - - - - - }
    coo_status:     byte;           // 0 inactive    1 active    2 failure
    coo_xbeestatus: byte;           // 0 reachable   1 testing   2 failure
  end;

  TModule      = class(TObject)
    mod_id:            integer;
    mod_name:          string;
    mod_address:       string;
    mod_lastactive:    string;
    mod_minvoltage:    double;
    mod_maxvoltage:    double;
    mod_criticvoltage: byte;
    mod_comment:       string;
   { - - - - - - - - - - - - - - }
    mod_status:        byte;
    mod_vessel:        TVessel;
    mod_coordinator:   TCoordinator;
    mod_accumulator:   byte;
    mod_position:      string;
    mod_statusbyte:    char;
   { - - - - - - - - - - - - - - }
    mod_getStatus:     boolean;
    mod_getOptions:    boolean;
    mod_setOptions:    boolean;
    mod_getPositions:  boolean;
    mod_setPositions:  boolean;
    mod_getPassword:   boolean;
    mod_setPassword:   boolean;
    mod_getSleepTime:  boolean;
    mod_setSleepTime:  boolean;
    mod_getAwakeTime:  boolean;
    mod_setAwakeTime:  boolean;
    mod_displayText:   string;
  end;

  TCryostat = class(TObject)
    dev_id:          integer;
    dev_name:        string;
    dev_address:     string;
    dev_comment:     string;
    dev_ch1_enabled: boolean;
    dev_ch1_gas:     string;
    dev_ch1_span:    double;
    dev_ch1_zero:    double;
    dev_ch2_enabled: boolean;
    dev_ch2_gas:     string;
    dev_ch2_span:    double;
    dev_ch2_zero:    double;
    dev_ch3_enabled: boolean;
    dev_ch3_gas:     string;
    dev_ch3_span:    double;
    dev_ch3_zero:    double;
    dev_class   :    integer;
   { - - - - - - - - - - - - - - }
    dev_lastactive:  string;
    dev_ch1_level:   double;
    dev_ch2_level:   double;
    dev_ch3_level:   double;
   { - - - - - - - - - - - - - - }
    dev_coordinator: TCoordinator;
  end;

  TDataMgmt    = class(TObject)
  private
    FLogErrorProc: TLogErrorProc;
    FLock:      TCriticalSection;
    FDbClient:   ISQLClient;
    FEvaluate:  TEvaluateDbThread;

    FVesList:   TObjectList;
    FModList:   TObjectList;
    FCooList:   TObjectList;
    FDevList:   TObjectList;
    procedure SQLError(const AObj: ISQLImplementor; const AMessage: string);
  public
    constructor Create(const ALogErrorProc: TLogErrorProc); reintroduce;
    destructor  Destroy; override;
    function    Init: boolean;
    
    procedure   LockWhileWorkingWithList;
    procedure   UnlockAfterWorkingWithList;

    procedure   ConfigureDbConnection;
    function    GetDbConnection: boolean;
    function    GetDbClient: ISQLClient;
    procedure   DisconnectDbConnection;

    procedure   LastActiveToDb(AId: integer);
    function    MeasurementToDb(ACode: char; AVessel: TVessel; AModule: TModule; ACooId: integer; ADate: string; ALevel: double; APressure: word; AAccu: byte): boolean; overload;
    function    MeasurementToDb(AVesId, AModId, ACooId: integer; APosition: string; ALevel: double; APressure: word; AAccu: byte; ADate: string; AStatus: integer): boolean; overload;

    function    CryoMeasurementToDb(const ADevId, ACooId: integer;
                                    const ACH1Level, ACh2Level, ACh3Level: double;
                                    const ADate: string): boolean;


    function    InsertVesOptionsToDb(AVessel: TVessel): boolean;
    function    UpdateVesOptionsToDb(AVessel: TVessel): boolean;
    function    DeleteVesOptionsToDb(const AId: integer): boolean;

    function    InsertModuleToDb(AModule: TModule): boolean;
    function    UpdateModuleToDb(AModule: TModule): boolean;
    function    DeleteModuleToDb(const AId: integer): boolean;

    function    InsertCoordinatorToDb(ACoo: TCoordinator): boolean;
    function    UpdateCoordinatorToDb(ACoo: TCoordinator): boolean;
    function    DeleteCoordinatorToDb(const AId: integer): boolean;

    function    InsertIlmDeviceToDb(ADev: TCryostat): boolean;
    function    UpdateIlmDeviceToDb(ADev: TCryostat): boolean;
    function    DeleteIlmDeviceToDb(const AId: integer): boolean;

    function    SqlCommand(const AQuery: string): boolean;

    function    RefreshVesListByDB: boolean;
    function    RefreshModListByDB: boolean;
    function    RefreshCooListByDB: boolean;
    function    RefreshDevListByDB: boolean;

    function    GetVesItem(const AId: integer): TVessel;
    function    GetModItem(const AId: integer): TModule;
    function    GetCooItem(const AId: integer): TCoordinator;
    function    GetDevItem(const AId: integer): TCryostat;

    function    GetVesIndex(const AId: integer): integer;
    function    GetModIndex(const AId: integer): integer;
    function    GetCooIndex(const AId: integer): integer;
    function    GetDevIndex(const AId: integer): integer;

    function    GetVesItemByName(const AName: word): TVessel;
    function    GetModItemByAddr(const AAddr: string): TModule;
    function    GetDevItemByAddr(const AAddr: string): TCryostat;

    function    GetVesIndexByName(const AName: word): integer;
    function    GetModIndexByAddr(const AAddr: string): integer;
    function    GetDevIndexByAddr(const AAddr: string): integer;

    function    GetCopyOfVessel(const AIndex: integer): TVessel;
    function    GetCopyOfModule(const AIndex: integer): TModule;
    function    GetCopyOfCoordinator(const AIndex: integer): TCoordinator;
    function    GetCopyOfIlmDevice(const AIndex: integer): TCryostat;

    function    InsertNewCryoClass(const AClassName: string): integer;
    function    GetRegisteredCryoClasses: TStringList;
    procedure   EvaluateDatabase;
    procedure   FreeEvaluateThread(Sender: TObject);
  published
    property    VesselList:      TObjectList read FVesList;
    property    ModuleList:      TObjectList read FModList;
    property    CoordinatorList: TObjectList read FCooList;
    property    DeviceList:      TObjectList read FDevList;
  end;

  function BoolToNumber(const ABool: boolean): Char;
  function NumberToBool(const ANumber: byte): boolean;

  function GetModStatus(const AStatus: byte): string;
  function GetCooStatus(const AStatus: byte): string;

  function NewSQLClient: ISQLClient;

  function GetSQLClientFactories: IDataModule; stdcall; external 'SQLClients.dll' name 'GetSQLClientFactories';

implementation

uses
  HeliumFunctions, Dialogs;

const EClassConvertError = 'Error converting %s into DB_Key.';
const EKeyConvertError = 'Error converting %d into ClassName.';

function BoolToNumber(const ABool: boolean): Char;
begin
  if ABool then
    Result := '1'
  else
    Result := '0';
end;

function NumberToBool(const ANumber: byte): boolean;
begin
  Result := not (ANumber = 0);  
end;

function GetModStatus(const AStatus: byte): string;
begin
  Result := '';
  case AStatus of
   0 : Result := 'inactive';
   1 : Result := 'connecting';
   2 : Result := 'connected';
   3 : Result := 'fill begin';
   4 : Result := 'filling';
   5 : Result := 'timeout';
  end;
end;

function GetCooStatus(const AStatus: byte): string;
begin
  Result := '';
  case AStatus of
   0 : Result := 'inactive';
   1 : Result := 'connected';
   2 : Result := 'not connected';
  end;
end;

function NewSQLClient: ISQLClient;
var
  lRegId, lRegGUID: string;
  lFactories: IDataModule;
  lIntf: IInterface;
  lFactory: ISQLImplementorFactory;
begin
  Result := nil;

  lFactories := GetSQLClientFactories;
  if Assigned(lFactories) then
    try
      lRegGUID := getRegString('', 'SQLClientClassId');
      lRegId   := getRegString('', 'SQLClientId');
      if lRegGUID <> '' then
      begin
        lFactories.GetInterfaceValue(PChar(lRegGUID), lIntf);
        if not(supports(lIntf, ISQLImplementorFactory, lFactory)) then
          raise Exception.Create(Format('ClassFactory for database ''%s'' could not be initialized', [lRegId]));
      end
      else
        raise Exception.Create(Format('SQLClientFactory %s could not be initialized, no DataBase type specified', [lRegId]));
      Result := lFactory.CreateClient;
    finally
      lFactories := nil;
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//   << T D a t a - M a n a g e m e n t >>   - - - - - - - - - - - - - - - - - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TDataMgmt.Create(const ALogErrorProc: TLogErrorProc);
begin
  inherited Create;
  FLogErrorProc := ALogErrorProc;
  FLock      := TCriticalSection.Create;
  FVesList   := TObjectList.Create(true);
  FModList   := TObjectList.Create(true);
  FCooList   := TObjectList.Create(true);
  FDevList   := TObjectList.Create(true);
  FDbClient   := nil;
  FEvaluate  := nil;
end;

destructor TDataMgmt.Destroy;
begin
  FLock.Acquire;
  try
    FVesList.Free;
    FModList.Free;
    FCooList.Free;
    FDevList.Free;

    FDbClient := nil;
  finally
    FLock.Release;
    FLock.Free;
  end;

  inherited Destroy;
end;


procedure TDataMgmt.LockWhileWorkingWithList;
begin
  FLock.Acquire;
end;

procedure TDataMgmt.UnlockAfterWorkingWithList;
begin
  FLock.Release;
end;


procedure TDataMgmt.ConfigureDbConnection;
begin
  if Assigned(FDbClient) then FDbClient := nil;
  FDbClient := NewSQLClient;

  if not Assigned(FDbClient) then
    raise Exception.Create('Failure configuring Db, SQLClient not assigned');
  if not(Assigned(FDbClient.GetSession)) then
    raise Exception.Create('Failure configuring Db, session not assigned');

  FDbClient.GetSession.InitSession(getRegString('', 'DbServer'),
                                   getRegString('', 'DbUser'),
                                   decrypt(getRegString('', 'DbKey')));
end;

function TDataMgmt.GetDbClient: ISQLClient;
begin
  result := FDbClient;
end;

function TDataMgmt.GetDbConnection: boolean;
var
  lSession: ISQLSession;
begin
  lSession := FDbClient.GetSession;
  Result := lSession.Connected;
  if not(Result) then
    if not ((lSession.GetDataSource = '')   or
            (lSession.GetUsername = '') or
            (lSession.GetPassword = '')) then
      Result := lSession.Connect;
  if not(Result) then
    SQLError(lSession, Format(EConnectSession, [lSession.GetDataSource, lSession.GetUsername]));
end;

procedure TDataMgmt.DisconnectDbConnection;
begin
  if assigned(FDbClient) then
    FDbClient.GetSession.Disconnect;
end;


procedure TDataMgmt.LastActiveToDb(AId: integer);
begin
  if (AId > -1) and GetDbConnection then
  begin
    setDbQuery(FDbClient, 'UPDATE ' + db_t_module + ' SET '
                          + 'MOD_LASTTIMEACTIVE = SYSDATE WHERE '
                          + 'MOD_ID = ' + intToStr(AId));
    if not(FDbClient.Submit) then
      SQLError(FDbClient, ESubmitSQL);
  end;
end;

function TDataMgmt.MeasurementToDb(ACode: char; AVessel: TVessel; AModule: TModule; ACooId: integer; ADate: string; ALevel: double; APressure: word; AAccu: byte): boolean;
var LMeaStatus: integer;
    LPosition:  string;
begin
  Result     := false;
  LMeaStatus := 1;
  LPosition  := '';

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

  if GetDbConnection then
  try
    setDbQuery(FDbClient, 'INSERT INTO ' + db_t_measurement + ' VALUES (1, '
                         + intToStr(AVessel.ves_id) + ', '
                         + intToStr(AModule.mod_id) + ', '
                         + intToStr(ACooId)         + ', '
                         + #39 + LPosition + #39    + ', '
                         + floatToDbFloat(ALevel)   + ', '
                         + intToStr(APressure)      + ', '
                         + intToStr(AAccu)          + ', '
                         + 'to_date(' + #39 + ADate + #39
                         + ',' + #39 + 'dd.mm.yyyy hh24:mi:ss' + #39 + '), '
                         + intToStr(LMeaStatus)     + ')');
    if not(FDbClient.Submit) then
      SQLError(FDbClient, ESubmitSQL);
    Result := true;
  finally
  end;
end;

function TDataMgmt.MeasurementToDb(AVesId, AModId, ACooId: integer; APosition: string; ALevel: double; APressure: word; AAccu: byte; ADate: string; AStatus: integer): boolean;
begin
  Result := false;
  if GetDbConnection then
  try
    setDbQuery(FDbClient, 'INSERT INTO ' + db_t_measurement + ' VALUES (1, '
                         + intToStr(AVesId)         + ', '
                         + intToStr(AModId)         + ', '
                         + intToStr(ACooId)         + ', '
                         + #39 + APosition + #39    + ', '
                         + floatToDbFloat(ALevel)   + ', '
                         + intToStr(APressure)      + ', '
                         + intToStr(AAccu)          + ', '
                         + 'to_date(' + #39 + ADate + #39
                         + ',' + #39 + 'dd.mm.yyyy hh24:mi:ss' + #39 + '), '
                         + intToStr(AStatus)        + ')');
    if not(FDbClient.Submit) then
      SQLError(FDbClient, ESubmitSQL);
    Result := true;
  finally
  end;
end;        




function TDataMgmt.CryoMeasurementToDb(const ADevId, ACooId: Integer;
                                       const ACH1Level, ACh2Level, ACh3Level: Double;
                                       const ADate: string): boolean;

  function GetLevel(const ALevel: double): string;
  begin
    Result := 'null';
    if not (ALevel > 200) then
      Result := floatToDbFloat(ALevel);
  end;

begin
  Result := false;

  if GetDbConnection then
  try
    setDbQuery(FDbClient, 'INSERT INTO ' + db_t_ilmmeasurement + ' VALUES (1, '
                         + intToStr(ADevId)    + ', '
                         + intToStr(ACooId)    + ', '
                         + GetLevel(ACh1Level) + ', '
                         + GetLevel(ACh2Level) + ', '
                         + GetLevel(ACh3Level) + ', '
                         + 'to_date(' + #39 + ADate + #39
                         + ',' + #39 + 'dd.mm.yyyy hh24:mi:ss' + #39 + '))');
    if not(FDbClient.Submit) then
      SQLError(FDbClient, ESubmitSQL);
    Result := true;
  finally
  end;
end;


function TDataMgmt.InsertVesOptionsToDb(AVessel: TVessel): boolean;
begin
  Result := false;

  if GetDbConnection then
  try
    setDbQuery(FDbClient, 'INSERT INTO ' + db_t_hlm_vessel + ' VALUES ('
               + intToStr(AVessel.ves_id)                  + ', '
               + intToStr(AVessel.ves_shortinterval)       + ', '
               + intToStr(AVessel.ves_longinterval)        + ', '
               + floatToDbFloat(AVessel.ves_minresistance) + ', '
               + floatToDbFloat(AVessel.ves_maxresistance) + ', '
               + intToStr(AVessel.ves_heattime)            + ', '
               + intToStr(AVessel.ves_adcloops)            + ', '
               + intToStr(AVessel.ves_filltimeout)         + ', '
               + intToStr(AVessel.ves_span)                + ', '
               + intToStr(AVessel.ves_zero)                + ', '
               + #39 + AVessel.ves_comment2 + #39          + ')');
    if not(FDbClient.Submit) then
      SQLError(FDbClient, ESubmitSQL);
    Result := true;
  finally
  end;
end;

function TDataMgmt.UpdateVesOptionsToDb(AVessel: TVessel): boolean;
begin
  Result := false;

  if GetDbConnection then
  try
    setDbQuery(FDbClient, 'UPDATE ' + db_t_hlm_vessel + ' SET '
                       + 'VES_SHORTINTERVAL = ' + intToStr(AVessel.ves_shortinterval)       + ', '
                       + 'VES_LONGINTERVAL = '  + intToStr(AVessel.ves_longinterval)        + ', '
                       + 'VES_MINRESISTANCE = ' + floatToDbFloat(AVessel.ves_minresistance) + ', '
                       + 'VES_MAXRESISTANCE = ' + floatToDbFloat(AVessel.ves_maxresistance) + ', '
                       + 'VES_HEATTIME = '      + intToStr(AVessel.ves_heattime)            + ', '
                       + 'VES_ADCLOOP = '       + intToStr(AVessel.ves_adcloops)            + ', '
                       + 'VES_FILLTIMEOUT = '   + intToStr(AVessel.ves_filltimeout)         + ', '
                       + 'VES_SPAN = '          + intToStr(AVessel.ves_span)                + ', '
                       + 'VES_ZERO = '          + intToStr(AVessel.ves_zero)                + ', '
                       + 'VES_COMMENT = '       + #39 + AVessel.ves_comment2 + #39
                       + ' WHERE VES_V_ID = '   + intToStr(AVessel.ves_id));
    if not(FDbClient.Submit) then
      SQLError(FDbClient, ESubmitSQL);
    Result := true;
  finally
  end;
end;

function TDataMgmt.DeleteVesOptionsToDb(const AId: Integer): boolean;
begin
  Result := false;
  if (AId > 0) then
   if GetDbConnection then
   try
     setDbQuery(FDbClient, 'DELETE FROM ' + db_t_hlm_vessel
                        + ' WHERE VES_V_ID = ' + intToStr(AId));
      if not(FDbClient.Submit) then
        SQLError(FDbClient, ESubmitSQL);
     Result := true;
   finally
   end;
end;


function TDataMgmt.InsertModuleToDb(AModule: TModule): boolean;
begin
  Result := false;

  if GetDbConnection then
  try
    setDbQuery(FDbClient, 'INSERT INTO ' + db_t_module + ' VALUES ('
                         + '((SELECT MAX(MOD_ID) FROM ' + db_t_module + ') + 1),'
                         + #39 + AModule.mod_name            + #39 + ', '
                         + #39 + AModule.mod_address         + #39 + ', '
                         + #39                               + #39 + ', '
                         + floatToDbFloat(AModule.mod_minvoltage)  + ', '
                         + floatToDbFloat(AModule.mod_maxvoltage)  + ', '
                         + intToStr(AModule.mod_criticvoltage)     + ', '
                         + #39 + AModule.mod_comment         + #39 + ')');
    if not(FDbClient.Submit) then
      SQLError(FDbClient, ESubmitSQL);
    Result := true;
  finally
  end;
end;

function TDataMgmt.UpdateModuleToDb(AModule: TModule): boolean;
begin
 Result := false;

  if GetDbConnection then
  try
    setDbQuery(FDbClient, 'UPDATE ' + db_t_module + ' SET '
                       + 'MOD_NAME = '            + #39 + AModule.mod_name            + #39 + ', '
                       + 'MOD_ADDRESS = '         + #39 + AModule.mod_address         + #39 + ', '
                       + 'MOD_MINVOLTAGE = '      + floatToDbFloat(AModule.mod_minvoltage)  + ', '
                       + 'MOD_MAXVOLTAGE = '      + floatToDbFloat(AModule.mod_maxvoltage)  + ', '
                       + 'MOD_CRITICALVOLTAGE = ' + intToStr(AModule.mod_criticvoltage)     + ', '
                       + 'MOD_COMMENT = '         + #39 + AModule.mod_comment + #39
                       + ' WHERE MOD_ID = ' + intToStr(AModule.mod_id));
    if not(FDbClient.Submit) then
      SQLError(FDbClient, ESubmitSQL);
    Result := true;
  finally
  end;
end;

function TDataMgmt.DeleteModuleToDb(const AId: Integer): boolean;
begin
  Result := false;
  if (AId > 0) then
   if GetDbConnection then
   try
     setDbQuery(FDbClient, 'DELETE FROM ' + db_t_module
                        + ' WHERE MOD_ID = ' + intToStr(AId));
      if not(FDbClient.Submit) then
        SQLError(FDbClient, ESubmitSQL);
     Result := true;
   finally
   end;
end;

    
function TDataMgmt.Init: Boolean;
begin
  Result := True;
  try
    ConfigureDbConnection;
    if not(GetDbConnection) then
      raise Exception.Create(Format('Failure connecting to database (data source: %s, user: %s)', [FDbClient.GetSession.GetDataSource, FDbClient.GetSession.GetUsername]));
  except
    on E: Exception do
    begin
      MessageDlg(Format('Failure initializing DataMgmt, message: %s', [E.Message]),
                 mtError, [mbOk], 0);
      Result := False;
    end;
  end;
end;

function TDataMgmt.InsertCoordinatorToDb(ACoo: TCoordinator): boolean;
begin
  Result := false;

  if GetDbConnection then
  try
    setDbQuery(FDbClient, 'INSERT INTO ' + db_t_coordinator + ' VALUES ('
               + '((SELECT MAX(COO_ID) FROM ' + db_t_coordinator + ') + 1),'
               + #39 + ACoo.coo_name     + #39 + ', '
               + #39 + ACoo.coo_comport  + #39 + ', '
               + #39 + ACoo.coo_ip       + #39 + ', '
               + #39 + ACoo.coo_address  + #39 + ', '
               + #39 + ACoo.coo_position + #39 + ', '
               + #39 + ACoo.coo_comment  + #39 + ')');
    if not(FDbClient.Submit) then
      SQLError(FDbClient, ESubmitSQL);
    Result := true;
  finally
  end;
end;

function TDataMgmt.UpdateCoordinatorToDb(ACoo: TCoordinator): boolean;
begin
  Result := false;

  if GetDbConnection then
  try
    setDbQuery(FDbClient, 'UPDATE ' + db_t_coordinator + ' SET '
                       + 'COO_NAME = '     + #39 + ACoo.coo_name     + #39 + ', '
                       + 'COO_COMPORT = '  + #39 + ACoo.coo_comport  + #39 + ', '
                       + 'COO_IP = '       + #39 + ACoo.coo_ip       + #39 + ', '
                       + 'COO_ADDRESS = '  + #39 + ACoo.coo_address  + #39 + ', '
                       + 'COO_POSITION = ' + #39 + ACoo.coo_position + #39 + ', '
                       + 'COO_COMMENT = '  + #39 + ACoo.coo_comment  + #39
                       + ' WHERE COO_ID = ' + intToStr(ACoo.coo_id));
    if not(FDbClient.Submit) then
      SQLError(FDbClient, ESubmitSQL);
    Result := true;
  finally
  end;
end;

function TDataMgmt.DeleteCoordinatorToDb(const AId: Integer): boolean;
begin
  Result := false;
  if (AId > 0) then
   if GetDbConnection then
   try
     setDbQuery(FDbClient, 'DELETE FROM ' + db_t_coordinator
                        + ' WHERE COO_ID = ' + intToStr(AId));
      if not(FDbClient.Submit) then
        SQLError(FDbClient, ESubmitSQL);
     Result := true;
   finally
   end;
end;                                  

function TDataMgmt.InsertNewCryoClass(const AClassName: string): integer;

  function GetClassID(AClassName: string): integer;
  var i: integer;
  begin
    SetDBQuery(FDbClient, Format('SELECT ILC_ID FROM HLM_ILMCLASS ' +
                                'WHERE ILC_NAME=%s', [AClassName]));

    if FDbClient.Open then
      try
        if FDbClient.Active then
          result := FDbClient.FieldByName('ILC_ID').AsInteger
        else
          raise Exception.Create(Format(EClassConvertError, [AClassName]));
      finally
        FDbClient.Close;
      end
    else
      SQLError(FDbClient, EOpenClient);
  end;

begin
  result := -1;
  if GetDbConnection then
  begin
    SetDbQuery(FDbClient,
               Format('insert into hlm_ilmclass(ilc_name) ' +
                      'values(''%s'')', [AClassName]));
    if not(FDbClient.Submit) then
      raise Exception(Format(ESubmitSQL, [FDbClient.GetErrorBuffer.Message]));
    result := GetClassID(AClassName);
  end;
end;

function TDataMgmt.InsertIlmDeviceToDb(ADev: TCryostat): boolean;

  function GetClassID(AClassName: string): integer;
  var i: integer;
  begin
    SetDBQuery(FDbClient, Format('SELECT ILC_ID FROM HLM_ILMCLASS ' +
                                'WHERE ILC_NAME=%s', [AClassName]));

    if FDbClient.Open then
      try
        if FDbClient.Active then
          result := FDbClient.FieldByName('ILC_ID').AsInteger
        else
          raise Exception.Create(Format(EClassConvertError, [AClassName]));
      finally
        FDbClient.Close;
      end
    else
      SQLError(FDbClient, EOpenClient);
  end;

begin
  Result := false;

  if GetDbConnection then
  try
    setDbQuery(FDbClient, 'INSERT INTO ' + db_t_device + ' VALUES ('
               + '((SELECT MAX(ILD_ID) FROM ' + db_t_device + ') + 1),'
               + #39 + ADev.dev_name     + #39      + ', '
               + #39 + ADev.dev_address  + #39      + ', '
               + #39 + ADev.dev_comment  + #39      + ', '
               + BoolToNumber(ADev.dev_ch1_enabled) + ', '
               + #39 + ADev.dev_ch1_gas + #39       + ', '
               + floatToDbFloat(ADev.dev_ch1_span)  + ', '
               + floatToDbFloat(ADev.dev_ch1_zero)  + ', '
               + BoolToNumber(ADev.dev_ch2_enabled) + ', '
               + #39 + ADev.dev_ch2_gas + #39       + ', '
               + floatToDbFloat(ADev.dev_ch2_span)  + ', '
               + floatToDbFloat(ADev.dev_ch2_zero)  + ', '
               + BoolToNumber(ADev.dev_ch3_enabled) + ', '
               + #39 + ADev.dev_ch3_gas + #39       + ', '
               + floatToDbFloat(ADev.dev_ch3_span)  + ', '
               + floatToDbFloat(ADev.dev_ch3_zero)  + ', '
               + IntToStr(ADev.dev_Class)+ ')');

    if not(FDbClient.Submit) then
      SQLError(FDbClient, ESubmitSQL);
    Result := true;
  finally
  end;
end;

function TDataMgmt.UpdateIlmDeviceToDb(ADev: TCryostat): boolean;
begin
  Result := false;

  if GetDbConnection then
  try
    setDbQuery(FDbClient, 'UPDATE ' + db_t_device + ' SET '
                       + 'ILD_NAME = '           + #39 + ADev.dev_name     + #39      + ', '
                       + 'ILD_ADDRESS = '        + #39 + ADev.dev_address  + #39      + ', '
                       + 'ILD_COMMENT = '        + #39 + ADev.dev_comment  + #39      + ', '
                       + 'ILD_CH1_ENABLED = '    + BoolToNumber(ADev.dev_ch1_enabled) + ', '
                       + 'ILD_CH1_SUBSTANCE = '  + #39 + ADev.dev_ch1_gas  + #39      + ', '
                       + 'ILD_CH1_SPAN = '       + floatToDbFloat(ADev.dev_ch1_span)  + ', '
                       + 'ILD_CH1_ZERO = '       + floatToDbFloat(ADev.dev_ch1_zero)  + ', '
                       + 'ILD_CH2_ENABLED = '    + BoolToNumber(ADev.dev_ch2_enabled) + ', '
                       + 'ILD_CH2_SUBSTANCE = '  + #39 + ADev.dev_ch2_gas  + #39      + ', '
                       + 'ILD_CH2_SPAN = '       + floatToDbFloat(ADev.dev_ch2_span)  + ', '
                       + 'ILD_CH2_ZERO = '       + floatToDbFloat(ADev.dev_ch2_zero)  + ', '
                       + 'ILD_CH3_ENABLED = '    + BoolToNumber(ADev.dev_ch3_enabled) + ', '
                       + 'ILD_CH3_SUBSTANCE = '  + #39 + ADev.dev_ch3_gas  + #39      + ', '
                       + 'ILD_CH3_SPAN = '       + floatToDbFloat(ADev.dev_ch3_span)  + ', '
                       + 'ILD_CH3_ZERO = '       + floatToDbFloat(ADev.dev_ch3_zero)  + ', '
                       + 'ILD_CLASS = '          + IntToStr(ADev.dev_class)
                       + ' WHERE ILD_ID = ' + intToStr(ADev.dev_id));
 
    if not(FDbClient.Submit) then
      SQLError(FDbClient, ESubmitSQL);
    Result := true;
  finally
  end;
end;

function TDataMgmt.DeleteIlmDeviceToDb(const AId: integer): boolean;
begin
  Result := false;
  if (AId > 0) then
   if GetDbConnection then
   try
     setDbQuery(FDbClient, 'DELETE FROM ' + db_t_device
                        + ' WHERE ILD_ID = ' + intToStr(AId));
      if not(FDbClient.Submit) then
        SQLError(FDbClient, ESubmitSQL);
     Result := true;
   finally
   end;
end;


function TDataMgmt.SqlCommand(const AQuery: string): boolean;
begin
  Result := false;

  if not (AQuery = '') then
   if GetDbConnection then
   try
     setDbQuery(FDbClient, AQuery);
      if not(FDbClient.Submit) then
        SQLError(FDbClient, ESubmitSQL);
     Result := true;
   finally
   end;
end;

procedure TDataMgmt.SQLError(const AObj: ISQLImplementor;
  const AMessage: string);
var
  lMsg: string;
  lErrBuffer: IException;
begin
  lErrBuffer := AObj.GetErrorBuffer;
  if Assigned(lErrBuffer) then
    try
      lMsg := Format('%s, message: %s', [AMessage, lErrBuffer.Message]);
    finally
      lErrBuffer := nil;
    end
  else
    lMsg := Format(AMessage, ['No error message']);

  if Assigned(FLogErrorProc) then
    FLogErrorProc(lMsg)
  else
    raise Exception.Create(lMsg);
end;

function TDataMgmt.GetRegisteredCryoClasses: TStringList;

  function GetClassName(AID: integer): string;
  begin
    SetDBQuery(FDbClient, Format('SELECT ILC_NAME FROM HLM_ILMCLASS ' +
                                'WHERE ILC_ID=%d', [AID]));

    if FDbClient.Open then
      try
        if FDbClient.Active then
          result := FDbClient.FieldByName('ILC_NAME').AsString
        else
          raise Exception.Create(Format(EKeyConvertError, [AID]));
      finally
        FDbClient.Close;
      end
    else
      SQLError(FDbClient, EOpenClient);
  end;

begin
  result := TStringList.Create;
  if GetDbConnection then
  begin
    SetDBQuery(FDbClient , 'SELECT * FROM HLM_ILMCLASS');
    try
      if FDbClient.Open then
      begin
        if FDbClient.Active then
          with FDbClient do
            while not Eof do
            begin
              result.AddObject(FieldByName('ILC_NAME').AsString, Pointer(FieldByName('ILC_ID').AsInteger));
              Next;
            end;
      end
      else
        SQLError(FDbClient, EOpenClient);

    finally
      FDbClient.Close;
    end;
  end;
end;

function TDataMgmt.RefreshVesListByDB: boolean;
var LVesList: TObjectList;
    LVessel:  TVessel;
    i:        integer;

  function CompareVes(Item1: Pointer; Item2: Pointer): integer;
  begin
    Result := CompareValue(TVessel(Item1).ves_name, TVessel(Item2).ves_name);
  end;

begin
  Result   := false;
  LVesList := TObjectList.Create(false);

  if GetDbConnection then
  try
    for i := 0 to FVesList.Count - 1 do LVesList.Add(FVesList[i]);

    setDbQuery(FDbClient, 'SELECT * FROM ' + db_t_type
                       + ' RIGHT OUTER JOIN ' + db_t_hen2_vessel
                       + ' ON V_T_ID = T_ID'
                       + ' LEFT OUTER JOIN ' + db_t_weighing
                       + ' ON W_V_ID = V_ID '
                       + ' LEFT OUTER JOIN ' + db_t_hlm_vessel
                       + ' ON VES_V_ID = V_ID'
                       + ' WHERE T_SUBSTANCE = ' + #39 + 'He' + #39
                       + ' AND W_ARRIVING_DATE ='
                       + ' (SELECT MAX(W_ARRIVING_DATE) FROM ' + db_t_weighing
                       + ' WHERE W_V_ID = V_ID)'
                       + ' ORDER BY UPPER(V_NAME)');
    if FDbClient.Open then
    begin
      Result := FDbClient.Active;

      if Result then
      with FDbClient do
        while not Eof do
        begin
          i := GetVesIndex(FieldByName('V_ID').AsInteger);

          if (i = -1) then
          begin
            LVessel := TVessel.Create;
            FVesList.Add(LVessel);

            LVessel.ves_id             := FieldByName('V_ID').AsInteger;
            LVessel.ves_he_level       := 255;
            LVessel.ves_pressure       := 0;
            LVessel.ves_position       := '';
          end
          else begin
            LVessel     := TVessel(FVesList[i]);
            LVesList[i] := nil;
          end;

          LVessel.ves_type_id  := FieldByName('T_ID').AsInteger;
          LVessel.ves_name     := FieldByName('V_NAME').AsInteger;
          LVessel.ves_type     := FieldByName('T_NAME').AsString;
          LVessel.ves_volume   := FieldByName('T_VOLUME').AsFloat;
          LVessel.ves_tare     := FieldByName('V_TARE').AsFloat;
          LVessel.ves_comment1 := FieldByName('V_COMMENT').AsString;

          if (FieldByName('W_LEAVING_DATE').AsString = '')
           then LVessel.ves_at_hzb_since := FieldByName('W_ARRIVING_DATE').AsString
           else LVessel.ves_at_hzb_since := '';

          LVessel.ves_options_in_db := FieldByName('VES_V_ID').AsInteger > 0;
          if LVessel.ves_options_in_db then
          begin
            LVessel.ves_shortinterval := FieldByName('VES_SHORTINTERVAL').AsInteger;
            LVessel.ves_longinterval  := FieldByName('VES_LONGINTERVAL').AsInteger;
            LVessel.ves_minresistance := FieldByName('VES_MINRESISTANCE').AsFloat;
            LVessel.ves_maxresistance := FieldByName('VES_MAXRESISTANCE').AsFloat;
            LVessel.ves_heattime      := FieldByName('VES_HEATTIME').AsInteger;
            LVessel.ves_adcloops      := FieldByName('VES_ADCLOOP').AsInteger;
            LVessel.ves_filltimeout   := FieldByName('VES_FILLTIMEOUT').AsInteger;
            LVessel.ves_span          := FieldByName('VES_SPAN').AsInteger;
            LVessel.ves_zero          := FieldByName('VES_ZERO').AsInteger;
            LVessel.ves_comment2      := FieldByName('VES_COMMENT').AsString;
          end;

          Next;
        end;
    end
    else
      SQLError(FDbClient, EOpenClient);

    for i := LVesList.Count - 1 downto 0 do
      if Assigned(LVesList[i]) then
      begin
        LVesList[i] := nil;
        FVesList.Delete(i);
      end;

    FVesList.Sort(@CompareVes);
  finally
    FDbClient.Close;
  end;
  LVesList.Free;
end;

function TDataMgmt.RefreshModListByDB: boolean;
var LModList: TObjectList;
    LModule:  TModule;
    i:        integer;

  function CompareMod(Item1: Pointer; Item2: Pointer): integer;
  begin
    Result := CompareText(TModule(Item1).mod_name, TModule(Item2).mod_name);
  end;

begin
  Result   := false;
  LModList := TObjectList.Create(false);

  if GetDbConnection then
  try
    for i := 0 to FModList.Count - 1 do LModList.Add(FModList[i]);

    setDbQuery(FDbClient, 'SELECT * FROM ' + db_t_module
                       + ' ORDER BY UPPER(MOD_NAME)');

    if FDbClient.Open then
    begin
      Result := FDbClient.Active;

      if Result then
      with FDbClient do
        while not Eof do
        begin
          i := GetModIndex(FieldByName('MOD_ID').AsInteger);

          if (i = -1) then
          begin
            // initilize with default values
            LModule := TModule.Create;
            FModList.Add(LModule);

            LModule.mod_id           := FieldByName('MOD_ID').AsInteger;
            LModule.mod_lastactive   := FieldByName('MOD_LASTTIMEACTIVE').AsString;
            LModule.mod_status       := 0;
            LModule.mod_coordinator  := nil;
            LModule.mod_vessel       := nil;
            LModule.mod_accumulator  := 255;
            LModule.mod_position     := '';
            LModule.mod_statusbyte   := #0;

            LModule.mod_getStatus    := false;
            LModule.mod_getOptions   := false;
            LModule.mod_setOptions   := false;
            LModule.mod_getPositions := false;
            LModule.mod_setPositions := false;
            LModule.mod_getPassword  := false;
            LModule.mod_setPassword  := false;
            LModule.mod_getSleepTime := false;
            LModule.mod_setSleepTime := false;
            LModule.mod_getAwakeTime := false;
            LModule.mod_setAwakeTime := false;
            LModule.mod_displayText  := '';
          end
          else begin
            LModule     := TModule(FModList[i]);
            LModList[i] := nil;
          end;

          LModule.mod_name          := FieldByName('MOD_NAME').AsString;
          LModule.mod_address       := FieldByName('MOD_ADDRESS').AsString;
          LModule.mod_minvoltage    := FieldByName('MOD_MINVOLTAGE').AsFloat;
          LModule.mod_maxvoltage    := FieldByName('MOD_MAXVOLTAGE').AsFloat;
          LModule.mod_criticvoltage := FieldByName('MOD_CRITICALVOLTAGE').AsInteger;
          LModule.mod_comment       := FieldByName('MOD_COMMENT').AsString;

          Next;
        end;
    end
    else
      SQLError(FDbClient, EOpenClient);

    for i := LModList.Count - 1 downto 0 do
      if Assigned(LModList[i]) then
      begin
        LModList[i] := nil;
        FModList.Delete(i);
      end;

    FModList.Sort(@CompareMod);
  finally
    FDbClient.Close;
  end;
  LModList.Free;
end;

function TDataMgmt.RefreshCooListByDB: boolean;
var LCooList:     TObjectList;
    LCoordinator: TCoordinator;
    i:            integer;

  function CompareCoo(Item1: Pointer; Item2: Pointer): integer;
  begin
    Result := CompareText(TCoordinator(Item1).coo_name, TCoordinator(Item2).coo_name);
  end;

begin
  Result   := false;
  LCooList := TObjectList.Create(false);

  if GetDbConnection then
  try
    for i := 0 to FCooList.Count - 1 do LCooList.Add(FCooList[i]);

    setDbQuery(FDbClient, 'SELECT * FROM ' + db_t_coordinator
                       + ' ORDER BY UPPER(COO_NAME)');

    if FDbClient.Open then
    begin
      Result := FDbClient.Active;

      if Result then
      with FDbClient do
        while not Eof do
        begin
          i := GetCooIndex(FieldByName('COO_ID').AsInteger);

          if (i = -1) then
          begin
            LCoordinator := TCoordinator.Create;
            FCooList.Add(LCoordinator);

            LCoordinator.coo_id         := FieldByName('COO_ID').AsInteger;
            LCoordinator.coo_status     := 0;
            LCoordinator.coo_xbeestatus := 0;
          end
          else begin
            LCoordinator := TCoordinator(FCooList[i]);
            LCooList[i]  := nil;
          end;

          LCoordinator.coo_name     := FieldByName('COO_NAME').AsString;
          LCoordinator.coo_comport  := FieldByName('COO_COMPORT').AsString;
          LCoordinator.coo_ip       := FieldByName('COO_IP').AsString;
          LCoordinator.coo_address  := FieldByName('COO_ADDRESS').AsString;
          LCoordinator.coo_position := FieldByName('COO_POSITION').AsString;
          LCoordinator.coo_comment  := FieldByName('COO_COMMENT').AsString;

          Next;
        end;
    end
    else
      SQLError(FDbClient, EOpenClient);

    for i := LCooList.Count - 1 downto 0 do
      if Assigned(LCooList[i]) then
      begin
        LCooList[i] := nil;
        FCooList.Delete(i);
      end;

    FCooList.Sort(@CompareCoo);
  finally
    FDbClient.Close;
  end;
  LCooList.Free;
end;

function TDataMgmt.RefreshDevListByDB: boolean;
var LDevList: TObjectList;
    LDevice:  TCryostat;
    i:        integer;

  function CompareDev(Item1: Pointer; Item2: Pointer): integer;
  begin            
    Result := CompareText(TCryostat(Item1).dev_name, TCryostat(Item2).dev_name);
  end;

begin
  Result   := false;
  LDevList := TObjectList.Create(false);

  if GetDbConnection then
  try
    for i := 0 to FDevList.Count - 1 do LDevList.Add(FDevList[i]);

    setDbQuery(FDbClient, 'SELECT * FROM ' + db_t_device
                       + ' ORDER BY UPPER(ILD_NAME)');

    if FDbClient.Open then
    begin
      Result := FDbClient.Active;

      if Result then
      with FDbClient do
        while not Eof do
        begin
          i := GetDevIndex(FieldByName('ILD_ID').AsInteger);

          if (i = -1) then
          begin
            LDevice := TCryostat.Create;
            FDevList.Add(LDevice);
            LDevice.dev_id          := FieldByName('ILD_ID').AsInteger;
            LDevice.dev_lastactive  := '';
            LDevice.dev_ch1_level   := 255;
            LDevice.dev_ch2_level   := 255;
            LDevice.dev_ch3_level   := 255;
            LDevice.dev_coordinator := nil;
          end
          else begin
            LDevice := TCryostat(FDevList[i]);
            LDevList[i] := nil;
          end;

          LDevice.dev_name        := FieldByName('ILD_NAME').AsString;
          LDevice.dev_address     := FieldByName('ILD_ADDRESS').AsString;
          LDevice.dev_comment     := FieldByName('ILD_COMMENT').AsString;
          LDevice.dev_ch1_enabled := NumberToBool(FieldByName('ILD_CH1_ENABLED').AsInteger);
          LDevice.dev_ch1_gas     := FieldByName('ILD_CH1_SUBSTANCE').AsString;
          LDevice.dev_ch1_span    := FieldByName('ILD_CH1_SPAN').AsFloat;
          LDevice.dev_ch1_zero    := FieldByName('ILD_CH1_ZERO').AsFloat;
          LDevice.dev_ch2_enabled := NumberToBool(FieldByName('ILD_CH2_ENABLED').AsInteger);
          LDevice.dev_ch2_gas     := FieldByName('ILD_CH2_SUBSTANCE').AsString;
          LDevice.dev_ch2_span    := FieldByName('ILD_CH2_SPAN').AsFloat;
          LDevice.dev_ch2_zero    := FieldByName('ILD_CH2_ZERO').AsFloat;
          LDevice.dev_ch3_enabled := NumberToBool(FieldByName('ILD_CH3_ENABLED').AsInteger);
          LDevice.dev_ch3_gas     := FieldByName('ILD_CH3_SUBSTANCE').AsString;
          LDevice.dev_ch3_span    := FieldByName('ILD_CH3_SPAN').AsFloat;
          LDevice.dev_ch3_zero    := FieldByName('ILD_CH3_ZERO').AsFloat;
          LDevice.dev_class       := FieldByName('ILD_CLASS').AsInteger;

          Next;
        end;
    end
    else
      SQLError(FDbClient, EOpenClient);

    for i := LDevList.Count - 1 downto 0 do
      if Assigned(LDevList[i]) then
      begin
        LDevList[i] := nil;
        FDevList.Delete(i);
      end;

    FDevList.Sort(@CompareDev);
  finally
    FDbClient.Close;
  end;
  LDevList.Free;
end;


function TDataMgmt.GetVesItem(const AId: Integer): TVessel;
var i: integer;
begin
  Result := nil;
  i      := GetVesIndex(AId);
  if (i > -1)then Result := TVessel(FVesList[i]);
end;

function TDataMgmt.GetModItem(const AId: Integer): TModule;
var i: integer;
begin
  Result := nil;
  i      := GetModIndex(AId);
  if (i > -1) then Result := TModule(FModList[i]);
end;

function TDataMgmt.GetCooItem(const AId: Integer): TCoordinator;
var i: integer;
begin
  Result := nil;
  i      := GetCooIndex(AId);
  if (i > -1) then Result := TCoordinator(FCooList[i]);
end;

function TDataMgmt.GetDevItem(const AId: Integer): TCryostat;
var i: integer;
begin
  Result := nil;
  i      := GetDevIndex(AId);
  if (i > -1) then Result := TCryostat(FDevList[i]);
end;



function TDataMgmt.GetVesIndex(const AId: Integer): integer;
var i: integer;
begin
  Result := -1;

  for i := 0 to FVesList.Count - 1 do
    if (TVessel(FVesList[i]).ves_id = AId) then
    begin
      Result := i;
      break;
    end;
end;

function TDataMgmt.GetModIndex(const AId: Integer): integer;
var i: integer;
begin
  Result := -1;

  for i := 0 to FModList.Count - 1 do
    if (TModule(FModList[i]).mod_id = AId) then
    begin
      Result := i;
      break;
    end;
end;

function TDataMgmt.GetCooIndex(const AId: Integer): integer;
var i: integer;
begin
  Result := -1;

  for i := 0 to FCooList.Count - 1 do
    if (TCoordinator(FCooList[i]).coo_id = AId) then
    begin
      Result := i;
      break;
    end;
end;

function TDataMgmt.GetDevIndex(const AId: Integer): integer;
var i: integer;
begin
  Result := -1;

  for i := 0 to FDevList.Count - 1 do
    if (TCryostat(FDevList[i]).dev_id = AId) then
    begin
      Result := i;
      break;
    end;
end;


function TDataMgmt.GetVesItemByName(const AName: word): TVessel;
var i: integer;
begin
  Result := nil;
  i      := GetVesIndexByName(AName);
  if (i > -1) then Result := TVessel(FVesList[i]);
end;

function TDataMgmt.GetModItemByAddr(const AAddr: string): TModule;
var i: integer;
begin
  Result := nil;
  i      := GetModIndexByAddr(AAddr);
  if (i > -1) then Result := TModule(FModList[i]);
end;

function TDataMgmt.GetDevItemByAddr(const AAddr: string): TCryostat;
var i: integer;
begin
  Result := nil;
  i      := GetDevIndexByAddr(AAddr);
  if (i > -1) then Result := TCryostat(FDevList[i]);
end;


function TDataMgmt.GetVesIndexByName(const AName: Word): integer;
var i: integer;
begin
  Result := -1;

  for i := 0 to FVesList.Count - 1 do
    if (TVessel(FVesList[i]).ves_name = AName) then
    begin
      Result := i;
      break;
    end;
end;

function TDataMgmt.GetModIndexByAddr(const AAddr: string): integer;
var i: integer;
begin
  Result := -1;

  for i := 0 to FModList.Count - 1 do
    if (TModule(FModList[i]).mod_address = AAddr) then
    begin
      Result := i;
      break;
    end;
end;

function TDataMgmt.GetDevIndexByAddr(const AAddr: string): integer;
var i: integer;
begin
  Result := -1;

  for i := 0 to FDevList.Count - 1 do
    if (TCryostat(FDevList[i]).dev_address = AAddr) then
    begin
      Result := i;
      break;
    end;
end;


function TDataMgmt.GetCopyOfVessel(const AIndex: integer): TVessel;
var LVessel: TVessel;
begin
  Result := nil;
  if AIndex < FVesList.Count then
  begin
    Result  := TVessel.Create;
    LVessel := TVessel(FVesList[AIndex]);

    with Result do
    begin
      ves_id            := LVessel.ves_id;
      ves_type_id       := LVessel.ves_type_id;
      ves_name          := LVessel.ves_name;
      ves_type          := LVessel.ves_type;
      ves_volume        := LVessel.ves_volume;
      ves_tare          := LVessel.ves_tare;
      ves_comment1      := LVessel.ves_comment1;
      ves_at_hzb_since  := LVessel.ves_at_hzb_since;

      ves_shortinterval := LVessel.ves_shortinterval;
      ves_longinterval  := LVessel.ves_longinterval;
      ves_minresistance := LVessel.ves_minresistance;
      ves_maxresistance := LVessel.ves_maxresistance;
      ves_heattime      := LVessel.ves_heattime;
      ves_adcloops      := LVessel.ves_adcloops;
      ves_filltimeout   := LVessel.ves_filltimeout;
      ves_span          := LVessel.ves_span;
      ves_zero          := LVessel.ves_zero;
      ves_comment2      := LVessel.ves_comment2;
      ves_options_in_db := LVessel.ves_options_in_db;

      ves_he_level      := LVessel.ves_he_level;
      ves_pressure      := LVessel.ves_pressure;
      ves_position      := LVessel.ves_position;
    end;
  end;
end;

function TDataMgmt.GetCopyOfModule(const AIndex: Integer): TModule;
var LMod: TModule;
begin
  Result := nil;
  if AIndex < FModList.Count then
  begin
    Result := TModule.Create;
    LMod   := TModule(FModList[AIndex]);

    with Result do
    begin
      mod_id            := LMod.mod_id;
      mod_name          := LMod.mod_name;
      mod_address       := LMod.mod_address;
      mod_lastactive    := LMod.mod_lastactive;
      mod_minvoltage    := LMod.mod_minvoltage;
      mod_maxvoltage    := LMod.mod_maxvoltage;
      mod_criticvoltage := LMod.mod_criticvoltage;
      mod_comment       := LMod.mod_comment;

      mod_status        := LMod.mod_status;
      mod_vessel        := nil;
      mod_coordinator   := nil;
      mod_accumulator   := LMod.mod_accumulator;
      mod_position      := LMod.mod_position;
      mod_statusbyte    := LMod.mod_statusbyte;

      mod_getStatus     := LMod.mod_getStatus;
      mod_getOptions    := LMod.mod_getOptions;
      mod_setOptions    := LMod.mod_setOptions;
      mod_getPositions  := LMod.mod_getPositions;
      mod_setPositions  := LMod.mod_setPositions;
      mod_getPassword   := LMod.mod_getPassword;
      mod_setPassword   := LMod.mod_setPassword;
      mod_getSleepTime  := LMod.mod_getSleepTime;
      mod_setSleepTime  := LMod.mod_setSleepTime;
      mod_getAwakeTime  := LMod.mod_getAwakeTime;
      mod_setAwakeTime  := LMod.mod_setAwakeTime;
      mod_displayText   := LMod.mod_displayText;
    end;
  end;
end;

function TDataMgmt.GetCopyOfCoordinator(const AIndex: Integer): TCoordinator;
var LCoo: TCoordinator;
begin
  Result := nil;
  if AIndex < FCooList.Count then
  begin
    Result := TCoordinator.Create;
    LCoo   := TCoordinator(FCooList[AIndex]);

    with Result do
    begin
      coo_id         := LCoo.coo_id;
      coo_name       := LCoo.coo_name;
      coo_comport    := LCoo.coo_comport;
      coo_ip         := LCoo.coo_ip;
      coo_address    := LCoo.coo_address;
      coo_position   := LCoo.coo_position;
      coo_comment    := LCoo.coo_comment;
      coo_status     := LCoo.coo_status;
      coo_xbeestatus := LCoo.coo_xbeestatus;
    end;
  end;
end;

function TDataMgmt.GetCopyOfIlmDevice(const AIndex: Integer): TCryostat;
var LDev: TCryostat;
begin
  Result := nil;
  if AIndex < FDevList.Count then
  begin
    Result := TCryostat.Create;
    LDev   := TCryostat(FDevList[AIndex]);

    with Result do
    begin
      dev_id          := LDev.dev_id;
      dev_name        := LDev.dev_name;
      dev_address     := LDev.dev_address;
      dev_comment     := LDev.dev_comment;
      dev_ch1_enabled := LDev.dev_ch1_enabled;
      dev_ch1_gas     := LDev.dev_ch1_gas;
      dev_ch1_span    := LDev.dev_ch1_span;
      dev_ch1_zero    := LDev.dev_ch1_zero;
      dev_ch2_enabled := LDev.dev_ch2_enabled;
      dev_ch2_gas     := LDev.dev_ch2_gas;
      dev_ch2_span    := LDev.dev_ch2_span;
      dev_ch2_zero    := LDev.dev_ch2_zero;
      dev_ch3_enabled := LDev.dev_ch3_enabled;
      dev_ch3_gas     := LDev.dev_ch3_gas;
      dev_ch3_span    := LDev.dev_ch3_span;
      dev_ch3_zero    := LDev.dev_ch3_zero;
      dev_lastactive  := LDev.dev_lastactive;
      dev_ch1_level   := LDev.dev_ch1_level;
      dev_ch2_level   := LDev.dev_ch2_level;
      dev_ch3_level   := LDev.dev_ch3_level;
      dev_class       := LDev.dev_class;
      dev_coordinator := nil;
    end;
  end;
end;


procedure TDataMgmt.EvaluateDatabase;
begin
  if not Assigned(FEvaluate) then
  begin
    FEvaluate := TEvaluateDbThread.CreateWithProperties();
    FEvaluate.OnTerminate := FreeEvaluateThread;
  end;
end;

procedure TDataMgmt.FreeEvaluateThread(Sender: TObject);
var
  lException: Exception;
begin
  TObject(lException) := FEvaluate.FatalException;
  FEvaluate := nil;
  if Assigned(lException) then
    if Assigned(FLogErrorProc) then
      FLogErrorProc(lException.Message)
    else
      raise Exception.Create(lException.Message);
end;

end.
