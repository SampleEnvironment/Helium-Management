unit SQLAccessClasses;

interface

uses
  Windows, Classes, IniFiles, SysUtils, DB, Ora, ADODB, Contnrs,
  SQLClientInterfaces, IsaacStandardInterfaces, IsaacDataModule;

resourcestring
  E_SQL_GETVALUE = 'Error querying sql command %s, field %s[%d], message: %s';
  E_SQL_SETVALUE = 'Error executing sql command %s, message: %s';
  E_READFIELD    = 'Error reading field %s, message: %s';
  E_CONVERTVALUE = 'Failure converting %s to type %s';

type
  TFncExportSQLClientFactories = function: IDataModule; stdcall;

  TInterfacedException = class(TInterfacedObject, IException)
  private
    FMessage: string;
  protected
    function Message: WideString; stdcall;
  public
    constructor Create(const AErrMessage: string); reintroduce; overload;
  end;

  TCustomSQLImplementor = class;
  TSQLImplementorClass = class of TCustomSQLImplementor;

  TSQLNotificationLinkEvent = procedure(const AItem: ISQLImplementor; Operation: TOperation) of object;

  TSQLNotificationLink = class(TInterfacedObject, ISQLNotificationLink)
  private
    FLinkNotifyEvent: TSQLNotificationLinkEvent;
  protected
    procedure Notification(const AItem: ISQLImplementor; const Operation: Byte); stdcall;
  public
    constructor Create(ANotifyEvent: TSQLNotificationLinkEvent); reintroduce;
  end;

  TSQLClassFactory = class(TInterfacedPersistent, ISQLImplementorFactory)
  private
    FLink: ISQLNotificationLink;
    FItems: IDataModule;
    FQueryClass: TSQLImplementorClass;
    FSupportedDataBaseGUID: TGUID;
    FSupportedDataBaseId: string;
    procedure Insert(const AItem: ISQLImplementor);
    procedure Remove(const AItem: ISQLImplementor);
  protected
    procedure LinkNotification(const AItem: ISQLImplementor; Operation: TOperation);

    function SupportedDataBaseType: TGUID; stdcall;
    function SupporteDataBaseIdentifier: WideString; stdcall;
    function CreateClient: ISQLClient; stdcall;
  public
    constructor Create(const AQueryClass: TSQLImplementorClass;
      const ASupportedDataBaseGUID: TGUID; const ASupportedDataBaseId: string); reintroduce; virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
  end;

  TCustomSQLDataField = class(TInterfacedObject, ISQLDataField)
  protected
    function GetStrValue: string; virtual; abstract;
    function GetFloatValue: Double; virtual; abstract;
    function GetIntValue: Integer; virtual; abstract;
    function GetBoolValue: Boolean; virtual; abstract;

    function AsString: WideString; stdcall;
    function AsFloat: Double; stdcall;
    function AsInteger: Integer; stdcall;
    function AsBoolean: Boolean; stdcall;
    function AsDateTime: Double; stdcall;
  end;

  TCustomSQLImplementor = class(TCountedInterfacedObject, ISQLImplementor)
  private
    FId, FSupportedGUID: TGUID;
    FLink: ISQLNotificationLink;
    FErrorBuffer: IException;
    procedure UninstallLink;
  protected
    procedure LogError(const AMessage: string);
    function Id: TGUID; stdcall;
    function GetErrorBuffer: IException; stdcall;
    function GetSupportedDataBaseType: TGUID; stdcall;
    procedure InstallSQLNotificationLink(const ALink: ISQLNotificationLink); stdcall;
  public
    constructor Create(const ASupportedGUID: TGUID); reintroduce; virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
  end;        

  TCustomSQLSession = class(TCustomSQLImplementor, ISQLSession)
  protected
    function QueryServer: string; virtual; abstract;
    function QueryUsername: string; virtual; abstract;
    function QueryPassword: string; virtual; abstract;
    function DoShowDataSourceDialog(var AValue: string): boolean; virtual; abstract;
    procedure DoInitSession(const ADataSource, AUser, APassword: string); virtual; abstract;
    procedure DoSetConnected(const AValue: Boolean); virtual; abstract;
    function DoGetConnected: Boolean; virtual; abstract;
  public
    function GetDataSource: WideString; stdcall;
    function GetUsername: WideString; stdcall;
    function GetPassword: WideString; stdcall;

    function ShowDataSourceDialog(var AValue: WideString): Boolean; stdcall;
    procedure InitSession(ADataSource, AUser, APassword: WideString); stdcall;
    function Connected: Boolean; stdcall;
    function Connect: Boolean; stdcall;
    procedure Disconnect; stdcall;
  end;

  TCustomSQLClient = class(TCustomSQLImplementor, ISQLClient)
  private
    FCurrentBlock: Integer;
    procedure ReadError(const AKey: string; ABlock: Integer; AMsg: string);
  protected
    function DoGetSession: ISQLSession; virtual; abstract;
    function DoGetActive: Boolean; virtual; abstract;
    function DoOpen: Boolean; virtual; abstract;
    function DoSetNext: boolean; virtual; abstract;
    function DoGetEof: boolean; virtual; abstract;
    function DoSubmitExecute: Boolean; virtual; abstract;
    function DoClose: Boolean; virtual; abstract;
    function GetCurrentField(const AKey: string): ISQLDataField; virtual; abstract;
    function DoGetSQLText: string; virtual; abstract;
    procedure DoSetSQLText(const AValue: string); virtual; abstract;

    property CurrentBlock: Integer read FCurrentBlock;
  public
    constructor Create(const ASupportedGUID: TGUID); override;
    destructor Destroy; override;

    function GetSession: ISQLSession; stdcall;
    function GetSQLText: WideString; stdcall;
    procedure SetSQLText(const AText: WideString); stdcall;
    function FieldByName(const AKey: string): ISQLDataField; stdcall;
    function Open: Boolean; stdcall;
    function Close: Boolean; stdcall;
    function Submit: Boolean; stdcall;
    function Next: Boolean; stdcall;         
    function EoF: Boolean; stdcall;
    function Active: Boolean; stdcall;
  end;

  TOraSQLDataField = class(TCustomSQLDataField)
  private
    FFieldId: string;
    FQuery: TOraQuery;
  protected
    function GetStrValue: string; override;
    function GetFloatValue: Double; override;
    function GetIntValue: Integer; override;
    function GetBoolValue: Boolean; override;
  public
    constructor Create(const AFieldId: string; const AQuery: TOraQuery); reintroduce;
  end;

  TOraSQLSession = class(TCustomSQLSession)
  private
    FSession: TOraSession;
  protected
    function QueryServer: string; override;
    function QueryUsername: string; override;
    function QueryPassword: string; override;
    function DoShowDataSourceDialog(var AValue: string): boolean; override;
    procedure DoInitSession(const ADataSource, AUser, APassword: string); override;
    procedure DoSetConnected(const AValue: Boolean); override;
    function DoGetConnected: Boolean; override;
  public
    constructor Create(const ASupportedGUID: TGUID); override;
    destructor Destroy; override;

    property Session: TOraSession read FSession;
  end;

  TOraSQLClient = class(TCustomSQLClient)
  private
    FSession: ISQLSession;
    FQuery: TOraQuery;
  protected
    function DoGetSession: ISQLSession; override;             
    function DoGetActive: Boolean; override;
    function DoOpen: Boolean; override;           
    function DoGetEof: boolean; override;
    function DoSetNext: boolean; override;
    function DoSubmitExecute: boolean; override;
    function DoClose: Boolean; override;
    function GetCurrentField(const AKey: string): ISQLDataField; override;
    function DoGetSQLText: string; override;
    procedure DoSetSQLText(const AValue: string); override;
  public
    constructor Create(const ASupportedGUID: TGUID); override;
    destructor Destroy; override;
  end;

  TODBCDataField = class(TCustomSQLDataField)
  private
    FField: TField;
  protected
    function GetStrValue: string; override;
    function GetFloatValue: Double; override;
    function GetIntValue: Integer; override;
    function GetBoolValue: Boolean; override;
  public
    constructor Create(const AField: TField); reintroduce;
  end;

  TODBCSessionClass = class of TCustomODBCSession;

  TCustomODBCSession = class(TCustomSQLSession)
  private
    FConnection: TADOConnection;
  protected
    function GetConnectionString(const AServer, AUser, APassword: string): string; virtual; abstract;
    function DoShowDataSourceDialog(var AValue: string): boolean; override;
    procedure DoInitSession(const ADataSource, AUser, APassword: string); override;
    procedure DoSetConnected(const AValue: Boolean); override;
    function DoGetConnected: Boolean; override;
  public
    constructor Create(const ASupportedGUID: TGUID); override;
    destructor Destroy; override;

    property Connection: TADOConnection read FConnection;
  end;

  TCustomODBCClient = class(TCustomSQLClient)
  private
    FSession: ISQLSession;
    FQuery: TADOQuery;
  protected
    function GetSessionClass: TODBCSessionClass; virtual; abstract;
    function DoGetSession: ISQLSession; override;             
    function DoGetActive: Boolean; override;
    function DoOpen: Boolean; override;
    function DoGetEof: boolean; override;
    function DoSetNext: boolean; override;
    function DoSubmitExecute: boolean; override;
    function DoClose: Boolean; override;
    function GetCurrentField(const AKey: string): ISQLDataField; override;
    function DoGetSQLText: string; override;
    procedure DoSetSQLText(const AValue: string); override;
  public
    constructor Create(const ASupportedGUID: TGUID); override;
    destructor Destroy; override;
  end;

const
  SOraConnectionString = 'DSN=%s;Uid=%s;Pwd=%s;Persist Security Info=True';

type
  TOracleODBCSession = class(TCustomODBCSession)
  protected
    function GetConnectionString(const AServer, AUser, APassword: string): string; override;
    function QueryServer: string; override;
    function QueryUsername: string; override;
    function QueryPassword: string; override;
  end;

  TOracleODBCClient = class(TCustomODBCClient)
  protected
    function GetSessionClass: TODBCSessionClass; override;
  end;

  function GetSQLClientFactories: IDataModule; stdcall;
  // methods to find existing DSNs
const
  SQL_NULL_HANDLE = 0;
  SQL_HANDLE_ENV = 1;
  SQL_HANDLE_DBC = 2;
  SQL_HANDLE_STMT = 3;
  SQL_HANDLE_DESC = 4;

  SQL_ERROR = -1;
  SQL_SUCCESS = 0;
  SQL_FETCH_NEXT = 1;
  SQL_FETCH_FIRST = 2;  
  SQL_MAX_DSN_LENGTH = 32;
  SQL_MAX_OPTION_STRING_LENGTH = 256;

type
  SQLCHAR = Char;
  SQLSMALLINT = smallint;
  SQLUSMALLINT = Word;
  SQLRETURN = SQLSMALLINT;
  SQLHANDLE = LongInt;
  SQLHENV = SQLHANDLE;
  SQLHDBC = SQLHANDLE;
  SQLHSTMT = SQLHANDLE;
  SQLINTEGER = LongInt;
  SQLUINTEGER = Cardinal;
  SQLPOINTER = Pointer;
  SQLREAL = real;
  SQLDOUBLE = Double;
  SQLFLOAT = Double;
  PSQLCHAR = PChar;
  PSQLINTEGER = ^SQLINTEGER;
  PSQLUINTEGER = ^SQLUINTEGER;
  PSQLSMALLINT = ^SQLSMALLINT;
  PSQLUSMALLINT = ^SQLUSMALLINT;
  PSQLREAL = ^SQLREAL;
  PSQLDOUBLE = ^SQLDOUBLE;
  PSQLFLOAT = ^SQLFLOAT;
  PSQLHandle = ^SQLHANDLE;
  EODBCError = class(Exception);

  procedure GetDataSources(const DSList: TStrings);

  // extenal
  function SQLAllocEnv(out Handle: SQLHANDLE): SQLRETURN; stdcall; external 'ODBC32.DLL';
  function SQLFreeEnv(Handle: SQLHANDLE): SQLRETURN; stdcall; external 'ODBC32.DLL';
  function SQLAllocHandle(
    HandleType: SQLSMALLINT;
    InputHandle: SQLHANDLE;
    out OutHandle: SQLHANDLE): SQLRETURN; stdcall; external 'ODBC32.DLL';

  function SQLFreeHandle(
    HandleType: SQLSMALLINT;
    Handle: SQLHANDLE): SQLRETURN; stdcall; external 'ODBC32.DLL';

  function SQLDataSources(
    hEnv: SQLHENV;
    Direction: SQLUSMALLINT;
    szDSN: PSQLCHAR;
    cbDSN: SQLSMALLINT;
    var pbDSN: PSQLSMALLINT;
    szDescr: PSQLCHAR;
    cbDescr: SQLSMALLINT;
    var pbDescr: PSQLSMALLINT): SQLRETURN; stdcall; external 'ODBC32.DLL';
    
  function SQLGetDiagRec(
    HandleType: SQLSMALLINT;
    Handle: SQLHANDLE;
    RecNumber: SQLSMALLINT;
    SQLState : PSQLCHAR;
    out NativeErrorPtr: PSQLINTEGER;
    MessageText: PSQLCHAR;
    BufferLength: SQLSMALLINT;
    out TextLengthPtr: PSQLSMALLINT): SQLRETURN; stdcall; external 'ODBC32.DLL';

var
  RegisteredSQLClientFactories: TObjectList;

implementation

uses
  Math, ExtFileClasses, ActiveX, Dialogs, FrmODBCSelectDSN;

function GetSQLClientFactories: IDataModule; stdcall;
var
  I: Integer;
  obj: ISQLImplementorFactory;
begin
  result := TCustomDataModule.Create(PChar('SQLClientFactories'));
  for I := 0 to RegisteredSQLClientFactories.Count - 1 do
  begin
    obj := TSQLClassFactory(RegisteredSQLClientFactories[I]) as ISQLImplementorFactory;
    Result.SetInterfaceValue(PChar(GUIDToString(obj.SupportedDataBaseType)), obj);
  end;
end;

{ TInterfacedException }

constructor TInterfacedException.Create(const AErrMessage: string);
begin
  inherited Create;
  FMessage := AErrMessage;
end;

function TInterfacedException.Message: WideString;
begin
  result := FMessage;
end;

{ TSQLNotificationLink }

constructor TSQLNotificationLink.Create(
  ANotifyEvent: TSQLNotificationLinkEvent);
begin
  inherited Create;
  FLinkNotifyEvent := ANotifyEvent;
end;

procedure TSQLNotificationLink.Notification(const AItem: ISQLImplementor;
  const Operation: Byte);
begin
  if Assigned(FLinkNotifyEvent) then
    FLinkNotifyEvent(AItem, TOperation(Operation));
end;      

{ TSQLClassFactory }

constructor TSQLClassFactory.Create(const AQueryClass: TSQLImplementorClass;
  const ASupportedDataBaseGUID: TGUID; const ASupportedDataBaseId: string);
begin
  inherited Create;
  FQueryClass := AQueryClass;
  FSupportedDataBaseGUID := ASupportedDataBaseGUID;
  FSupportedDataBaseId := ASupportedDataBaseId;
  FLink := TSQLNotificationLink.Create(LinkNotification);
  FItems := TCustomDataModule.Create(nil);
end;

destructor TSQLClassFactory.Destroy;
begin
  FItems := nil;
  FLink := nil;
  inherited;
end;

procedure TSQLClassFactory.Insert(const AItem: ISQLImplementor);
begin
  if FItems.HasValue(PChar(GUIDToString(AItem.Id))) then
  begin
    FItems.SetInterfaceValue(PChar(GUIDToString(AItem.Id)), AItem);
    AItem._Release
  end;
end;

procedure TSQLClassFactory.LinkNotification(const AItem: ISQLImplementor;
  Operation: TOperation);
begin
  case Operation of
    opInsert: Insert(AItem);
    opRemove: Remove(AItem);
  end;
end;

procedure TSQLClassFactory.Remove(const AItem: ISQLImplementor);
begin
  if FItems.HasValue(PChar(GUIDToString(AItem.Id))) then
  begin
    AItem._AddRef;
    FItems.DeleteValue(PChar(GUIDToString(AItem.Id)));
  end;
end;

procedure TSQLClassFactory.BeforeDestruction;
begin
  inherited;
  if FItems.ValueCount > 0 then
    raise Exception.Create(Format('Error releasing sql class factory (%s), there are still references to data objects!', [SupporteDataBaseIdentifier]));
end;

function TSQLClassFactory.CreateClient: ISQLClient;
begin
  result := FQueryClass.Create(FSupportedDataBaseGUID) as ISQLClient;
  result.InstallSQLNotificationLink(FLink);
end;

function TSQLClassFactory.SupporteDataBaseIdentifier: WideString;
begin
  result := FSupportedDataBaseId;
end;

function TSQLClassFactory.SupportedDataBaseType: TGUID;
begin
  result := FSupportedDataBaseGUID;
end;

{ TCustomSQLDataField }

function TCustomSQLDataField.AsBoolean: Boolean;
begin
  result := GetBoolValue;
end;

function TCustomSQLDataField.AsDateTime: Double;
begin
  result := GetFloatValue;
end;

function TCustomSQLDataField.AsFloat: Double;
begin
  result := GetFloatValue;
end;

function TCustomSQLDataField.AsInteger: Integer;
begin
  result := GetIntValue;
end;

function TCustomSQLDataField.AsString: WideString;
begin
  result := WideString(GetStrValue);
end;

{ TCustomSQLImplementor }

constructor TCustomSQLImplementor.Create(const ASupportedGUID: TGUID);
begin
  inherited Create;
  FSupportedGUID := ASupportedGUID;
  CoCreateGUID(FId);
  FErrorBuffer := nil; // init ErrorBuffer
end;

destructor TCustomSQLImplementor.Destroy;
begin
  FErrorBuffer := nil;
  inherited;
end;

procedure TCustomSQLImplementor.BeforeDestruction;
begin
  inherited;
  UninstallLink;
end;

function TCustomSQLImplementor.GetErrorBuffer: IException;
begin
  result := FErrorBuffer;
  FErrorBuffer := nil;
end;

function TCustomSQLImplementor.GetSupportedDataBaseType: TGUID;
begin
  result := FSupportedGUID;
end;

function TCustomSQLImplementor.Id: TGUID;
begin
  result := FId;
end;

procedure TCustomSQLImplementor.InstallSQLNotificationLink(
  const ALink: ISQLNotificationLink);
begin
  if Assigned(ALink) then
  begin
    UninstallLink;
    FLink := ALink;
    FLink._Release;
    FLink.Notification(Self, Byte(opInsert));
  end;
end;

procedure TCustomSQLImplementor.LogError(const AMessage: string);
begin
  FErrorBuffer := TInterfacedException.Create(AMessage);
end;

procedure TCustomSQLImplementor.UninstallLink;
begin
  if Assigned(FLink) then
  begin
    FLink.Notification(Self, Byte(opRemove));
    FLink._AddRef;
    FLink := nil;
  end;
end;  

{ TCustomSQLSession }

function TCustomSQLSession.Connect: Boolean;
begin
  try
    DoSetConnected(True);
    result := Connected;
  except
    on E: Exception do
    begin
      Result := False;
      LogError(E.Message);
    end;
  end;
end;

function TCustomSQLSession.Connected: Boolean;
begin
  try
    result := DoGetConnected;
  except
    on E: Exception do
    begin
      result := False;
      LogError(E.Message);
    end;
  end;
end;

procedure TCustomSQLSession.Disconnect;
begin
  try
    DoSetConnected(False);
  except
    on E: Exception do
      LogError(E.Message);
  end;
end;

function TCustomSQLSession.GetPassword: WideString;
begin
  try
    result := QueryPassword;
  except
    on E: Exception do
      LogError(E.Message);
  end;
end;

function TCustomSQLSession.GetDataSource: WideString;
begin
  try
    result := QueryServer;
  except
    on E: Exception do
      LogError(E.Message);
  end;
end;

function TCustomSQLSession.GetUsername: WideString;
begin
  try
    result := QueryUsername;
  except
    on E: Exception do
      LogError(E.Message);
  end;
end;

procedure TCustomSQLSession.InitSession(ADataSource, AUser, APassword: WideString);
begin
  try
    DoInitSession(ADataSource, AUser, APassword);
  except
    on E: Exception do
      LogError(E.Message);
  end;
end;

function TCustomSQLSession.ShowDataSourceDialog(
  var AValue: WideString): Boolean;
var
  lValue: string;
begin
  lValue := AValue;
  result := DoShowDataSourceDialog(lValue);
  if Result then
    AValue := lValue;
end;

{ TCustomSQLClient }

constructor TCustomSQLClient.Create(const ASupportedGUID: TGUID);
begin
  inherited;
  FCurrentBlock := -1;
end;

destructor TCustomSQLClient.Destroy;
begin
  Close;
  inherited;
end;

function TCustomSQLClient.EoF: Boolean;
begin
  result := DoGetEof;
end;

function TCustomSQLClient.Active: Boolean;
begin
  result := DoGetActive;
end;

function TCustomSQLClient.Close: Boolean;
begin
  Result := DoClose;
end;

function TCustomSQLClient.FieldByName(const AKey: string): ISQLDataField;
begin
  try
    result := GetCurrentField(AKey);
  except
    on E: Exception do
      ReadError(AKey, FCurrentBlock, E.Message);
  end;
end;

function TCustomSQLClient.GetSession: ISQLSession;
begin
  result := DoGetSession;
end;

function TCustomSQLClient.GetSQLText: WideString;
begin
  result := DoGetSQLText;
end;

function TCustomSQLClient.Next: Boolean;
begin
  Inc(FCurrentBlock);
  Result := DoSetNext;
end;

function TCustomSQLClient.Open: Boolean;
begin
  Result := DoOpen;
  if Result then
    FCurrentBlock := -1;
end;

procedure TCustomSQLClient.ReadError(const AKey: string; ABlock: Integer;
  AMsg: string);
begin
  raise Exception.Create(Format(E_SQL_GETVALUE, [GetSQLText, AKey, ABlock, AMsg]));
end;

procedure TCustomSQLClient.SetSQLText(const AText: WideString);
begin
  DoSetSQLText(AText);
end;

function TCustomSQLClient.Submit: Boolean;
begin
  Result := DoSubmitExecute;
end;

{ TOraSQLDataField }

constructor TOraSQLDataField.Create(const AFieldId: string;
  const AQuery: TOraQuery);
begin
  inherited Create;
  FQuery := AQuery;
  FFieldId := AFieldId;
end;

function TOraSQLDataField.GetBoolValue: Boolean;
begin
  result := FQuery.FieldByName(FFieldId).AsBoolean;
end;

function TOraSQLDataField.GetFloatValue: Double;
begin
  result := FQuery.FieldByName(FFieldId).AsFloat;
end;

function TOraSQLDataField.GetIntValue: Integer;
begin
  result := FQuery.FieldByName(FFieldId).AsInteger;
end;

function TOraSQLDataField.GetStrValue: string;
begin
  result := FQuery.FieldByName(FFieldId).AsString;
end;     

{ TOraSQLSession }

constructor TOraSQLSession.Create;
begin
  inherited;
  FSession := TOraSession.Create(nil);
end;

destructor TOraSQLSession.Destroy;
begin
  FSession.Free;
  inherited;
end;

function TOraSQLSession.DoGetConnected: Boolean;
begin
  result := FSession.Connected;
end;

procedure TOraSQLSession.DoInitSession(const ADataSource, AUser, APassword: string);
begin
  FSession.Server := ADataSource;
  FSession.Username := AUser;
  FSession.Password := APassword;

  FSession.Options.ConnectionTimeout := 2000;
  FSession.Options.Direct            := True;
  FSession.Options.StatementCache    := False;
  FSession.ConnectPrompt             := False;
end;

procedure TOraSQLSession.DoSetConnected(const AValue: Boolean);
begin
  FSession.Connect;
end;

function TOraSQLSession.DoShowDataSourceDialog(var AValue: string): boolean;
begin
  result := InputQuery('select data source', 'host:port:sid', AValue);
end;

function TOraSQLSession.QueryPassword: string;
begin
  result := FSession.Password;
end;

function TOraSQLSession.QueryServer: string;
begin
  result := FSession.Server;
end;

function TOraSQLSession.QueryUsername: string;
begin
  result := FSession.Username;
end;

{ TOraSQLClient }

constructor TOraSQLClient.Create(const ASupportedGUID: TGUID);
var
  lSession: TOraSQLSession;
begin
  inherited;
  FQuery := TOraQuery.Create(nil);
  lSession := TOraSQLSession.Create(ASupportedGUID);
  FSession := lSession;
  FQuery.Session := lSession.Session;
end;

destructor TOraSQLClient.Destroy;
begin
  FQuery.Session := nil;
  FSession := nil;
  FQuery.Free;
  inherited;
end;

function TOraSQLClient.DoClose: Boolean;
begin
  try
    FQuery.Close;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      LogError(E.Message);
    end;
  end;
end;

function TOraSQLClient.DoGetActive: Boolean;
begin
  result := FQuery.Active;
end;

function TOraSQLClient.DoGetEof: boolean;
begin
  result := FQuery.Eof;
end;

function TOraSQLClient.DoGetSession: ISQLSession;
begin
  result := FSession;
end;

function TOraSQLClient.DoGetSQLText: string;
begin
  result := FQuery.SQL.Text;
end;

function TOraSQLClient.DoSubmitExecute: boolean;
begin
  try
    FQuery.Execute;
    FQuery.Session.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      LogError(E.Message);
    end;
  end;
end;

function TOraSQLClient.DoOpen: Boolean;
begin
  try
    FQuery.Open;
    result := True;
  except
    on E: Exception do
    begin
      Result := False;
      LogError(E.Message);
    end;
  end;
end;

function TOraSQLClient.DoSetNext: boolean;
begin
  try
    FQuery.Next;
    result := True;
  except
    on E: Exception do
    begin
      Result := False;
      LogError(E.Message);
    end;
  end;
end;

procedure TOraSQLClient.DoSetSQLText(const AValue: string);
begin
  try
    FQuery.SQL.Text := AValue;
  except
    on E: Exception do
      LogError(E.Message);
  end;
end;

function TOraSQLClient.GetCurrentField(
  const AKey: string): ISQLDataField;
begin
  result := TOraSQLDataField.Create(AKey, FQuery);
end;

{ TODBCDataField }

constructor TODBCDataField.Create(const AField: TField);
begin
  inherited Create;
  FField := AField;
end;

function TODBCDataField.GetBoolValue: Boolean;
begin
  result := FField.AsBoolean;
end;

function TODBCDataField.GetFloatValue: Double;
begin
  result := FField.AsFloat;
end;

function TODBCDataField.GetIntValue: Integer;
begin
  result := FField.AsInteger;
end;

function TODBCDataField.GetStrValue: string;
begin
  result := FField.AsString;
end;

{ TCustomODBCSession }

constructor TCustomODBCSession.Create(const ASupportedGUID: TGUID);
begin
  inherited;
  FConnection := TADOConnection.Create(nil);
end;

destructor TCustomODBCSession.Destroy;
begin
  FConnection.Free;
  inherited;
end;

function TCustomODBCSession.DoGetConnected: Boolean;
begin
  result := FConnection.Connected;
end;

procedure TCustomODBCSession.DoInitSession(const ADataSource, AUser, APassword: string);
begin
  FConnection.ConnectionString := GetConnectionString(ADataSource, AUser, APassword);
  FConnection.LoginPrompt := False;
end;

procedure TCustomODBCSession.DoSetConnected(const AValue: Boolean);
begin
  FConnection.Connected := AValue;
end;

function TCustomODBCSession.DoShowDataSourceDialog(var AValue: string): boolean;
begin
  with TFormODBCSelectDSN.Create(nil) do
    try
      Result := Execute(AValue);
    finally
      Free;
    end;
end;

{ TCustomODBCClient }

constructor TCustomODBCClient.Create(const ASupportedGUID: TGUID);
var
  lSession: TCustomODBCSession;
begin
  inherited;
  FQuery := TADOQuery.Create(nil);
  lSession := GetSessionClass.Create(ASupportedGUID);
  FSession := lSession;
  FQuery.Connection := lSession.Connection;
end;

destructor TCustomODBCClient.Destroy;
begin
  FQuery.Free;
  FSession := nil;
  inherited;
end;

function TCustomODBCClient.DoClose: Boolean;
begin
  try
    FQuery.Close;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      LogError(E.Message);
    end;
  end;
end;

function TCustomODBCClient.DoGetActive: Boolean;
begin
  result := FQuery.Active;
end;

function TCustomODBCClient.DoGetEof: boolean;
begin
  result := FQuery.Eof;
end;

function TCustomODBCClient.DoGetSession: ISQLSession;
begin
  result := FSession;
end;

function TCustomODBCClient.DoGetSQLText: string;
begin
  result := FQuery.SQL.Text;
end;

function TCustomODBCClient.DoOpen: Boolean;
begin
  try
    FQuery.Open;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      LogError(E.Message);
    end;
  end;
end;

function TCustomODBCClient.DoSetNext: boolean;
begin
  try
    FQuery.Next;
    result := True;
  except
    on E: Exception do
    begin
      Result := False;
      LogError(E.Message);
    end;
  end;
end;

procedure TCustomODBCClient.DoSetSQLText(const AValue: string);
begin
  FQuery.SQL.Text := AValue;
end;

function TCustomODBCClient.DoSubmitExecute: boolean;
begin
  try
    FQuery.ExecSQL;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      LogError(E.Message);
    end;
  end;
end;

function TCustomODBCClient.GetCurrentField(const AKey: string): ISQLDataField;
begin
  result := TODBCDataField.Create(FQuery.FieldByName(AKey));
end;

{ TOracleODBCSession }

function TOracleODBCSession.GetConnectionString(const AServer, AUser,
  APassword: string): string;
begin
  result := Format(SOraConnectionString, [AServer, AUser, APassword]);
end;

function TOracleODBCSession.QueryPassword: string;
var
  sList: TStringList;
begin
  sList := TStringList.Create;
  try
    sList.Text := StringReplace(Connection.ConnectionString, ';', #13, [rfReplaceAll]);
    sList.NameValueSeparator := '=';
    result := sList.Values['Pwd'];
  finally
    sList.Free;
  end;
end;

function TOracleODBCSession.QueryServer: string;
var
  sList: TStringList;
begin
  sList := TStringList.Create;
  try
    sList.Text := StringReplace(Connection.ConnectionString, ';', #13, [rfReplaceAll]);
    sList.NameValueSeparator := '=';
    result := sList.Values['DSN'];
  finally
    sList.Free;
  end;
end;

function TOracleODBCSession.QueryUsername: string;
var
  sList: TStringList;
begin
  sList := TStringList.Create;
  try
    sList.Text := StringReplace(FConnection.ConnectionString, ';', #13, [rfReplaceAll]);
    sList.NameValueSeparator := '=';
    result := sList.Values['Uid'];
  finally
    sList.Free;
  end;
end;

{ TOracleODBCClient }

function TOracleODBCClient.GetSessionClass: TODBCSessionClass;
begin
  result := TOracleODBCSession;
end;

procedure GetDataSources(const DSList: TStrings);

  function GetErrorMsg(const AHandleType: SQLSMALLINT;
    AHandle: SQLHANDLE; var AMessage: string): Boolean;
  var
    lSQLBuf : Array[0..5] of Char;
    lMessageBuf: Array[0..MaxByte-1] of Char;
    lNativeErrPointer: PSQLINTEGER;
    lBufSize: SQLSMALLINT;
    lTextLen: PSQLSMALLINT;
  begin
    Result := False;
    AMessage := '';

    FillChar(lSQLBuf, SizeOf(lSQLBuf), #0);
    FillChar(lMessageBuf, SizeOf(lMessageBuf), #0);
    Integer(lTextLen) := 0;

    if SQLGetDiagRec(AHandleType, AHandle,
                     1, @lSQLBuf[1],
                     lNativeErrPointer,
                     @lMessageBuf[1],
                     SizeOf(lMessageBuf),
                     lTextLen) <> SQL_ERROR then
    begin
      Result := True;
      AMessage := PChar(@lMessageBuf[1]);
    end;
  end;

var
  szDSN: array[0..SQL_MAX_DSN_LENGTH] of SQLCHAR;
  szDescr: array[0..SQL_MAX_OPTION_STRING_LENGTH] of SQLCHAR;
  lResult: SQLRETURN;
  hEnv, hDBC: SQLHANDLE;
  cbDSN: PSQLSMALLINT;
  cbDescr: PSQLSMALLINT;
  lErrMsg: string;
begin
  DSList.clear;

  lResult := SQLAllocEnv(hEnv);
  if lResult = SQL_SUCCESS then
    try
      FillChar(szDSN, SizeOf(szDSN), #0);
      FillChar(szDescr, SizeOf(szDescr), #0);
      Integer(cbDSN) := 0;
      Integer(cbDescr) := 0;
      lResult := SQLDataSources(hEnv, SQL_FETCH_FIRST, @szDSN[1], sizeof(szDSN), cbDSN,
                                @szDescr[1], SizeOf(szDescr), cbDescr);
      while lResult = SQL_SUCCESS do
      begin
        DSList.Add(Format('%s@%s', [PChar(@szDSN[1]), PChar(@szDescr[1])]));
        Integer(cbDSN) := 0;
        Integer(cbDescr) := 0;
        FillChar(szDSN, SizeOf(szDSN), #0);
        FillChar(szDescr, SizeOf(szDescr), #0);
        lResult := SQLDataSources(hEnv, SQL_FETCH_NEXT, @szDSN[1], sizeof(szDSN), cbDSN,
                                  @szDescr[1], SizeOf(szDescr), cbDescr);
      end;

      if (lResult = SQL_ERROR) then
        if GetErrorMsg(SQL_HANDLE_ENV, hEnv, lErrMsg) then
          raise Exception.Create(Format('Failure loading available data source names, message: %s', [lErrMsg]))
        else
          Exception.Create('Failure loading available data source names');
    finally
      SQLFreeEnv(hEnv);
    end
  else
    raise EODBCError.Create('Cannot allocate ODBC handle');
end;

initialization
  RegisteredSQLClientFactories := TObjectList.Create(True);

  RegisteredSQLClientFactories.Add(
    TSQLClassFactory.Create(TOraSQLClient, GUID_CoreLabForOracle, Id_CoreLabForOracle));
  RegisteredSQLClientFactories.Add(
    TSQLClassFactory.Create(TOracleODBCClient, GUID_ODBC32Bit, Id_ODBC32Bit));

finalization
  RegisteredSQLClientFactories.Free;

end.

