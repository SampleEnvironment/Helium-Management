unit SQLClientInterfaces;

interface

uses
  Classes;

resourcestring
  EOpenClient = 'Error open client';
  ESubmitSQL  = 'Error submitting SQL statement';
  EConnectSession = 'Error connecting session (data source: %s, user: %s)';

const
  GUID_CoreLabForOracle: TGUID  = '{AC465B79-E4AE-4302-8E0C-5C68746F3C98}';
  Id_CoreLabForOracle  : string = 'Oracle via CoreLab';

  GUID_ODBC32Bit : TGUID  = '{AA9AA47A-60A7-4840-B222-578D7D58F976}';
  Id_ODBC32Bit   : string = 'ODBC (32Bit)';

type
  IException = interface(IInterface)
  ['{001808C1-93E8-481D-B7C9-72DFEF24844A}']
    function Message: WideString; stdcall;
  end;

  ISQLImplementor = interface;
  ISQLClient = interface;

  ISQLNotificationLink = interface(IInterface)
  ['{96FBB5D0-9036-4D9B-9133-74F7DEF92F8F}']
    procedure Notification(const AItem: ISQLImplementor; const Operation: Byte); stdcall;
  end;

  ISQLImplementorFactory = interface(IInterface)
  ['{F687D73F-0BAA-4993-BD2A-D4F82E5566AC}']
    function SupportedDataBaseType: TGUID; stdcall;
    function SupporteDataBaseIdentifier: WideString; stdcall;
    function CreateClient: ISQLClient; stdcall;
  end;

  ISQLImplementor = interface(IInterface)
  ['{965DEFC8-3338-4921-9DA7-603C0CC98CF6}']
    function Id: TGUID; stdcall;
    function GetErrorBuffer: IException; stdcall;
    function GetSupportedDataBaseType: TGUID; stdcall;
    procedure InstallSQLNotificationLink(const ALink: ISQLNotificationLink); stdcall;
  end;

  ISQLDataField = interface(IInterface)
  ['{897A54B6-BC6A-4C5F-8EDE-A1D1F2A640F5}']
    function AsString: WideString; stdcall;
    function AsFloat: Double; stdcall;
    function AsInteger: Integer; stdcall;
    function AsBoolean: Boolean; stdcall;
    function AsDateTime: Double; stdcall;
  end;

  ISQLSession = interface(ISQLImplementor)
  ['{401C0C85-5E6A-42A1-A86B-167B8AFEB2BA}']
    function GetDataSource: WideString; stdcall;
    function GetUsername: WideString; stdcall;
    function GetPassword: WideString; stdcall;

    function ShowDataSourceDialog(var AValue: WideString): Boolean; stdcall;
    procedure InitSession(ADataSource, AUser, APassword: WideString); stdcall;
    function Connected: Boolean; stdcall;
    function Connect: Boolean; stdcall;
    procedure Disconnect; stdcall;
  end;

  ISQLClient = interface(ISQLImplementor)
  ['{F2D2EE00-EFD0-43E7-BDFE-FED35535A3F5}']
    function GetSession: ISQLSession; stdcall;
    function GetSQLText: WideString; stdcall;
    procedure SetSQLText(const AText: WideString); stdcall;
    function FieldByName(const AKey: string): ISQLDataField; stdcall;
    function Open: Boolean; stdcall;
    function Close: Boolean; stdcall;// queries db using mask given in SQLText field
    function Submit: Boolean; stdcall; // executes & submitts text in SQLText field
    function Next: Boolean; stdcall;
    function EoF: Boolean; stdcall;
    function Active: Boolean; stdcall;
  end;

implementation

end.
