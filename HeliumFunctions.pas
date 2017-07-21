unit HeliumFunctions;

interface

uses
  Windows, Classes, SysUtils, StrUtils, Dialogs, Registry, CPort, Grids,
  SQLClientInterfaces, ADODB;

  { - - - - V I S U A L - C O M P O N E N T S - - - - }
  procedure deleteRow(AStringGrid: TStringGrid; ARow: Integer);
  procedure setLastColSize(AStringGrid: TStringGrid);


  { - - - - - - - - D A T A B A S E - - - - - - - - - }
  function floatToDbFloat(AValue: double): string;
  procedure setDbQuery(AQuery: ISQLClient; ASqlText: string);


  { - - - - R E G I S T R Y - F U N C T I O N S - - - }
  function getRegString (AKeyName: string; AName: string): string;
  function getRegInteger(AKeyName: string; AName: string): integer;
  function getRegFloat(AKeyName: string; AName: string): double;
  function getRegBoolean(AKeyName: string; AName: string): boolean;
  procedure deleteKey(APath: string);


  function encrypt(AChars: string): string;
  function decrypt(AChars: string): string;

  { - - - - - - - - C O M - P O R T - - - - - - - - - }
  function getPortList: TStringList;
  function portExists(APort: string): boolean;
  function createComPort(APort: string): TComPort;
  function openComPort(APort: TComPort): boolean;

const
  FOLDER_REPORT = 'C:\Dokumente und Einstellungen\local_admin\Desktop\';
  ERROR_LOGFILE = 'C:\HeMgmt_ErrorList.log';
  ZERO_OFFSET   = 5000;

  { - - - - - - D B - C O N S T A N T S - - - - - - - }
  db_t_coordinator    = 'HLM_COORDINATOR';
  db_t_measurement    = 'HLM_MEASUREMENT';
  db_t_ilmmeasurement = 'HLM_ILMMEASUREMENT';
  db_t_module         = 'HLM_MODULE';
  db_t_device         = 'HLM_ILMDEVICE';
  db_t_hlm_vessel     = 'HLM_VESSEL';
  db_t_hen2_vessel    = 'IND690_VESSEL';
  db_t_type           = 'IND690_TYPE';
  db_t_weighing       = 'IND690_WEIGHING';


implementation

{ - - - - - - - - - - - - - - - - - - - - - - - - - }
{ - - - - V I S U A L - C O M P O N E N T S - - - - }
{ - - - - - - - - - - - - - - - - - - - - - - - - - }
// delete row in TStringGrid
procedure deleteRow(AStringGrid: TStringGrid; ARow: Integer);
var i: integer;
begin
  with AStringGrid do
    if (0 < ARow) and (ARow < RowCount) then
    begin
      for i := ARow to RowCount - 2 do Rows[i].Assign(Rows[i + 1]);
      Rows[RowCount - 1].Clear;
      if (RowCount > 2) then RowCount := RowCount - 1;
    end;
end;

// set ColSize of last column in AStringGrid
procedure setLastColSize(AStringGrid: TStringGrid);
var LTemp: integer;

  function getColSize: integer;
  var i: integer;
  begin
    Result := 0;
    for i := 0 to AStringGrid.ColCount - 2
      do Result := Result + AStringGrid.ColWidths[i];
  end;

begin
  LTemp := AStringGrid.Width - getColSize - AStringGrid.ColCount - 3;
  if (LTemp > 80) then AStringGrid.ColWidths[AStringGrid.ColCount-1] := LTemp;
end;


{ - - - - - - - - - - - - - - - - - - - - - - - - - }
{ - - - - - - - - D A T A B A S E - - - - - - - - - }
{ - - - - - - - - - - - - - - - - - - - - - - - - - }

function floatToDbFloat(AValue: double): string;
begin
  Result := floatToStr(AValue);
  if not (DecimalSeparator = '.')
   then Result := StringReplace(Result, DecimalSeparator, '.', [rfReplaceAll]);
end;

procedure setDbQuery(AQuery: ISQLClient; ASqlText: string);
begin
  try AQuery.SetSQLText(ASqlText);
  finally
  end;
end;


{ - - - - - - - - - - - - - - - - - - - - - - - - - }
{ - - - - R E G I S T R Y - F U N C T I O N S - - - }
{ - - - - - - - - - - - - - - - - - - - - - - - - - }
// read value of diffrent types from windows registry
// Registry Key:   Software\HZB_Helium\ + AKeyName
// Registry Value: AName
function getRegString (AKeyName: string; AName: string): string;
var LReg: TRegistry;
begin
  Result := '';
  LReg := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  try
    if LReg.KeyExists('Software\HZB_Helium\' + AKeyName) then
    begin
      LReg.OpenKey('Software\HZB_Helium\' + AKeyName,false);
      if LReg.ValueExists(AName) then Result := LReg.ReadString(AName);
    end;
  finally
    LReg.CloseKey;
    LReg.Free;
  end;
end;

function getRegInteger(AKeyName: string; AName: string): integer;
var LReg: TRegistry;
begin
  Result := 0;
  LReg := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  try
    if LReg.KeyExists('Software\HZB_Helium\' + AKeyName) then
    begin
      LReg.OpenKey('Software\HZB_Helium\' + AKeyName,false);
      if LReg.ValueExists(AName) then Result := LReg.ReadInteger(AName);
    end;
  finally
    LReg.CloseKey;
    LReg.Free;
  end;
end;

function getRegFloat(AKeyName: string; AName: string): double;
var LReg: TRegistry;
begin
  Result := 0;
  LReg := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  try
    if LReg.KeyExists('Software\HZB_Helium\' + AKeyName) then
    begin
      LReg.OpenKey('Software\HZB_Helium\' + AKeyName,false);
      if LReg.ValueExists(AName) then Result := LReg.ReadFloat(AName);
    end;
  finally
    LReg.CloseKey;
    LReg.Free;
  end;
end;

function getRegBoolean(AKeyName: string; AName: string): boolean;
var LReg: TRegistry;
begin
  Result := true;
  LReg := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  with LReg do
  try
    if KeyExists('Software\HZB_Helium\' + AKeyName) then
    begin
      OpenKey('Software\HZB_Helium\' + AKeyName,false);
      if ValueExists(AName) then Result := ReadBool(AName);
    end;
  finally
    LReg.CloseKey;
  end;
  LReg.Free;
end;

procedure deleteKey(APath: string);
var LReg: TRegistry;
begin
  LReg := TRegistry.Create;
  LReg.RootKey := HKEY_CURRENT_USER;
  try
    if LReg.KeyExists(APath) then LReg.DeleteKey(APath);
  finally
    LReg.Free;
  end;
end;



{ - - - - - - - - - - - - - - - - - - - - - - - - - }
{ - - - - - C R Y P T O - F U N C T I O N S - - - - }
{ - - - - - - - - - - - - - - - - - - - - - - - - - }
// encrypt string
function encrypt(AChars: string): string;
var i,l:integer;
    s: string;
begin
  randomize;
  s := chr(random(255));
  s := s + chr(random(255));
  for i := 1 to length(AChars) do
  begin
    l := ord(AChars[i]) + 77;
    if l > 255 then l := l - 255;
    s := s + chr(l);
  end;
  s := s + chr(random(255));
  s := s + chr(random(255));
  s := s + chr(random(255));
  Result := s;
end;

// decrypt cipher
function decrypt(AChars: string): string;
var i,l: integer;
    s: string;
begin
  Result := '';
  if (length(AChars) > 5) then
  begin
    s:= '';
    for i := 3 to length(AChars) - 3 do
    begin
      l := ord(AChars[i]) - 77;
      if (l < 0) then l := 255 + l;
      s := s + chr(l);
    end;
    Result := s;
  end;
end;



{ - - - - - - - - - - - - - - - - - - - - - - - - - }
{ - - - - - - - - C O M - P O R T - - - - - - - - - }
{ - - - - - - - - - - - - - - - - - - - - - - - - - }

// list installed com ports on local computer
function getPortList: TStringList;
begin
  Result := TStringList.Create;
  try     EnumComPorts(Result);
  finally
  end;
end;

// check whether APort exists
function portExists(APort: string): boolean;
var LPorts: TStringList;
begin
  LPorts := TStringList.Create;
  try     EnumComPorts(LPorts);
  finally Result := (LPorts.IndexOf(APort) > -1);
  end;    LPorts.Free;
end;

// create TComPort object
function createComPort(APort: string): TComPort;
begin
  Result := nil;
  if portExists(APort) then
  begin
    Result          := TComPort.Create(nil);
    Result.BaudRate := br9600;
    Result.DataBits := dbEight;
    Result.Stopbits := sbOneStopBit;
    Result.Port     := APort;
  end;
end;

// open TComPort
function openComPort(APort: TComPort): boolean;
begin
  try
    try
      APort.Open;
    except
    end;
  finally
    Result := APort.Connected;
  end;
end;

end.
