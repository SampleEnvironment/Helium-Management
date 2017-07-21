unit EvaluateDb;

interface

uses
  Classes, SysUtils, Contnrs, SQLClientInterfaces;

type
  TInstConsumption = class(Tobject)
    inst_name:  string;
    inst_days:  byte;
    inst_month: array of double;
  public
    constructor CreateWithProperties(AInstrument: string; ADays: byte);
  end;

  TVesConsumption = class(Tobject)
    inst_name:  word;
    inst_days:  byte;
    inst_month: array of double;
  public
    constructor CreateWithProperties(AVessel: word; ADays: byte);
  end;

  TMeaVessel = class(TObject)
    ves_id:     integer;
    ves_name:   word;
    ves_volume: double;
  end;

  TMeasurement = class(TObject)
    mea_position: string;
    mea_he_level: double;
    mea_pressure: word;
    mea_date:     TDateTime;
    mea_status:   byte;
  end;

  TIlmMeasurement = class(TMeasurement)
    mea_ch1_level: double;
    mea_ch2_level: double;
    mea_ch3_level: double;
  end;

  TEvaluateDbThread = class(TThread)
  private
    FDbClient:    ISQLClient;
    FVesList:    TObjectList;
    FMeaList:    TObjectList;
                          
    procedure SQLError(const AObj: ISQLImplementor; const AMessage: string);
    procedure AnalyzeAndPurgeDb;
    procedure EvaluationPeriod(var AYear, AYearNow: word; var AMonth, AMonthNow: byte);
    procedure GetVesselList;
    procedure GetMeasurements(AVesselId: integer; AYear: word; AMonth: byte);
    procedure EvaluateMonth(AYear: word; AMonth: byte);
    procedure GenerateConsumptionFile(AYear: word; AMonth: byte; AInstList: TObjectList);
    procedure GenerateExhaustSteamFile(AYear: word; AMonth: byte; AVesList: TObjectList);

    function  ConsumInLiter(AVessel: TMeaVessel; AConsumInProcent: double): double;
  protected
    procedure Execute; override;
  public
    constructor CreateWithProperties();
    destructor  Destroy; override;
  published
  end;

implementation

uses
  HeliumFunctions, DateUtils, dialogs, HeliumDataMgmt;

constructor TInstConsumption.CreateWithProperties(AInstrument: string; ADays: byte);
var i: integer;
begin
  inherited Create;

  inst_name := AInstrument;
  inst_days := ADays;
  setlength(inst_month, ADays);
  for i := 0 to high(inst_month) do inst_month[i] := 0;    
end;

constructor TVesConsumption.CreateWithProperties(AVessel: Word; ADays: Byte);
var i: integer;
begin
  inherited Create;

  inst_name := AVessel;
  inst_days := ADays;
  setlength(inst_month, ADays);
  for i := 0 to high(inst_month) do inst_month[i] := 0;
end;


constructor TEvaluateDbThread.CreateWithProperties();
begin
  inherited Create(true);
  FreeOnTerminate := true;
  FVesList        := TObjectList.Create(true);
  FMeaList        := TObjectList.Create(true);

  Resume;
end;

destructor TEvaluateDbThread.Destroy;
begin

  FDbClient.GetSession.Disconnect;

  FVesList.Free;
  FMeaList.Free;
  FDbClient := nil;

  inherited Destroy;
end;

procedure TEvaluateDbThread.Execute;
begin
  FDbClient         := NewSQLClient;
  FDbClient.GetSession.InitSession(getRegString('','DbServer'),
                                   getRegString('','DbUser'),
                                   decrypt(getRegString('','DbKey')));

  if FDbClient.GetSession.Connect then
  begin
    AnalyzeAndPurgeDb();
    FDbClient.GetSession.Disconnect;
  end
  else
    SQLError(FDbClient, Format(EConnectSession, [FDbClient.GetSession.GetDataSource, FDbCLient.GetSession.GetUsername]));
end;

procedure TEvaluateDbThread.AnalyzeAndPurgeDb;
var i, j: integer;
    LYear,  LYearNow:  word;
    LMonth, LMonthNow, LMonthFirst, LMonthLast: byte;
begin
  EvaluationPeriod(LYear, LYearNow, LMonth, LMonthNow);

  GetVesselList;

  for i := LYear to LYearNow do
  begin
    if (i = LYear)
     then LMonthFirst := LMonth
     else LMonthFirst := 1;
    if (i = LYearNow)
     then LMonthLast := LMonthNow
     else LMonthLast := 12;

    for j := LMonthFirst to LMonthLast do EvaluateMonth(i, j);
  end;
end;

procedure TEvaluateDbThread.EvaluationPeriod(var AYear, AYearNow: word; var AMonth, AMonthNow: byte);
var s: string;
begin
  s         := dateToStr(Date);
  AYearNow  :=strToInt(copy(s,7,4));
  AMonthNow := strToInt(copy(s,4,2));
  AYear     := AYearNow;
  AMonth    := AMonthNow;

  if FDbClient.GetSession.Connected then
  try
    setDbQuery(FDbClient, 'SELECT MIN(MEA_DATE) FROM ' + db_t_measurement);

    if FDbClient.Open then
    begin
      if FDbClient.Active then
      begin
        s := FDbClient.FieldByName('MIN(MEA_DATE)').AsString;

        if (length(s) > 9) then
        begin
          AYear  := strToInt(copy(s,7,4));
          AMonth := strToInt(copy(s,4,2));
        end;
      end;
    end
    else
      SQLError(FDbClient, EOpenClient);
  finally
    FDbClient.Close;
  end;
end;

procedure TEvaluateDbThread.GetVesselList;
var LVes: TMeaVessel;
begin
  if FDbClient.GetSession.Connected then
  try
    setDbQuery(FDbClient, 'SELECT V_ID, V_NAME, T_VOLUME FROM ' + db_t_type
                         + ' RIGHT OUTER JOIN ' + db_t_hen2_vessel
                         + ' ON V_T_ID = T_ID'
                         + ' WHERE T_SUBSTANCE = ' + #39 + 'He' + #39
                         + ' ORDER BY V_NAME');

    if FDbClient.Open then
    begin
      if FDbClient.Active then
      with FDbClient do
        while not Eof do
        begin
          LVes := TMeaVessel.Create;
          LVes.ves_id     := FieldByName('V_ID').AsInteger;
          LVes.ves_name   := FieldByName('V_NAME').AsInteger;
          LVes.ves_volume := FieldByName('T_VOLUME').AsFloat;

          FVesList.Add(LVes);
          Next;
        end;
    end
    else
      SQLError(FDbClient, EOpenClient);
  finally
    FDbClient.Close;
  end;
end;

procedure TEvaluateDbThread.SQLError(const AObj: ISQLImplementor;
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

  raise Exception.Create(lMsg);
end;

procedure TEvaluateDbThread.GetMeasurements(AVesselId: integer; AYear: word; AMonth: byte);
var s:    string;
    LMea: TMeasurement;
begin
  FMeaList.Clear;

  if FDbClient.GetSession.Connected then
  try
    s := 'SELECT MEA_POSITION, MEA_HE_LEVEL, MEA_PRESSURE, MEA_DATE, MEA_STATUS FROM '
         + db_t_measurement
         + ' WHERE '
         + #39 + '1.' + intToStr(AMonth) + '.' + intToStr(AYear) + #39
         + ' <= MEA_DATE AND MEA_DATE < ';
    if (AMonth = 12)
     then s := s + #39 + '1.1.' + intToStr(AYear + 1) + #39
     else s := s + #39 + '1.' + intToStr(AMonth + 1) + '.' + intToStr(AYear) + #39;
    s := s + ' AND MEA_VES_ID = ' + intToSTr(AVesselId)
         + ' ORDER BY MEA_DATE';

    setDbQuery(FDbClient, s);

    if FDbClient.Open then
    begin
      if FDbClient.Active then
      with FDbClient do
        while not Eof do
        begin
          LMea := TMeasurement.Create;
          LMea.mea_position := FieldByName('MEA_POSITION').AsString;
          LMea.mea_he_level := FieldByName('MEA_HE_LEVEL').AsFloat;
          LMea.mea_pressure := FieldByName('MEA_PRESSURE').AsInteger;
          LMea.mea_date     := FieldByName('MEA_DATE').AsDateTime;
          LMea.mea_status   := FieldByName('MEA_STATUS').AsInteger;
          FMeaList.Add(LMea);
          Next;
        end;
    end
    else
      SQLError(FDbClient, EOpenClient);
  finally
    FDbClient.Close;
  end;
end;

procedure TEvaluateDbThread.EvaluateMonth(AYear: word; AMonth: byte);
var i, j:         integer;
    LDays:        byte;
    LDayIndex:    byte;
    LMea1, LMea2: TMeasurement;
    LHeliumDiff:  double;
    LInstruments: TObjectList;
    LVessels:     TObjectList;
    LInstrument:  TInstConsumption;
    LVessel:      TVesConsumption;

  function GetInstrument(AInstName: string): TInstConsumption;
  var k: integer;
  begin
    Result := nil;
    for k := 0 to LInstruments.Count - 1 do
      if (TInstConsumption(LInstruments[k]).inst_name = AInstName) then
      begin
        Result := TInstConsumption(LInstruments[k]);
        break;
      end;

    if not Assigned(Result) then
    begin
      Result := TInstConsumption.CreateWithProperties(AInstName, LDays);
      LInstruments.Add(Result);
    end;
  end;

begin
  LInstruments := TObjectList.Create(true);
  LVessels     := TObjectList.Create(true);
  LDays        := DaysInAMonth(AYear, AMonth);

  for i := 0 to FVesList.Count - 1 do
  begin
    GetMeasurements(TMeaVessel(FVesList[i]).ves_id, AYear, AMonth);

    if (FMeaList.Count > 1) then
    try
      LVessel := TVesConsumption.CreateWithProperties(TMeaVessel(FVesList[i]).ves_name, LDays);
      LVessels.Add(LVessel);

      for j := 1 to FMeaList.Count - 1 do
      begin
        LMea1 := TMeasurement(FMeaList[j-1]);
        LMea2 := TMeasurement(FMeaList[j]);

        if not (LMea2.mea_status = 1) then
        begin
          LHeliumDiff := LMea1.mea_he_level - LMea2.mea_he_level;
    
          // Filling
          if ((LMea1.mea_status in [3, 4]) or (LMea2.mea_status in [4, 5])) then
          begin
            if (LMea1.mea_status in [3, 4])
             then LInstrument := GetInstrument(LMea1.mea_position)
             else LInstrument := GetInstrument(LMea2.mea_position);
            LDayIndex := strToInt(copy(dateToStr(LMea1.mea_date),1,2)) - 1;
            LInstrument.inst_month[LDayIndex] := LInstrument.inst_month[LDayIndex] + ConsumInLiter(TMeaVessel(FVesList[i]), LHeliumDiff);
          end

          // Exhaust steam
          else begin
            LDayIndex := strToInt(copy(dateToStr(LMea1.mea_date),1,2)) - 1;
            LVessel.inst_month[LDayIndex] := LVessel.inst_month[LDayIndex] + LHeliumDiff;
          end;
        end;
      end;
    finally
    end;
  end;

  GenerateConsumptionFile(AYear, AMonth, LInstruments);
  GenerateExhaustSteamFile(AYear, AMonth, LVessels);

  LInstruments.Free;
  LVessels.Free;
end;

procedure TEvaluateDbThread.GenerateConsumptionFile(AYear: word; AMonth: byte; AInstList: TObjectList);
var LPath,s:     string;
    LFile:       TextFile;
    i, j:        integer;
    LInstrument: TInstConsumption;
begin
  try
    LPath := FOLDER_REPORT + 'CONSUMPTION_' + intToStr(AYear) + '_' + intToStr(AMonth) + '.csv';
    AssignFile(LFile, LPath);
    Rewrite(LFile);

    WriteLn(LFile, 'Helium Consumption in ' + intToStr(AMonth) + '.' + intToStr(AYear));
    WriteLn(LFile, '');
    WriteLn(LFile, '');
    WriteLn(LFile, 'INSTRUMENT;DATE;');

    for i := 0 to AInstList.Count - 1 do
    begin
      LInstrument := TInstConsumption(AInstList[i]);

      if (i=0) then
      begin
        s := ';';
        for j := 0 to high(LInstrument.inst_month)
         do s := s + intToStr(j+1) + '.;';
        WriteLn(LFile, s);
        WriteLn(LFile, '');
      end;

      s := LInstrument.inst_name + ';';
      for j := 0 to high(LInstrument.inst_month)
       do s := s + floatToStr(LInstrument.inst_month[j]) + ';';
      WriteLn(LFile, s);            
    end;
  finally
    closeFile(LFile);
  end;
end;

procedure TEvaluateDbThread.GenerateExhaustSteamFile(AYear: Word; AMonth: Byte; AVesList: TObjectList);
var LVessel:       TVesConsumption;
    LFile:         TextFile;
    min, max, sum: double;
    i, j:          integer;
    LPath, s:      string;
begin
  try
    LPath := FOLDER_REPORT + 'EXHAUSTSTEAM_' + intToStr(AYear) + '_' + intToStr(AMonth) + '.csv';
    AssignFile(LFile, LPath);
    Rewrite(LFile);

    WriteLn(LFile, 'Helium Exhaust steam in ' + intToStr(AMonth) + '.' + intToStr(AYear));
    WriteLn(LFile, '');
    WriteLn(LFile, '');
    WriteLn(LFile, 'Vessel;' + #216 + ' (%/day);min (%/day);max (%/day);');
    WriteLn(LFile, '');

    for i := 0 to AVesList.Count - 1 do
    begin
      LVessel := TVesConsumption(AVesList[i]);
      s   := 'Vessel ' + intToStr(LVessel.inst_name) + ';';
      min := LVessel.inst_month[0];
      max := LVessel.inst_month[0];
      sum := 0;
      for j := 0 to high(LVessel.inst_month) do
      begin
        sum := sum + LVessel.inst_month[j];
        if (LVessel.inst_month[j] < min) then min := LVessel.inst_month[j];
        if (LVessel.inst_month[j] > max) then max := LVessel.inst_month[j];
      end;
      s := s + floatToSTr(sum/(high(LVessel.inst_month)+1)) + ';' + floatToSTr(min) + ';' + floatToSTr(max) + ';';
      WriteLn(LFile, s);
    end;
  finally
    closeFile(LFile);
  end;

end;

function TEvaluateDbThread.ConsumInLiter(AVessel: TMeaVessel; AConsumInProcent: double): double;
begin
  Result := AConsumInProcent * AVessel.ves_volume / 100;
end;

end.
