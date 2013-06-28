
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Odac.inc}
unit OracleUniProvider;
{$ENDIF}

interface

uses
  SysUtils, Classes, Variants, DB, CRAccess, CRConnectionPool, MemData,
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  DBAccess, MemUtils, CRParser, DAScript, DADump, UniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TOracleUniProvider = class(TUniProvider)
  public
    class function GetProviderName: string; override;

    function NeedRecreateProcCall: boolean; override;

    function GetParserClass: TSQLParserClass; override;
    function GetConnectionParametersClass: TCRConnectionParametersClass; override;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; override;
    function GetConnectionClass: TCRConnectionClass; override;
    function GetServerEnumeratorClass: TCRServerEnumeratorClass; override;    
    function GetTransactionClass: TCRTransactionClass; override;
    function GetCommandClass: TCRCommandClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetDataSetServiceClass: TDADataSetServiceClass; override;
    function GetScriptProcessorClass: TDAScriptProcessorClass; override;
    function GetLoaderClass: TCRLoaderClass; override;
    function GetAlerterClass: TCRAlerterClass; override;
    function GetDumpProcessorClass: TDADumpProcessorClass; override;
    function GetMetaDataClass: TCRMetaDataClass; override;
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetDataTypesMap: TDataTypesMapClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;
    function GetParamObjectClass(Param: TDAParam): TClass; override;
    function CreateParamObject(Param: TDAParam): TSharedObject; override;

    procedure SetObjectProps(Obj: TObject; Options: _TStrings; SetAllProps: boolean); override;

    function GetConnectionOptions: TOptionsList; override;
    function GetSQLOptions: TOptionsList; override;
    function GetDataSetOptions: TOptionsList; override;
    function GetScriptOptions: TOptionsList; override;
    function GetLoaderOptions: TOptionsList; override;
  end;

  TOraConnectDialogService = class (TConnectDialogService)
  private
    FDirect: boolean;
  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetConnectMode: integer; override;
  end;

  TOraSqlFormatter = class(TUniSqlFormatter)
  protected
    function GetFunction(const FunctionName: _string; const Params: _TStringArray): _string; override;
    function GetDateAdd(const Params: _TStringArray): _string;
    function GetDateDiff(const Params: _TStringArray): _string;
  public
    constructor Create; override;
  end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}

implementation

uses
  TypInfo, DAConsts, UniConsts,
{$IFNDEF UNIDACPRO}
  OraParser, OraConnectionPool, OraClasses, OraServices, OraCall,
  OraScriptProcessor;
{$ELSE}
  OraParserUni, OraConnectionPoolUni, OraClassesUni, OraServicesUni, OraCallUni,
  OraScriptProcessorUni;
{$ENDIF}

var
  OraFunctions, OraMacros: _TStringList;

{ TOracleUniProvider }

class function TOracleUniProvider.GetProviderName: string;
begin
  Result := 'Oracle';
end;

function TOracleUniProvider.NeedRecreateProcCall: boolean;
begin
  Result := True;
end;

function TOracleUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TOraParser;
end;

function TOracleUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TOraConnectionParameters;
end;

function TOracleUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TOraConnectionPoolManager;
end;

function TOracleUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TOCIConnection;
end;

function TOracleUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TOraServerEnumerator;
end;

function TOracleUniProvider.GetTransactionClass: TCRTransactionClass;
begin
  Result := TOCITransaction;
end;

function TOracleUniProvider.GetCommandClass: TCRCommandClass;
begin
  Result := TOCICommand;
end;

function TOracleUniProvider.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TOCIRecordSet;
end;

function TOracleUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomOraDataSetService;
end;

function TOracleUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TOraScriptProcessor;
end;

function TOracleUniProvider.GetLoaderClass: TCRLoaderClass;
begin
  Result := TOCILoader;
end;

function TOracleUniProvider.GetAlerterClass: TCRAlerterClass;
begin
  Result := TOCIAlerter;
end;

function TOracleUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomOraDumpProcessor;
end;

function TOracleUniProvider.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := TCustomOraDataTypesMap;
end;

function TOracleUniProvider.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TOCIMetaData;
end;

function TOracleUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TOraConnectDialogService;
end;

function TOracleUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TOraSqlFormatter;
end;

function TOracleUniProvider.GetParamObjectClass(Param: TDAParam): TClass;
begin
  case Param.DataType of
    ftOraClob, ftOraBlob:
      Result := TOraLob;
    ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}:
      Result := TCompressedBlob;
    ftCursor:
      Result := TOraCursor;
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

function TOracleUniProvider.CreateParamObject(Param: TDAParam): TSharedObject;
begin
  case Param.DataType of
    ftOraClob, ftOraBlob: begin
      Result := TOraLob.Create(nil);
      if Param.DataType = ftOraClob then begin
        if TDBAccessUtils.GetNational(Param) then
          TOraLob(Result).LobType := ltNClob
        else
          TOraLob(Result).LobType := ltClob;
        if OCIUnicode then
          TOraLob(Result).IsUnicode := True;
      end
      else
        TOraLob(Result).LobType := ltBlob;
    end;
    ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}: begin
      Result := TCompressedBlob.Create;
    {$IFDEF VER10P}
      if Param.DataType = ftWideMemo then
        TBlob(Result).IsUnicode := True;
    {$ENDIF}
    end;
    ftCursor:
      Result := TOraCursor.Create;
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

procedure TOracleUniProvider.SetObjectProps(Obj: TObject; Options: _TStrings; SetAllProps: boolean);
begin
  if Obj.ClassType = TOCIConnection then begin
  {$IFNDEF FPC}
    TOCIConnection(Obj).SetProp(prEnableSQLTimeStamp, True);
  {$ELSE}
    TOCIConnection(Obj).SetProp(prTimeStampAsString, True);
  {$ENDIF}
    TOCIConnection(Obj).SetProp(prIntervalAsString, True);
  end
  else
  if Obj.ClassType = TOCICommand then begin
    TOCICommand(Obj).SetProp(prCheckParamHasDefault, False);
    TOCICommand(Obj).SetProp(prUseResultParams, True);
  end;

  inherited;
end;

function TOracleUniProvider.GetConnectionOptions: TOptionsList;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('CharLength', prCharLength, [TOCIConnection], 1));
    FConnectionOptions.Add(TStringOption.Create('Charset', prCharset, [TOCIConnection], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TOCIConnection, TOraConnectionParameters], {$IFNDEF UNICODE_BUILD}False{$ELSE}True{$ENDIF}));
    FConnectionOptions.Add(TStringOption.Create('DateFormat', prDateFormat, [TOCIConnection], ''));
    FConnectionOptions.Add(TStringOption.Create('DateLanguage', prDateLanguage, [TOCIConnection], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseOCI7', prUseOCI7, [TOCIConnection, TOraConnectionParameters], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('OptimizerMode', prOptimizerMode, [TOCIConnection, TOraConnectionParameters], Variant(omDefault), TypeInfo(TOptimizerMode)));
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeOut, [TOCIConnection, TOraConnectionParameters], 0));
    FConnectionOptions.Add(TBooleanOption.Create('StatementCache', prStatementCache, [TOCIConnection, TOraConnectionParameters], False));
    FConnectionOptions.Add(TIntegerOption.Create('StatementCacheSize', prStatementCacheSize, [TOCIConnection, TOraConnectionParameters], 20));
    FConnectionOptions.Add(TStringOption.Create('ClientIdentifier', prClientIdentifier, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('Direct', prDirect, [TOCIConnection, TOraConnectionParameters, TOraServerEnumerator, TOraConnectDialogService], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('ConnectMode', prConnectMode, [TOCIConnection, TOraConnectionParameters], Variant(cmNormal), TypeInfo(TConnectMode)));
    FConnectionOptions.Add(TBooleanOption.Create('ThreadSafety', prThreadSafety, [TOCIConnection], True));
    FConnectionOptions.Add(TStringOption.Create('HomeName', prHomeName, [TOCIConnection, TOraConnectionParameters, TOraServerEnumerator], ''));
    FConnectionOptions.Add(TStringOption.Create('Schema', prSchema, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TEnumeratorOption.Create('PoolingType', prPoolingType, [TOraConnectionParameters], Variant(optLocal), TypeInfo(TOraPoolingType)));
    FConnectionOptions.Add(TIntegerOption.Create('PrecisionSmallint', prSmallintPrecision, [TOCIConnection], 4));
    FConnectionOptions.Add(TIntegerOption.Create('PrecisionInteger', prIntegerPrecision, [TOCIConnection], 9));
    FConnectionOptions.Add(TIntegerOption.Create('PrecisionLargeint', prLargeintPrecision, [TOCIConnection], 18));
    FConnectionOptions.Add(TIntegerOption.Create('PrecisionFloat', prFloatPrecision, [TOCIConnection], 0));
    FConnectionOptions.Add(TStringOption.Create('PrecisionBCD', prBCDPrecision, [TOCIConnection], '14,4'));
  {$IFNDEF FPC}
    FConnectionOptions.Add(TStringOption.Create('PrecisionFMTBCD', prFMTBCDPrecision, [TOCIConnection], '38,38'));
  {$ENDIF}
  end;
  Result := FConnectionOptions;
end;

function TOracleUniProvider.GetSQLOptions: TOptionsList;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := ToptionsList.Create(GetProviderName);
    FSQLOptions.Add(TBooleanOption.Create('StatementCache', prStatementCache, [TOCICommand], False));
    FSQLOptions.Add(TBooleanOption.Create('TemporaryLobUpdate', prTemporaryLobUpdate, [TOCICommand], True));
  end;
  Result := FSQLOptions;
end;

function TOracleUniProvider.GetDataSetOptions: TOptionsList;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TOCIRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('AutoClose', prAutoClose, [TOCIRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('FieldsAsString', prFieldsAsString, [TOCICommand], False));
    FDataSetOptions.Add(TBooleanOption.Create('DeferredLobRead', prDeferredLobRead, [TOCIRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('CacheLobs', prCacheLobs, [TOCICommand], True));
    FDataSetOptions.Add(TBooleanOption.Create('ScrollableCursor', prScrollableCursor, [TOCICommand], False));
  {$IFNDEF FPC}
    FDataSetOptions.Add(TBooleanOption.Create('RawAsString', prRawAsString, [TOCICommand], False));
  {$ENDIF}
    FDataSetOptions.Add(TBooleanOption.Create('TemporaryLobUpdate', prTemporaryLobUpdate, [TOCICommand], True));
    FDataSetOptions.Add(TBooleanOption.Create('StatementCache', prStatementCache, [TOCICommand], False));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TOCIRecordSet], False));
    FDataSetOptions.Add(TStringOption.Create('KeySequence', prKeySequence, [TCustomOraDataSetService], ''));
    FDataSetOptions.Add(TEnumeratorOption.Create('SequenceMode', prSequenceMode, [TCustomOraDataSetService], Variant(_smPost), TypeInfo(_TSequenceMode)));
  end;
  Result := FDataSetOptions;
end;

function TOracleUniProvider.GetScriptOptions: TOptionsList;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
  end;
  Result := FScriptOptions;
end;

function TOracleUniProvider.GetLoaderOptions: TOptionsList;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
    FLoaderOptions.Add(TBooleanOption.Create('DirectPath', prDirectPath, [TOCILoader], True));
  end;
  Result := FLoaderOptions;
end;

{ TOraConnectDialogService }

function TOraConnectDialogService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDirect:
      FDirect := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOraConnectDialogService.GetConnectMode: integer;
begin
  if FDirect then
    Result := 2
  else
    Result := 1;
end;

{ TOraSqlFormatter }

constructor TOraSqlFormatter.Create;
begin
  inherited;

  FFunctions := OraFunctions;
  FPredefinedMacros := OraMacros;
end;

function TOraSqlFormatter.GetFunction(const FunctionName: _string; const Params: _TStringArray): _string;
begin
  if _UpperCase(FunctionName) = 'DATEADD' then
    Result := GetDateAdd(Params)
  else
  if _UpperCase(FunctionName) = 'DATEDIFF' then
    Result := GetDateDiff(Params)
  else
    Result := inherited GetFunction(FunctionName, Params);
end;

function TOraSqlFormatter.GetDateAdd(const Params: _TStringArray): _string;
var
  datepart, number, date: string;
begin
  if Length(Params) <> 3 then
    raise Exception.CreateFmt(SWrongArgCnt, ['DATEADD']);
  datepart := LowerCase(Params[0]);
  number := Params[1];
  date := Params[2];
  if datepart = 'year' then
    Result := ' ADD_MONTHS(' + date + ', (' + number + ')*12)'
  else
  if datepart = 'month' then
    Result := ' ADD_MONTHS(' + date + ', ' + number + ')'
  else
  if datepart = 'day' then
    Result := date + ' + ' + number
  else
  if datepart = 'hour' then
    Result := date + ' + (' + number + ')/24'
  else
  if datepart = 'minute' then
    Result := date + ' + (' + number + ')/1440'
  else
  if datepart = 'second' then
    Result := date + ' + (' + number + ')/86400'
  else
  if datepart = 'millisecond' then
    Result := date + ' + (' + number + ')/86400000'
  else
    raise Exception.CreateFmt(SUnknownDatepart, [Datepart]);
end;

function TOraSqlFormatter.GetDateDiff(const Params: _TStringArray): _string;
const
  monthFmt = '(EXTRACT(YEAR FROM %1:s) - EXTRACT(YEAR FROM %0:s))*12 + EXTRACT(MONTH FROM %1:s) - EXTRACT(MONTH FROM %0:s)';
var
  datepart, startDate, endDate: string;
begin
  if Length(Params) <> 3 then
    raise Exception.CreateFmt(SWrongArgCnt, ['DATEDIFF']);
  datepart := LowerCase(Params[0]);
  startDate := Params[1];
  endDate := Params[2];
  if datepart = 'year' then
    Result := Format('FLOOR((' + monthFmt + ')/12)', [startDate, endDate])
  else
  if datepart = 'month' then
    Result := Format(monthFmt, [startDate, endDate])
  else
  if datepart = 'day' then
    Result := 'FLOOR(' + endDate + ' - ' + startDate + ')'
  else
  if datepart = 'hour' then
    Result := '(' + endDate + ' - ' + startDate + ')*24'
  else
  if datepart = 'minute' then
    Result := '(' + endDate + ' - ' + startDate + ')*1440'
  else
  if datepart = 'second' then
    Result := '(' + endDate + ' - ' + startDate + ')*86400'
  else
  if datepart = 'millisecond' then
    Result := '(' + endDate + ' - ' + startDate + ')*86400000'
  else
    raise Exception.CreateFmt(SUnknownDatepart, [Datepart]);
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TOracleUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TOracleUniProvider);

  OraFunctions := _TStringList.Create;
  OraFunctions.AddObject('USER', TObject(PChar('USER')));
  OraFunctions.AddObject('CHAR_LENGTH', TObject(PChar('LENGTH(%s)')));
  OraFunctions.AddObject('LOCATE', TObject(PChar('INSTR(%1:s, %0:s)')));
  OraFunctions.AddObject('SUBSTRING', TObject(PChar('SUBSTR(%s, %s, %s)')));
  OraFunctions.AddObject('CONCAT', TObject(PChar('%s || %s')));
  OraFunctions.AddObject('CHAR', TObject(PChar('CHR(%s)')));
  OraFunctions.AddObject('TRIM', TObject(PChar('TRIM(%s)')));
  OraFunctions.AddObject('TRUNCATE', TObject(PChar('TRUNC(%s, %s)')));
  OraFunctions.AddObject('CEILING', TObject(PChar('CEIL(%s)')));
  // Date-time
  OraFunctions.AddObject('CURRENT_DATE', TObject(PChar('CURRENT_DATE')));
  OraFunctions.AddObject('YEAR', TObject(PChar('EXTRACT(YEAR FROM %s)')));
  OraFunctions.AddObject('MONTH', TObject(PChar('EXTRACT(MONTH FROM %s)')));
  OraFunctions.AddObject('DAY', TObject(PChar('EXTRACT(DAY FROM %s)')));
  // Date-time literals
  OraFunctions.AddObject('__DATE_TIME_LITERAL', TObject(PChar('TO_DATE(%s, ''yyyy-mm-dd hh:mi:ss'')')));
  OraFunctions.AddObject('__DATE_LITERAL', TObject(PChar('TO_DATE(%s, ''yyyy-mm-dd'')')));
  OraFunctions.AddObject('__TIME_LITERAL', TObject(PChar('TO_DATE(%s, ''hh:mi:ss'')')));
  // CONVERT functions
  OraFunctions.AddObject('TODATE', TObject(PChar('TO_DATE(%s, ''yyyy-mm-dd hh:mi:ss'')')));
  OraFunctions.AddObject('TONUMBER', TObject(PChar('TO_NUMBER(%s)')));
  OraFunctions.AddObject('TOCHAR', TObject(PChar('TO_CHAR(%s)')));

  OraMacros := _TStringList.Create;
  OraMacros.AddObject('PROVIDER', TObject(PChar('Oracle')));
  OraMacros.AddObject('ORACLE', TObject(PChar('')));
  // DataType macros
  OraMacros.AddObject('DATETIME', TObject(PChar('DATE')));
  OraMacros.AddObject('DOUBLE', TObject(PChar('NUMBER')));
  OraMacros.AddObject('VARCHAR', TObject(PChar('VARCHAR2')));

finalization
  UniProviders.UnRegisterProvider(TOracleUniProvider);

  OraFunctions.Free;
  OraMacros.Free;

end.
