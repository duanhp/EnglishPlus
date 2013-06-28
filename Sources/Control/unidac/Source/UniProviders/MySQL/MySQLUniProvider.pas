
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I MyDac.inc}

unit MySQLUniProvider;
{$ENDIF}

interface

uses
  SysUtils, Classes, Variants, DB, MemData, CRAccess, CRConnectionPool, DBAccess,
  {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF}, 
  UniProvider, DAScript, DADump, MemUtils, CRParser;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMySQLUniProvider = class(TUniProvider)
  public
    class function GetProviderName: string; override;

    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;

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
    function GetDumpProcessorClass: TDADumpProcessorClass; override;
    function GetMetaDataClass: TCRMetaDataClass; override;
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetDataTypesMap: TDataTypesMapClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;

    procedure SetObjectProps(Obj: TObject; Options: _TStrings; SetAllProps: boolean); override;

    function GetConnectionOptions: TOptionsList; override;
    function GetSQLOptions: TOptionsList; override;
    function GetDataSetOptions: TOptionsList; override;
    function GetScriptOptions: TOptionsList; override;
    function GetLoaderOptions: TOptionsList; override;
    function GetDumpOptions: TOptionsList; override;
  end;

  TMyConnectDialogService = class (TConnectDialogService)
  private
    FEmbedded: boolean;
  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function ServerEnabled: boolean; override;
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TMySqlFormatter = class(TUniSqlFormatter)
  protected
    function LeftQuote: _char; override;
    function RightQuote: _char; override;
    function GetFunction(const FunctionName: _string; const Params: _TStringArray): _string; override;
    function GetDateDiff(const Params: _TStringArray): _string;
  public
    constructor Create; override;
  end;

  function GetConnectionThreadID(Connection: TCustomDAConnection): integer;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}

var
  MyFunctions: _TStringList;

implementation

uses
  DAConsts, UniConsts,
{$IFNDEF UNIDACPRO}
  MyServices, MyClasses, MyConnectionPool, MyScriptProcessor, MyParser;
{$ELSE}
  MyServicesUni, MyClassesUni, MyConnectionPoolUni, MyScriptProcessorUni, MyParserUni;
{$ENDIF}

var
  MyMacros: _TStringList;

{ TMySQLUniProvider }

class function TMySQLUniProvider.GetProviderName: string;
begin
  Result := 'MySQL';
end;

function TMySQLUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TMySQLUniProvider.IsPortSupported: boolean;
begin
  Result := True;
end;

function TMySQLUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TMyParser;
end;

function TMySQLUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TMyConnectionParameters;
end;

function TMySQLUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TMyConnectionPoolManager;
end;

function TMySQLUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TMySQLConnection;
end;

function TMySQLUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TMyServerEnumerator;
end;

function TMySQLUniProvider.GetTransactionClass: TCRTransactionClass;
begin
  Result := TMySQLTransaction;
end;

function TMySQLUniProvider.GetCommandClass: TCRCommandClass;
begin
  Result := TMySQLCommand;
end;

function TMySQLUniProvider.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TMySQLRecordset;
end;

function TMySQLUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomMyDataSetService;
end;

function TMySQLUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TMyScriptProcessor;
end;

function TMySQLUniProvider.GetLoaderClass: TCRLoaderClass;
begin
  Result := TMySQLLoader;
end;

function TMySQLUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomMyDumpProcessor;
end;

function TMySQLUniProvider.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TMySQLMetaData;
end;

function TMySQLUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TMyConnectDialogService;
end;

function TMySQLUniProvider.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := TCustomMyDataTypesMap;
end;

function TMySQLUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TMySqlFormatter;
end;

procedure TMySQLUniProvider.SetObjectProps(Obj: TObject; Options: _TStrings; SetAllProps: boolean);
begin
  if Obj is TMySQLConnection then begin
    TMySQLConnection(Obj).SetProp(prCheckPrecision, True);
  end
  else
  if Obj is TCustomMyDataSetService then begin
    TCustomMyDataSetService(Obj).SetProp(prAutoIncrementReadOnly, False);
  end;

  inherited;
end;

function TMySQLUniProvider.GetConnectionOptions: TOptionsList;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TMySQLConnection, TMyConnectionParameters], 15));
    FConnectionOptions.Add(TStringOption.Create('Charset', prCharset, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TMySQLConnection, TMyConnectionParameters], {$IFNDEF UNICODE_BUILD}False{$ELSE}True{$ENDIF}));
    FConnectionOptions.Add(TBooleanOption.Create('OptimizedBigint', prOptimizedBigint, [TMySQLConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('NullForZeroDelphiDate', prNullForZeroDelphiDate, [TMySQLConnection], False));

    FConnectionOptions.Add(TBooleanOption.Create('Compress', prCompress, [TMySQLConnection, TMyConnectionParameters], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('Protocol', prProtocol, [TMySQLConnection, TMyConnectionParameters], Variant(mpDefault), TypeInfo(TMyProtocol)));
    FConnectionOptions.Add(TBooleanOption.Create('Embedded', prEmbedded, [TMySQLConnection, TMyConnectionParameters, TMyConnectDialogService], False));
    FConnectionOptions.Add(TBooleanOption.Create('Direct', prDirect, [TMySQLConnection, TMyConnectionParameters], True));
    FConnectionOptions.Add(TStringOption.Create('EmbeddedParams', prEmbParams, [TMySQLConnection, TMyConnectionParameters], ''));

    // SSL
    FConnectionOptions.Add(TStringOption.Create('SSLChipherList', prSSL_Chipher, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLCACert', prSSL_CA, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLKey', prSSL_Key, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLCert', prSSL_Cert, [TMySQLConnection, TMyConnectionParameters], ''));
  end;
  Result := FConnectionOptions;
end;

function TMySQLUniProvider.GetSQLOptions: TOptionsList;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TMySQLCommand], 0));
  end;
  Result := FSQLOptions;
end;

function TMySQLUniProvider.GetDataSetOptions: TOptionsList;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TMySQLRecordSet], True));
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TMySQLRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('FieldsAsString', prFieldsAsString, [TMySQLRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('NullForZeroDate', prNullForZeroDate, [TMySQLRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('CheckRowVersion', prCheckRowVersion, [TCustomMyDataSetService], False));
    FDataSetOptions.Add(TBooleanOption.Create('EnableBoolean', prEnableBoolean, [TMySQLRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('CreateConnection', prCreateConnection, [TMySQLRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('BinaryAsString', prBinaryAsString, [TMySQLRecordSet], True));
  end;
  Result := FDataSetOptions;
end;

function TMySQLUniProvider.GetScriptOptions: TOptionsList;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
  end;
  Result := FScriptOptions;
end;

function TMySQLUniProvider.GetLoaderOptions: TOptionsList;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
    FLoaderOptions.Add(TBooleanOption.Create('LockTable', prLock, [TMySQLLoader], False));
    FLoaderOptions.Add(TBooleanOption.Create('Delayed', prDelayed, [TMySQLLoader], False));
    FLoaderOptions.Add(TIntegerOption.Create('RowsPerQuery', prRowsPerQuery, [TMySQLLoader], 0));
    FLoaderOptions.Add(TEnumeratorOption.Create('DuplicateKeys', prDuplicateKeys, [TMySQLLoader], Variant(_dkNone), TypeInfo(_TMyDuplicateKeys)));
  end;
  Result := FLoaderOptions;
end;

function TMySQLUniProvider.GetDumpOptions: TOptionsList;
begin
  if FDumpOptions = nil then begin
    FDumpOptions := TOptionsList.Create(GetProviderName);
    FDumpOptions.Add(TBooleanOption.Create('AddLock', prAddLock, [TCustomMyDumpProcessor], False));
    FDumpOptions.Add(TBooleanOption.Create('DisableKeys', prDisableKeys, [TCustomMyDumpProcessor], False));
    FDumpOptions.Add(TBooleanOption.Create('HexBlob', prHexBlob, [TCustomMyDumpProcessor], False));
    FDumpOptions.Add(TBooleanOption.Create('UseExtSyntax', prUseExtSyntax, [TCustomMyDumpProcessor], True));
    FDumpOptions.Add(TBooleanOption.Create('UseDelayedIns', prUseDelayedIns, [TCustomMyDumpProcessor], False));
    FDumpOptions.Add(TIntegerOption.Create('CommitBatchSize', prCommitBatchSize, [TCustomMyDumpProcessor], 0));
    FDumpOptions.Add(TEnumeratorOption.Create('InsertType', prInsertType, [TCustomMyDumpProcessor], Variant(_itInsert), TypeInfo(_TMyInsertType)));
    FDumpOptions.Add(TBooleanOption.Create('BackupTables', prBackupTables, [TCustomMyDumpProcessor], False));
    FDumpOptions.Add(TBooleanOption.Create('BackupViews', prBackupViews, [TCustomMyDumpProcessor], False));
  end;
  Result := FDumpOptions;
end;

{ TMyConnectDialogService }

function TMyConnectDialogService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prEmbedded:
      FEmbedded := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TMyConnectDialogService.ServerEnabled: boolean;
begin
  Result := not FEmbedded;
end;

function TMyConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := not FEmbedded;
end;

function TMyConnectDialogService.PortEnabled: boolean;
begin
  Result := not FEmbedded;
end;

{ TMySqlFormatter }

constructor TMySqlFormatter.Create;
begin
  inherited;

  FFunctions := MyFunctions;
  FPredefinedMacros := MyMacros;
end;

function TMySqlFormatter.LeftQuote: _char;
begin
  Result := '`';
end;

function TMySqlFormatter.RightQuote: _char;
begin
  Result := '`';
end;

function TMySqlFormatter.GetFunction(const FunctionName: _string; const Params: _TStringArray): _string;
begin
  if _UpperCase(FunctionName) = 'DATEDIFF' then
    Result := GetDateDiff(Params)
  else
    Result := inherited GetFunction(FunctionName, Params);
end;

function TMySqlFormatter.GetDateDiff(const Params: _TStringArray): _string;
const
  subFmt = 'PERIOD_DIFF(EXTRACT(YEAR_MONTH FROM %1:s), EXTRACT(YEAR_MONTH FROM %0:s))';
  secFmt = '(TIME_TO_SEC(%1:s) - TIME_TO_SEC(%0:s))';
var
  datepart, startDate, endDate: string;
begin
  if Length(Params) <> 3 then
    raise Exception.CreateFmt(SWrongArgCnt, ['DATEDIFF']);
  datepart := LowerCase(Params[0]);
  startDate := Params[1];
  endDate := Params[2];
  if datepart = 'year' then
    Result := Format('FLOOR((' + subFmt + ')/12)', [startDate, endDate])
  else
  if datepart = 'month' then
    Result := Format(subFmt, [startDate, endDate])
  else
  if datepart = 'day' then
    Result := 'TO_DAYS(' + endDate + ')' + ' - ' + ' TO_DAYS(' + startDate + ')'
  else
  if datepart = 'hour' then
    Result := '(TO_DAYS(' + endDate + ')' + ' - ' + ' TO_DAYS(' + startDate + '))*24 + ' +
      'TRUNCATE(' + Format(secFmt, [startDate, endDate]) + '/3600, 0)'
  else
  if datepart = 'minute' then
    Result :=  '(TO_DAYS(' + endDate + ')' + ' - ' + ' TO_DAYS(' + startDate + '))*1440 + ' +
      ' TRUNCATE(' + Format(secFmt, [startDate, endDate]) + '/60, 0)'
  else
  if datepart = 'second' then
    Result := '(TO_DAYS(' + endDate + ')' + ' - ' + ' TO_DAYS(' + startDate + '))*86400 + ' +
      Format(secFmt, [startDate, endDate])
  else
  if datepart = 'millisecond' then
    Result := '(TO_DAYS(' + endDate + ')' + ' - ' + ' TO_DAYS(' + startDate + '))*86400000 + ' +
      Format(secFmt, [startDate, endDate]) + '*1000)'
  else
    raise Exception.CreateFmt(SUnknownDatepart, [Datepart]);
end;

function GetConnectionThreadID(Connection: TCustomDAConnection): integer;
begin
  TDBAccessUtils.InternalConnect(Connection);
  try
    Result := (TDBAccessUtils.GetIConnection(Connection) as TMySQLConnection).GetThreadId;
  finally
    TDBAccessUtils.InternalDisconnect(Connection);
  end;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TMySQLUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TMySQLUniProvider);

  MyFunctions := _TStringList.Create;
  MyFunctions.AddObject('USER', TObject(PChar('USER()')));
  MyFunctions.AddObject('CHAR_LENGTH', TObject(PChar('CHAR_LENGTH(%s)')));
  MyFunctions.AddObject('LOCATE', TObject(PChar('LOCATE(%s, %s)')));
  MyFunctions.AddObject('SUBSTRING', TObject(PChar('SUBSTRING(%s, %s, %s)')));
  MyFunctions.AddObject('CONCAT', TObject(PChar('CONCAT(%s, %s)')));
  MyFunctions.AddObject('CHAR', TObject(PChar('CHAR(%s)')));
  MyFunctions.AddObject('TRIM', TObject(PChar('TRIM(%s)')));
  MyFunctions.AddObject('TRUNCATE', TObject(PChar('TRUNCATE(%s, %s)')));
  MyFunctions.AddObject('CEILING', TObject(PChar('CEILING(%s)')));
  // Date-time
  MyFunctions.AddObject('CURRENT_DATE', TObject(PChar('CURRENT_DATE')));
  MyFunctions.AddObject('YEAR', TObject(PChar('YEAR(%s)')));
  MyFunctions.AddObject('MONTH', TObject(PChar('MONTH(%s)')));
  MyFunctions.AddObject('DAY', TObject(PChar('EXTRACT(DAY FROM %s)')));
  MyFunctions.AddObject('DATEADD', TObject(PChar('(%2:s + INTERVAL ''%1:s'' %0:s)')));
  // Date-time literals
  MyFunctions.AddObject('__DATE_TIME_LITERAL', TObject(PChar('CAST(%s AS DATETIME)')));
  MyFunctions.AddObject('__DATE_LITERAL', TObject(PChar('CAST(%s AS DATE)')));
  MyFunctions.AddObject('__TIME_LITERAL', TObject(PChar('CAST(%s AS TIME)')));
  // CONVERT functions
  MyFunctions.AddObject('TODATE', TObject(PChar('CAST(%s AS DATETIME)')));
  MyFunctions.AddObject('TONUMBER', TObject(PChar('CAST(%s AS DOUBLE)')));
  MyFunctions.AddObject('TOCHAR', TObject(PChar('CAST(%s AS VARCHAR)')));

  MyMacros := _TStringList.Create;
  MyMacros.AddObject('PROVIDER', TObject(PChar('MySQL')));
  MyMacros.AddObject('MYSQL', TObject(PChar('')));
  // DataType macros
  MyMacros.AddObject('DATETIME', TObject(PChar('DATETIME')));
  MyMacros.AddObject('DOUBLE', TObject(PChar('DOUBLE')));
  MyMacros.AddObject('VARCHAR', TObject(PChar('VARCHAR')));

finalization
  UniProviders.UnRegisterProvider(TMySQLUniProvider);

  MyFunctions.Free;
  MyMacros.Free;

end.

