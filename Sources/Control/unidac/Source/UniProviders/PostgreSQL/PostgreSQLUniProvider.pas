
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I PgDac.inc}
unit PostgreSQLUniProvider;
{$ENDIF}

interface

uses
  SysUtils, Classes, Variants, DB, CRAccess, CRConnectionPool, MemData,
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess, MemUtils, CRParser,
  DAScript, DADump, Uni, UniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TPostgreSQLUniProvider = class(TUniProvider)
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

    // for Failover Demo
    class function GetSocket(Connection: TUniConnection): {$IFNDEF CLR}longint{$ELSE}IntPtr{$ENDIF};
  end;

  TPgConnectDialogService = class(TConnectDialogService)
  public
    function GetDefaultDatabase: _string; override;
  end;

  TPgSqlFormatter = class(TUniSqlFormatter)
  protected
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
  DAConsts, UniConsts, CRVio, CRVioTcp,
{$IFNDEF UNIDACPRO}
  PgParser, PgClasses, PgObjects, PgConnectionPool, PgServices, PgScriptProcessor,
  PgSQLNet;
{$ELSE}
  PgParserUni, PgClassesUni, PgObjectsUni, PgConnectionPoolUni, PgServicesUni,
  PgScriptProcessorUni, PgSQLNetUni;
{$ENDIF}  

var
  PgFunctions, PgMacros: _TStringList;

class function TPostgreSQLUniProvider.GetProviderName: string;
begin
  Result := 'PostgreSQL';
end;

function TPostgreSQLUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TPostgreSQLUniProvider.IsPortSupported: boolean;
begin
  Result := True;
end;

function TPostgreSQLUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TPgParser;
end;

function TPostgreSQLUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TPgConnectionParameters;
end;

function TPostgreSQLUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TPgConnectionPoolManager;
end;

function TPostgreSQLUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TPgSQLConnection;
end;

function TPostgreSQLUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;    
begin
  Result := TPgServerEnumerator;
end;

function TPostgreSQLUniProvider.GetTransactionClass: TCRTransactionClass;
begin
  Result := TPgSQLTransaction;
end;

function TPostgreSQLUniProvider.GetCommandClass: TCRCommandClass;
begin
  Result := TPgSQLCommand;
end;

function TPostgreSQLUniProvider.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TPgSQLRecordSet;
end;

function TPostgreSQLUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomPgDataSetService;
end;

function TPostgreSQLUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TPgScriptProcessor;
end;

function TPostgreSQLUniProvider.GetLoaderClass: TCRLoaderClass;
begin
  Result := TPgSQLLoader;
end;

function TPostgreSQLUniProvider.GetAlerterClass: TCRAlerterClass;
begin
  Result := TPgSQLAlerter;
end;

function TPostgreSQLUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomPgDumpProcessor;
end;

function TPostgreSQLUniProvider.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TPgSQLMetaData;
end;

function TPostgreSQLUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TPgConnectDialogService;
end;

function TPostgreSQLUniProvider.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := TCustomPgDataTypesMap;
end;

function TPostgreSQLUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TPgSqlFormatter;
end;

function TPostgreSQLUniProvider.GetParamObjectClass(Param: TDAParam): TClass;
begin
  case Param.DataType of
    ftOraBlob:
      Result := TPgSQLLargeObject;
    ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}:
      Result := TCompressedBlob;
    ftCursor:
      Result := TPgRefCursor;
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

function TPostgreSQLUniProvider.CreateParamObject(Param: TDAParam): TSharedObject;
begin
  case Param.DataType of
    ftOraBlob:
      Result := TPgSQLLargeObject.Create(nil);
    ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}: begin
      Result := TCompressedBlob.Create;
    {$IFDEF VER10P}
      if Param.DataType = ftWideMemo then
        TBlob(Result).IsUnicode := True;
    {$ENDIF}
    end;
    ftCursor:
      Result := TPgRefCursor.Create;
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

procedure TPostgreSQLUniProvider.SetObjectProps(Obj: TObject; Options: _TStrings; SetAllProps: boolean);
begin
  if Obj is TPgSQLConnection then begin
    TPgSQLConnection(Obj).SetProp(prEnablePgTimeStamps, False);
    TPgSQLConnection(Obj).SetProp(prEnableGeometrics, False);
    TPgSQLConnection(Obj).SetProp(prEnableComposites, False);
    TPgSQLConnection(Obj).SetProp(prIntervalAsString, True);
  end;

  inherited;
end;

function TPostgreSQLUniProvider.GetConnectionOptions: TOptionsList;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TStringOption.Create('ApplicationName', prApplicationName, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('Charset', prCharset, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeOut, [TPgSQLConnection, TPgConnectionParameters], 15));
    FConnectionOptions.Add(TEnumeratorOption.Create('ProtocolVersion', prProtocolVersion, [TPgSQLConnection, TPgConnectionParameters], Variant(pv30), TypeInfo(TProtocolVersion)));
    FConnectionOptions.Add(TStringOption.Create('Schema', prSchema, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TPgSQLConnection, TPgConnectionParameters], {$IFNDEF UNICODE_BUILD}False{$ELSE}True{$ENDIF}));

    // SSL
    FConnectionOptions.Add(TEnumeratorOption.Create('SSLMode', prSSL_Mode, [TPgSQLConnection, TPgConnectionParameters], Variant(smDisable), TypeInfo(TSSLMode)));
    FConnectionOptions.Add(TStringOption.Create('SSLCACert', prSSL_CACert, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLCert', prSSL_Cert, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLKey', prSSL_Key, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLCipherList', prSSL_CipherList, [TPgSQLConnection, TPgConnectionParameters], ''));
  end;
  Result := FConnectionOptions;
end;

function TPostgreSQLUniProvider.GetSQLOptions: TOptionsList;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeOut, [TPgSQLCommand], 0));
    FSQLOptions.Add(TBooleanOption.Create('UnpreparedExecute', prSimpleQueryExecute, [TPgSQLCommand], False));
    FSQLOptions.Add(TBooleanOption.Create('UseParamTypes', prUseParamTypes, [TPgSQLCommand], False));
  end;
  Result := FSQLOptions;
end;

function TPostgreSQLUniProvider.GetDataSetOptions: TOptionsList;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TBooleanOption.Create('AutoDeleteBlob', prAutoDeleteBlob, [TCustomPgDataSetService], True));
    FDataSetOptions.Add(TBooleanOption.Create('CacheBlobs', prCacheBlobs, [TPgSQLRecordSet], True));
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeOut, [TPgSQLRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('DeferredBlobRead', prDeferredBlobRead, [TPgSQLRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TPgSQLRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TPgSQLRecordSet], True));
    FDataSetOptions.Add(TStringOption.Create('KeySequence', prKeySequence, [TCustomPgDataSetService], ''));
    FDataSetOptions.Add(TBooleanOption.Create('OIDAsInt', prOIDAsInt, [TPgSQLRecordSet], False));
    FDataSetOptions.Add(TEnumeratorOption.Create('SequenceMode', prSequenceMode, [TCustomPgDataSetService], Variant(_smPost), TypeInfo(_TSequenceMode)));
    FDataSetOptions.Add(TBooleanOption.Create('UnknownAsString', prUnknownAsString, [TPgSQLRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('UnpreparedExecute', prSimpleQueryExecute, [TPgSQLRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('UseParamTypes', prUseParamTypes, [TPgSQLRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('CursorWithHold', prCursorWithHold, [TPgSQLRecordSet], False));
  end;
  Result := FDataSetOptions;
end;

function TPostgreSQLUniProvider.GetScriptOptions: TOptionsList;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
  end;
  Result := FScriptOptions;
end;

function TPostgreSQLUniProvider.GetLoaderOptions: TOptionsList;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
    FLoaderOptions.Add(TBooleanOption.Create('TextMode', prTextMode, [TPgSQLLoader], False));
    FLoaderOptions.Add(TIntegerOption.Create('BufferSize', prBufferSize, [TPgSQLLoader], WriteBufferSize));
  end;
  Result := FLoaderOptions;
end;

class function TPostgreSQLUniProvider.GetSocket(Connection: TUniConnection): {$IFNDEF CLR}longint{$ELSE}IntPtr{$ENDIF};
var
  ICon: TPgSQLConnection;
begin
  Assert(Connection.Connected);
  ICon := TDBAccessUtils.GetIConnection(Connection) as TPgSQLConnection;
  Result := (ICon.GetProtocol.Net.Vio as TCRVioTcp).GetSocket;
end;

{ TPgConnectDialogService }

function TPgConnectDialogService.GetDefaultDatabase: _string;
begin
  Result := 'template1';
end;

{ TPgSqlFormatter }

constructor TPgSqlFormatter.Create;
begin
  inherited;

  FFunctions := PgFunctions;
  FPredefinedMacros := PgMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TPostgreSQLUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TPostgreSQLUniProvider);

  PgFunctions := _TStringList.Create;
  PgFunctions.AddObject('USER', TObject(PChar('USER()')));
  PgFunctions.AddObject('CHAR_LENGTH', TObject(PChar('CHAR_LENGTH(%s)')));
  PgFunctions.AddObject('LOCATE', TObject(PChar('STRPOS(%1:s, %0:s)')));
  PgFunctions.AddObject('SUBSTRING', TObject(PChar('SUBSTRING(%s, %s, %s)')));
  PgFunctions.AddObject('CONCAT', TObject(PChar('%s || %s')));
  PgFunctions.AddObject('CHAR', TObject(PChar('CHR(%s)')));
  PgFunctions.AddObject('TRIM', TObject(PChar('TRIM(%s)')));
  PgFunctions.AddObject('TRUNCATE', TObject(PChar('TRUNC(%s, %s)')));
  PgFunctions.AddObject('CEILING', TObject(PChar('CEILING(%s)')));
  // Date-time
  PgFunctions.AddObject('CURRENT_DATE', TObject(PChar('CURRENT_DATE')));
  PgFunctions.AddObject('YEAR', TObject(PChar('EXTRACT(YEAR FROM %s)')));
  PgFunctions.AddObject('MONTH', TObject(PChar('EXTRACT(MONTH FROM %s)')));
  PgFunctions.AddObject('DAY', TObject(PChar('EXTRACT(DAY FROM %s)')));
  PgFunctions.AddObject('DATEADD', TObject(PChar('(%2:s + INTERVAL'' %1:s %0:s'')')));
  PgFunctions.AddObject('DATEDIFF', TObject(PChar('EXTRACT(%s FROM (%2:s - %1:s))')));
  // Date-time literals
  PgFunctions.AddObject('__DATE_TIME_LITERAL', TObject(PChar('TIMESTAMP %s')));
  PgFunctions.AddObject('__DATE_LITERAL', TObject(PChar('DATE %s')));
  PgFunctions.AddObject('__TIME_LITERAL', TObject(PChar('TIME %s')));
  // CONVERT functions
  PgFunctions.AddObject('TODATE', TObject(PChar('CAST(%s AS TIMESTAMP)')));
  PgFunctions.AddObject('TONUMBER', TObject(PChar('CAST(%s AS NUMERIC)')));
  PgFunctions.AddObject('TOCHAR', TObject(PChar('CAST(%s AS VARCHAR)')));

  PgMacros := _TStringList.Create;
  PgMacros.AddObject('PROVIDER', TObject(PChar('PostgreSQL')));
  PgMacros.AddObject('POSTGRESQL', TObject(PChar('')));
  // DataType macros
  PgMacros.AddObject('DATETIME', TObject(PChar('TIMESTAMP')));
  PgMacros.AddObject('DOUBLE', TObject(PChar('DOUBLE PRECISION')));
  PgMacros.AddObject('VARCHAR', TObject(PChar('VARCHAR')));

finalization
  UniProviders.UnRegisterProvider(TPostgreSQLUniProvider);

  PgFunctions.Free;
  PgMacros.Free;

end.
