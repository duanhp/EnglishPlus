
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Sdac.inc}
unit SQLServerUniProvider;
{$ENDIF}

interface

uses
  SysUtils, Windows, Classes, Variants, DB, CRAccess, CRConnectionPool, MemData, DBAccess,
  {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF},
  MemUtils, CRParser, DAScript, DADump, UniProvider,
{$IFNDEF UNIDACPRO}
  OLEDBAccess;
{$ELSE}
  OLEDBAccessUni;
{$ENDIF}

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TSQLServerUniProvider = class(TUniProvider)
  public
    class function GetProviderName: string; override;

    function IsDatabaseSupported: boolean; override;

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
    function DefaultTableSchema: _string; override;
  end;

  TMSConnectDialogService = class (TConnectDialogService)
  private
    FProvider: TOLEDBProvider;
    FAuthentication: TMSAuthentication;
  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function UseDatabaseHistory: boolean; override;
    function UsernameEnabled: boolean; override;
    function PasswordEnabled: boolean; override;
    function ServerEnabled: boolean; override;
  end;

  TMSSqlFormatter = class(TUniSqlFormatter)
  protected
    function LeftQuote: _char; override;
    function RightQuote: _char; override;
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
  TypInfo,
{$IFNDEF UNIDACPRO}
  MSParser, MSConnectionPool, MSServices, MSScriptProcessor;
{$ELSE}
  MSParserUni, MSConnectionPoolUni, MSServicesUni, MSScriptProcessorUni;
{$ENDIF}

var
  MSFunctions, MSMacros: _TStringList;

class function TSQLServerUniProvider.GetProviderName: string;
begin
  Result := 'SQL Server';
end;

function TSQLServerUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TSQLServerUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TMSParser;
end;

function TSQLServerUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TMSConnectionParameters;
end;

function TSQLServerUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TMSConnectionPoolManager;
end;

function TSQLServerUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TOLEDBConnection;
end;

function TSQLServerUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TMSServerEnumerator;
end;

function TSQLServerUniProvider.GetTransactionClass: TCRTransactionClass;
begin
  Result := TOLEDBTransaction;
end;

function TSQLServerUniProvider.GetCommandClass: TCRCommandClass;
begin
  Result := TOLEDBCommand;
end;

function TSQLServerUniProvider.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TOLEDBRecordSet;
end;

function TSQLServerUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomMSDataSetService;
end;

function TSQLServerUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TMSScriptProcessor;
end;

function TSQLServerUniProvider.GetLoaderClass: TCRLoaderClass;
begin
  Result := TOLEDBLoader;
end;

function TSQLServerUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomMSDumpProcessor;
end;

function TSQLServerUniProvider.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TOLEDBMetaData;
end;

function TSQLServerUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TMSConnectDialogService;
end;

function TSQLServerUniProvider.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := TCustomMSDataTypesMap;
end;

function TSQLServerUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TMSSqlFormatter;
end;

procedure TSQLServerUniProvider.SetObjectProps(Obj: TObject; Options: _TStrings; SetAllProps: boolean);
begin
  if Obj is TOLEDBConnection then begin
    TOLEDBConnection(Obj).SetProp(prSetLockTimeout, True);
  end
  else
  if Obj is TOLEDBCommand then begin
    TOLEDBCommand(Obj).SetProp(prSensibleBCDMapping, True);
  end
  else
  if Obj is TOLEDBRecordSet then begin
    TOLEDBREcordSet(Obj).GetCommand.SetProp(prSensibleBCDMapping, True);
  end;

  inherited;
end;

function TSQLServerUniProvider.GetConnectionOptions: TOptionsList;
begin
  if FConnectionOptions = nil then begin

    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TBooleanOption.Create('QuotedIdentifier', prQuotedIdentifier, [TOLEDBConnection, TMSConnectionParameters], True));
    FConnectionOptions.Add(TBooleanOption.Create('Encrypt', prEncrypt, [TOLEDBConnection, TMSConnectionParameters], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('OLEDBProvider', prProvider, [TOLEDBConnection, TMSConnectionParameters, TMSServerEnumerator, TMSConnectDialogService], Variant(prAuto), TypeInfo(TOLEDBProvider)));

    // TMSConnection options
    FConnectionOptions.Add(TEnumeratorOption.Create('Authentication', prAuthentication, [TOLEDBConnection, TMSConnectionParameters, TMSConnectDialogService], Variant(auServer), TypeInfo(TMSAuthentication)));
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TOLEDBConnection], 15));
    FConnectionOptions.Add(TStringOption.Create('Language', prLanguage, [TOLEDBConnection, TMSConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('PersistSecurityInfo', prPersistSecurityInfo, [TOLEDBConnection, TMSConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('AutoTranslate', prAutoTranslate, [TOLEDBConnection, TMSConnectionParameters], True));
    FConnectionOptions.Add(TStringOption.Create('NetworkLibrary', prNetworkLibrary, [TOLEDBConnection, TMSConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ApplicationName', prApplicationName, [TOLEDBConnection, TMSConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('WorkstationID', prWorkstationID, [TOLEDBConnection, TMSConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('PacketSize', prPacketSize, [TOLEDBConnection, TMSConnectionParameters], 4096));
    FConnectionOptions.Add(TStringOption.Create('InitialFileName', prInitialFileName, [TOLEDBConnection], ''));
    FConnectionOptions.Add(TBooleanOption.Create('MultipleActiveResultSets', prMARS, [TOLEDBConnection], False));
    FConnectionOptions.Add(TStringOption.Create('FailoverPartner', prFailoverPartner, [TOLEDBConnection], ''));
    FConnectionOptions.Add(TBooleanOption.Create('TrustServerCertificate', prTrustServerCertificate, [TOLEDBConnection], False));
    FConnectionOptions.Add(TIntegerOption.Create('LockTimeout', prDefaultLockTimeout, [TOLEDBConnection], DefaultDefaultLockTimeout));
  {$IFDEF VER10P}
    FConnectionOptions.Add(TBooleanOption.Create('UseWideMemos', prWideMemos, [TOLEDBConnection], True));
  {$ENDIF}

    // TCompactConnection options
    FConnectionOptions.Add(TEnumeratorOption.Create('CompactInitMode', prInitMode, [TOLEDBConnection], Variant(imReadWrite), TypeInfo(TMSInitMode)));
    FConnectionOptions.Add(TIntegerOption.Create('CompactLocaleIdentifier', prLocaleIdentifier, [TOLEDBConnection], GetSystemDefaultLCID));
    FConnectionOptions.Add(TIntegerOption.Create('CompactLockEscalation', prLockEscalation, [TOLEDBConnection], 100));
    FConnectionOptions.Add(TEnumeratorOption.Create('CompactTransactionCommitMode', prTransactionCommitMode, [TOLEDBConnection], Variant(cmAsynchCommit), TypeInfo(TCompactCommitMode)));
    FConnectionOptions.Add(TIntegerOption.Create('CompactMaxDatabaseSize', prMaxDatabaseSize, [TOLEDBConnection], 128));
    FConnectionOptions.Add(TIntegerOption.Create('CompactMaxBufferSize', prMaxBufferSize, [TOLEDBConnection], 640));
    FConnectionOptions.Add(TStringOption.Create('CompactTempFileDirectory', prTempFileDirectory, [TOLEDBConnection], ''));
    FConnectionOptions.Add(TIntegerOption.Create('CompactTempFileMaxSize', prTempFileMaxSize, [TOLEDBConnection], 128));
    FConnectionOptions.Add(TIntegerOption.Create('CompactDefaultLockEscalation', prDefaultLockEscalation, [TOLEDBConnection], 100));
    FConnectionOptions.Add(TIntegerOption.Create('CompactAutoShrinkThreshold', prAutoShrinkThreshold, [TOLEDBConnection], 60));
    FConnectionOptions.Add(TIntegerOption.Create('CompactFlushInterval', prFlushInterval, [TOLEDBConnection], 10));
    FConnectionOptions.Add(TEnumeratorOption.Create('CompactVersion', prCompactVersion, [TOLEDBConnection], Variant(cvAuto), TypeInfo(TCompactVersion)));
  end;
  Result := FConnectionOptions;
end;

function TSQLServerUniProvider.GetSQLOptions: TOptionsList;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TOLEDBCommand], 0));
    FSQLOptions.Add(TBooleanOption.Create('DescribeParams', prUseDescribeParams, [TOLEDBCommand], False));
  end;
  Result := FSQLOptions;
end;

function TSQLServerUniProvider.GetDataSetOptions: TOptionsList;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);

    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TOLEDBRecordSet], True));
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TOLEDBRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('UniqueRecords', prUniqueRecords, [TOLEDBRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('CursorUpdate', prCursorUpdate, [TOLEDBRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('QueryIdentity', prQueryIdentity, [TCustomMSDataSetService], True));
    FDataSetOptions.Add(TBooleanOption.Create('CheckRowVersion', prCheckRowVersion, [TCustomMSDataSetService], False));
    FDataSetOptions.Add(TBooleanOption.Create('DisableMultipleResults', prDisableMultipleResults, [TOLEDBRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('DescribeParams', prUseDescribeParams, [TOLEDBCommand], False));
  end;
  Result := FDataSetOptions;
end;

function TSQLServerUniProvider.GetScriptOptions: TOptionsList;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
  end;
  Result := FScriptOptions;
end;

function TSQLServerUniProvider.GetLoaderOptions: TOptionsList;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
    FLoaderOptions.Add(TBooleanOption.Create('KeepIdentity', prKeepIdentity, [TOLEDBLoader], False));
    FLoaderOptions.Add(TBooleanOption.Create('KeepNulls', prKeepNulls, [TOLEDBLoader], False));
    FLoaderOptions.Add(TIntegerOption.Create('RowsPerBatch', prRowsPerBatch, [TOLEDBLoader], 0));
    FLoaderOptions.Add(TIntegerOption.Create('KilobytesPerBatch', prKilobytesPerBatch, [TOLEDBLoader], 0));
    FLoaderOptions.Add(TBooleanOption.Create('LockTable', prLockTable, [TOLEDBLoader], False));
    FLoaderOptions.Add(TBooleanOption.Create('CheckConstraints', prCheckConstraints, [TOLEDBLoader], False));
  end;
  Result := FLoaderOptions;
end;

function TSQLServerUniProvider.GetDumpOptions: TOptionsList;
begin
  if FDumpOptions = nil then begin
    FDumpOptions := TOptionsList.Create(GetProviderName);
    FDumpOptions.Add(TBooleanOption.Create('IdentityInsert', prIdentityInsert, [TCustomMSDumpProcessor], False));
  end;
  Result := FDumpOptions;
end;

function TSQLServerUniProvider.DefaultTableSchema: _string;
begin
  Result := 'dbo';
end;

{ TMSConnectDialogService }

function TMSConnectDialogService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prProvider:
      FPRovider := TOLEDBProvider(Value);
    prAuthentication:
      FAuthentication := TMSAuthentication(Value);
  else
    Result := inherited SetProp(Prop, Value);    
  end;  
end;

function TMSConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := FPRovider = prCompact;
end;

function TMSConnectDialogService.UsernameEnabled: boolean;
begin
  Result := (FPRovider <> prCompact) and (FAuthentication <> auWindows);
end;

function TMSConnectDialogService.PasswordEnabled: boolean;
begin
  Result := (FPRovider = prCompact) or (FAuthentication <> auWindows);
end;

function TMSConnectDialogService.ServerEnabled: boolean;
begin
  Result := FPRovider <> prCompact;
end;

{ TMSSqlFormatter }

constructor TMSSqlFormatter.Create;
begin
  inherited;

  FFunctions := MSFunctions;
  FPredefinedMacros := MSMacros;
end;

function TMSSqlFormatter.LeftQuote: _char;
begin
  Result := '[';
end;

function TMSSqlFormatter.RightQuote: _char;
begin
  Result := ']';
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TSQLServerUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TSQLServerUniProvider);

  MSFunctions := _TStringList.Create;
  MSFunctions.AddObject('USER', TObject(PChar('USER')));
  MSFunctions.AddObject('CHAR_LENGTH', TObject(PChar('LEN(%s)')));
  MSFunctions.AddObject('LOCATE', TObject(PChar('CHARINDEX(%s, %s)')));
  MSFunctions.AddObject('SUBSTRING', TObject(PChar('SUBSTRING(%s, %s, %s)')));
  MSFunctions.AddObject('CONCAT', TObject(PChar('%s + %s')));
  MSFunctions.AddObject('CHAR', TObject(PChar('CHAR(%s)')));
  MSFunctions.AddObject('TRIM', TObject(PChar('LTRIM(RTRIM(%s))')));
  MSFunctions.AddObject('TRUNCATE', TObject(PChar('ROUND(%s, %s, 1)')));
  MSFunctions.AddObject('CEILING', TObject(PChar('CEILING(%s)')));
  // Date-time
  MSFunctions.AddObject('CURRENT_DATE', TObject(PChar('GETDATE()')));
  MSFunctions.AddObject('YEAR', TObject(PChar('YEAR(%s)')));
  MSFunctions.AddObject('MONTH', TObject(PChar('MONTH(%s)')));
  MSFunctions.AddObject('DAY', TObject(PChar('DAY(%s)')));
  MSFunctions.AddObject('DATEDIFF', TObject(PChar('DATEDIFF(%s, %s, %s)')));
  MSFunctions.AddObject('DATEADD', TObject(PChar('DATEADD(%s, %s, %s)')));
  // Date-time literals
  MSFunctions.AddObject('__DATE_TIME_LITERAL', TObject(PChar('CONVERT(DATETIME, %s)')));
  MSFunctions.AddObject('__DATE_LITERAL', TObject(PChar('CONVERT(DATE, %s)')));
  MSFunctions.AddObject('__TIME_LITERAL', TObject(PChar('CONVERT(TIME, %s)')));
  // CONVERT functions
  MSFunctions.AddObject('TODATE', TObject(PChar('CONVERT(DATETIME, %s)')));
  MSFunctions.AddObject('TONUMBER', TObject(PChar('CONVERT(FLOAT(53), %s)')));
  MSFunctions.AddObject('TOCHAR', TObject(PChar('CONVERT(VARCHAR, %s, 20)')));

  MSMacros := _TStringList.Create;
  MSMacros.AddObject('PROVIDER', TObject(PChar('SQL Server')));
  MSMacros.AddObject('SQLSERVER', TObject(PChar('')));
  // DataType macros
  MSMacros.AddObject('DATETIME', TObject(PChar('DATETIME')));
  MSMacros.AddObject('DOUBLE', TObject(PChar('FLOAT(53)')));
  MSMacros.AddObject('VARCHAR', TObject(PChar('VARCHAR')));

finalization
  UniProviders.UnRegisterProvider(TSQLServerUniProvider);

  MSFunctions.Free;
  MSMacros.Free;

end.
