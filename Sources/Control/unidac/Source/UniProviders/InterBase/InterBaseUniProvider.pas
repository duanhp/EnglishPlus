
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I IbDac.inc}

unit InterBaseUniProvider;
{$ENDIF}

interface

uses
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  SysUtils, Classes, TypInfo, DB,
  MemUtils, MemData, CRAccess, CRConnectionPool, DBAccess,
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, UniProvider, DAScript, DADump, CRParser,
{$IFNDEF UNIDACPRO}
  IBCServices, IBCClasses, IBCConnectionPool, IBCScriptProcessor, IBCParser;
{$ELSE}
  IBCServicesUni, IBCClassesUni, IBCConnectionPoolUni, IBCScriptProcessorUni, IBCParserUni;
{$ENDIF}

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TInterBaseUniProvider = class(TUniProvider)
  public
    class function GetProviderName: string; override;

    function IsDatabaseSupported: boolean; override;
    function IsDataSetNeedTransaction: boolean; override;
    function IsInOutParamSupported: boolean; override;
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

  TIBCConnectDialogService = class (TConnectDialogService)
  public
    function UseDatabaseHistory: boolean; override;
  end;

  TUniIBCScriptProcessor = class (TCustomIBCScriptProcessor)
  protected
    procedure SetAutoDDL(Value: boolean); override;
    procedure SetDatabase(Connection: TCustomDAConnection; const Value: string); override;
    procedure SetRole(Connection: TCustomDAConnection; const Value: string); override;
    procedure SetCharset(Connection: TCustomDAConnection; const Value: string); override;
    procedure SetSQLDialect(Connection: TCustomDAConnection; Value: integer); override;
    procedure CreateDatabase(Connection: TCustomDAConnection; const Params: string); override;
    procedure DropDatabase(Connection: TCustomDAConnection); override;
  end;

  TIBCSqlFormatter = class(TUniSqlFormatter)
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
  DAConsts, Uni, UniScript;

var
  IBCFunctions, IBCMacros: _TStringList;

{ TInterBaseUniProvider }

class function TInterBaseUniProvider.GetProviderName: string;
begin
  Result := 'InterBase';
end;

function TInterBaseUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TInterBaseUniProvider.IsDataSetNeedTransaction: boolean;
begin
  Result := True;
end;

function TInterBaseUniProvider.IsInOutParamSupported: boolean;
begin
  Result := False;
end;

function TInterBaseUniProvider.NeedRecreateProcCall: boolean;
begin
  Result := True;
end;

function TInterBaseUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TIBCParser;
end;

function TInterBaseUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TIBCConnectionParameters;
end;

function TInterBaseUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TIBCConnectionPoolManager;
end;

function TInterBaseUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TGDSConnection;
end;

function TInterBaseUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TIBCServerEnumerator;
end;

function TInterBaseUniProvider.GetTransactionClass: TCRTransactionClass;
begin
  Result := TGDSTransaction;
end;

function TInterBaseUniProvider.GetCommandClass: TCRCommandClass;
begin
  Result := TGDSCommand;
end;

function TInterBaseUniProvider.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TGDSRecordSet;
end;

function TInterBaseUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomIBCDataSetService;
end;

function TInterBaseUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TUniIBCScriptProcessor;
end;

function TInterBaseUniProvider.GetLoaderClass: TCRLoaderClass;
begin
  Result := TGDSLoader;
end;

function TInterBaseUniProvider.GetAlerterClass: TCRAlerterClass;
begin
  Result := TGDSAlerter;
end;

function TInterBaseUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomIBCDumpProcessor;
end;

function TInterBaseUniProvider.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TGDSMetaData;
end;

function TInterBaseUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TIBCConnectDialogService;
end;

function TInterBaseUniProvider.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := TCustomIBCDataTypesMap;
end;

function TInterBaseUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TIBCSqlFormatter;
end;

function TInterBaseUniProvider.GetParamObjectClass(Param: TDAParam): TClass;
begin
  case Param.DataType of
    ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}:
      Result := TIBCBlob;
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

function TInterBaseUniProvider.CreateParamObject(Param: TDAParam): TSharedObject;
begin
  case Param.DataType of
    ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}: begin
      Result := TIBCBlob.Create(TGDSConnection(nil), TGDSTransaction(nil));
      TIBCBlob(Result).IsUnicode := Param.AsBlobRef.IsUnicode;
    end
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

procedure TInterBaseUniProvider.SetObjectProps(Obj: TObject; Options: _TStrings; SetAllProps: boolean);
begin
  if Obj is TGDSConnection then begin
    TGDSConnection(Obj).SetProp(prSimpleNumericMap, True);
    TGDSConnection(Obj).SetProp(prEnableMemos, True);
  end;

  inherited;
end;

function TInterBaseUniProvider.GetConnectionOptions: TOptionsList;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('CharLength', prCharLength, [TGDSConnection], 0));
    FConnectionOptions.Add(TStringOption.Create('Charset', prCharset, [TGDSConnection, TIBCConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('Role', prRole, [TGDSConnection, TIBCConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TGDSConnection, TIBCConnectionParameters], {$IFNDEF UNICODE_BUILD}False{$ELSE}True{$ENDIF}));
    FConnectionOptions.Add(TIntegerOption.Create('SQLDialect', prSQLDialect, [TGDSConnection, TIBCConnectionParameters], 3));
    FConnectionOptions.Add(TStringOption.Create('ClientLibrary', prClientLibrary, [TGDSConnection], ''));
    FConnectionOptions.Add(TEnumeratorOption.Create('Protocol', prProtocol, [TGDSConnection, TIBCConnectionParameters], Variant(_TCP), TypeInfo(_TIBCProtocol)));
  end;
  Result := FConnectionOptions;
end;

function TInterBaseUniProvider.GetSQLOptions: TOptionsList;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TBooleanOption.Create('DescribeParams', prUseDescribeParams, [TGDSCommand], False));
  end;
  Result := FSQLOptions;
end;

function TInterBaseUniProvider.GetDataSetOptions: TOptionsList;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TGDSRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('AutoClose', prAutoClose, [TGDSRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('FieldsAsString', prFieldsAsString, [TGDSRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('DeferredBlobRead', prDeferredBlobRead, [TGDSRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('CacheBlobs', prCacheBlobs, [TGDSCommand], True));
    FDataSetOptions.Add(TBooleanOption.Create('StreamedBlobs', prStreamedBlobs, [TGDSCommand], False));
    FDataSetOptions.Add(TBooleanOption.Create('ComplexArrayFields', prComplexArrayFields, [TGDSRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('DeferredArrayRead', prDeferredArrayRead, [TGDSRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('CacheArrays', prCacheArrays, [TGDSCommand], True));
    FDataSetOptions.Add(TStringOption.Create('KeyGenerator', prKeyGenerator, [TCustomIBCDataSetService], ''));
    FDataSetOptions.Add(TEnumeratorOption.Create('GeneratorMode', prGeneratorMode, [TCustomIBCDataSetService], Variant(_gmPost), TypeInfo(_TGeneratorMode)));
    FDataSetOptions.Add(TStringOption.Create('GeneratorStep', prGeneratorStep, [TCustomIBCDataSetService], 1));
    FDataSetOptions.Add(TBooleanOption.Create('BooleanDomainFields', prBooleanDomainFields, [TGDSRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('DescribeParams', prUseDescribeParams, [TGDSCommand], False));
    FDataSetOptions.Add(TBooleanOption.Create('SetDomainNames', prSetDomainNames, [TGDSRecordSet], False));
  end;
  Result := FDataSetOptions;
end;

function TInterBaseUniProvider.GetScriptOptions: TOptionsList;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
    FScriptOptions.Add(TBooleanOption.Create('AutoDDL', prAutoDDL, [TCustomIBCScriptProcessor], True));
  end;
  Result := FScriptOptions;
end;

function TInterBaseUniProvider.GetLoaderOptions: TOptionsList;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
    FLoaderOptions.Add(TEnumeratorOption.Create('InsertMode', prInsertMode, [TGDSLoader], Variant(_imInsert), TypeInfo(_TIBCInsertMode)));
    FLoaderOptions.Add(TIntegerOption.Create('RowsPerBatch', prRowsPerBatch, [TGDSLoader], 50));
  end;
  Result := FLoaderOptions;
end;

{ TIBCConnectDialogService }

function TIBCConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := True;
end;

{ TUniIBCScriptProcessor }

procedure TUniIBCScriptProcessor.SetAutoDDL(Value: boolean);
begin
  TUniScript(FOwner).SpecificOptions.Values['InterBase.AutoDDL'] := BoolToStr(Value, True);
end;

procedure TUniIBCScriptProcessor.SetDatabase(Connection: TCustomDAConnection; const Value: string);
var
  Host, FileName: string;
  Protocol: _TIBCProtocol;
begin
  if GetDBNamePos(Value) > 0 then begin
    ParseDatabaseName(Value, Host, Protocol, FileName);
    Connection.Server := Host;
    TUniConnection(Connection).SpecificOptions.Values['InterBase.Protocol'] :=
      Copy(GetEnumName(TypeInfo(_TIBCProtocol), Integer(Protocol)), 2, MaxInt);
  end
  else
    FileName := Value;

  TUniConnection(Connection).Database := FileName;
end;

procedure TUniIBCScriptProcessor.SetRole(Connection: TCustomDAConnection; const Value: string);
begin
  TUniConnection(Connection).SpecificOptions.Values['InterBase.Role'] := Value;
end;

procedure TUniIBCScriptProcessor.SetCharset(Connection: TCustomDAConnection; const Value: string);
begin
  TUniConnection(Connection).SpecificOptions.Values['InterBase.Charset'] := Value;
end;

procedure TUniIBCScriptProcessor.SetSQLDialect(Connection: TCustomDAConnection; Value: integer);
begin
  TUniConnection(Connection).SpecificOptions.Values['InterBase.SQLDialect'] := IntToStr(Value);
end;

procedure TUniIBCScriptProcessor.CreateDatabase(Connection: TCustomDAConnection; const Params: string);
var
  ICon: TGDSConnection;
  List: TStringList;
begin
  TDBAccessUtils.CreateIConnection(Connection);
  ICon := TGDSConnection(TDBAccessUtils.GetIConnection(Connection));
  ICon.SetServer(Connection.Server);
  List := TStringList.Create;
  try
    List.Text := Params;
    ICon.SetParams(List);
  finally
    List.Free;
  end;
  ICon.CreateDatabase;
end;

procedure TUniIBCScriptProcessor.DropDatabase(Connection: TCustomDAConnection);
var
  ICon: TGDSConnection;
begin
  if not Connection.Connected then
    DatabaseError(SConnectionIsClosed);

  TDBAccessUtils.DisconnectTransaction(Connection);

  ICon := TGDSConnection(TDBAccessUtils.GetIConnection(Connection));
  ICon.DropDatabase;
end;

{ TIBCSqlFormatter }

constructor TIBCSqlFormatter.Create;
begin
  inherited;

  FFunctions := IBCFunctions;
  FPredefinedMacros := IBCMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TInterBaseUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TInterBaseUniProvider);

  IBCFunctions := _TStringList.Create;
  IBCFunctions.AddObject('USER', TObject(PChar('USER')));
  IBCFunctions.AddObject('CHAR_LENGTH', TObject(PChar('strlen(%s)')));
  //IBCFunctions.AddObject('LOCATE', TObject(PChar('')));
  IBCFunctions.AddObject('SUBSTRING', TObject(PChar('substring(%s from %s for %s)')));
  IBCFunctions.AddObject('CONCAT', TObject(PChar('%s || %s')));
  IBCFunctions.AddObject('CHAR', TObject(PChar('ascii_char(%s)')));
  IBCFunctions.AddObject('TRIM', TObject(PChar('TRIM(%s)')));
  IBCFunctions.AddObject('TRUNCATE', TObject(PChar('TRUNCATE(%s)')));
  IBCFunctions.AddObject('CEILING', TObject(PChar('CEILING(%s)')));
  // Date-time
  IBCFunctions.AddObject('CURRENT_DATE', TObject(PChar('CURRENT_DATE')));
  IBCFunctions.AddObject('YEAR', TObject(PChar('EXTRACT(YEAR FROM %s)')));
  IBCFunctions.AddObject('MONTH', TObject(PChar('EXTRACT(MONTH FROM %s)')));
  IBCFunctions.AddObject('DAY', TObject(PChar('EXTRACT(DAY FROM %s)')));
  // Date-time literals
  IBCFunctions.AddObject('__DATE_TIME_LITERAL', TObject(PChar('CAST(%s AS TIMESTAMP)')));
  IBCFunctions.AddObject('__DATE_LITERAL', TObject(PChar('CAST(%s AS DATE)')));
  IBCFunctions.AddObject('__TIME_LITERAL', TObject(PChar('CAST(%s AS TIME)')));
  // CONVERT functions
  IBCFunctions.AddObject('TODATE', TObject(PChar('CAST(%s AS TIMESTAMP)')));
  IBCFunctions.AddObject('TONUMBER', TObject(PChar('CAST(%s AS DOUBLE PRECISION)')));
  IBCFunctions.AddObject('TOCHAR', TObject(PChar('CAST(%s AS VARCHAR(24))')));

  IBCMacros := _TStringList.Create;
  IBCMacros.AddObject('PROVIDER', TObject(PChar('InterBase')));
  IBCMacros.AddObject('INTERBASE', TObject(PChar('')));
  // DataType macros
  IBCMacros.AddObject('DATETIME', TObject(PChar('TIMESTAMP')));
  IBCMacros.AddObject('DOUBLE', TObject(PChar('DOUBLE PRECISION')));
  IBCMacros.AddObject('VARCHAR', TObject(PChar('VARCHAR')));

finalization
  UniProviders.UnRegisterProvider(TInterBaseUniProvider);

  IBCFunctions.Free;
  IBCMacros.Free;

end.

