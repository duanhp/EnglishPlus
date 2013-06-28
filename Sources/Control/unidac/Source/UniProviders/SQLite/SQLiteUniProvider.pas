
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I SQLiteDac.inc}
unit SQLiteUniProvider;
{$ENDIF}

interface

uses
  SysUtils, Classes, Variants, DB, CRAccess, CRConnectionPool, MemData,
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, MemUtils, CRParser, DBAccess, 
  DAScript, DADump, Uni, UniProvider,
{$IFNDEF UNIDACPRO}
  LiteCall, LiteClasses, LiteCollation, LiteFunction;
{$ELSE}
  LiteCallUni, LiteClassesUni, LiteCollationUni, LiteFunctionUni;
{$ENDIF}

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TSQLiteUniProvider = class(TUniProvider)
  public
    class function GetProviderName: string; override;

    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;
    function IsInOutParamSupported: boolean; override;
    function IsPoolingSupported: boolean; override;

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
    function GetParamObjectClass(Param: TDAParam): TClass; override;
    function CreateParamObject(Param: TDAParam): TSharedObject; override;

    procedure SetObjectProps(Obj: TObject; Options: _TStrings; SetAllProps: boolean); override;

    function GetConnectionOptions: TOptionsList; override;
    function GetSQLOptions: TOptionsList; override;
    function GetDataSetOptions: TOptionsList; override;
    function GetScriptOptions: TOptionsList; override;
  end;

  TLiteConnectDialogService = class(TConnectDialogService)
  public
    function UseDatabaseHistory: boolean; override;
    function UsernameEnabled: boolean; override;
    function PasswordEnabled: boolean; override;
    function ServerEnabled: boolean; override;
  end;

  TLiteFormatter = class(TUniSqlFormatter)
  protected
  public
    constructor Create; override;
  end;

  TLiteUtils = class
  private
    class function GetConnection(Connection: TCustomDAConnection): TSQLiteConnection;
  public
    { SQLite user collations }
    class procedure RegisterCollation(Connection: TCustomDAConnection; Name: _string; LiteCollation: TLiteCollation);
    class procedure UnRegisterCollation(Connection: TCustomDAConnection; Name: _string);
    class procedure RegisterAnsiCollation(Connection: TCustomDAConnection; Name: _string; LiteAnsiCollation: TLiteAnsiCollation);
    class procedure UnRegisterAnsiCollation(Connection: TCustomDAConnection; Name: _string);
    class procedure RegisterWideCollation(Connection: TCustomDAConnection; Name: _string; LiteWideCollation: TLiteWideCollation);
    class procedure UnRegisterWideCollation(Connection: TCustomDAConnection; Name: _string);

    { SQLite user functions }
    class procedure RegisterFunction(Connection: TCustomDAConnection; Name: _string; ParamCount: Integer; LiteFunction: TLiteFunction);
    class procedure UnRegisterFunction(Connection: TCustomDAConnection; Name: _string; ParamCount: Integer);

    { SQLite encryption }
    class procedure EncryptDatabase(Connection: TCustomDAConnection; NewKey: _string);
  end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}

implementation

uses
  DAConsts, UniConsts,
{$IFNDEF UNIDACPRO}
  LiteConsts, LiteParser, LiteServices;
{$ELSE}
  LiteConstsUni, LiteParserUni, LiteServicesUni;
{$ENDIF}

var
  LiteFunctions, LiteMacros: _TStringList;

class function TSQLiteUniProvider.GetProviderName: string;
begin
  Result := 'SQLite';
end;

function TSQLiteUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TSQLiteUniProvider.IsPortSupported: boolean;
begin
  Result := False;
end;

function TSQLiteUniProvider.IsInOutParamSupported: boolean;
begin
  Result := False;
end;

function TSQLiteUniProvider.IsPoolingSupported: boolean;
begin
  Result := False;
end;

function TSQLiteUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TLiteParser;
end;

function TSQLiteUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TCRConnectionParameters;
end;

function TSQLiteUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TCRConnectionPoolManager;
end;

function TSQLiteUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TSQLiteConnection;
end;

function TSQLiteUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TLiteServerEnumerator;
end;

function TSQLiteUniProvider.GetTransactionClass: TCRTransactionClass;
begin
  Result := TSQLiteTransaction;
end;

function TSQLiteUniProvider.GetCommandClass: TCRCommandClass;
begin
  Result := TSQLiteCommand;
end;

function TSQLiteUniProvider.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TSQLiteRecordSet;
end;

function TSQLiteUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomLiteDataSetService;
end;

function TSQLiteUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TLiteScriptProcessor;
end;

function TSQLiteUniProvider.GetLoaderClass: TCRLoaderClass;
begin
  Result := TSQLiteLoader;
end;

function TSQLiteUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomLiteDumpProcessor;
end;

function TSQLiteUniProvider.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TSQLiteMetaData;
end;

function TSQLiteUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TLiteConnectDialogService;
end;

function TSQLiteUniProvider.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := TCustomLiteDataTypesMap;
end;

function TSQLiteUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TLiteFormatter;
end;

function TSQLiteUniProvider.GetParamObjectClass(Param: TDAParam): TClass;
begin
  case Param.DataType of
    ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}:
      Result := TCompressedBlob;
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

function TSQLiteUniProvider.CreateParamObject(Param: TDAParam): TSharedObject;
begin
  case Param.DataType of
    ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}: begin
      Result := TCompressedBlob.Create;
    {$IFDEF VER10P}
      if Param.DataType = ftWideMemo then
        TBlob(Result).IsUnicode := True;
    {$ENDIF}
    end;
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

procedure TSQLiteUniProvider.SetObjectProps(Obj: TObject; Options: _TStrings; SetAllProps: boolean);
begin
  inherited;
end;

function TSQLiteUniProvider.GetConnectionOptions: TOptionsList;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TBooleanOption.Create('ASCIIDataBase', prASCIIDataBase, [TSQLiteConnection], False));
    FConnectionOptions.Add(TIntegerOption.Create('BusyTimeout', prBusyTimeout, [TSQLiteConnection], 0));
    FConnectionOptions.Add(TStringOption.Create('ClientLibrary', prClientLibrary, [TSQLiteConnection], ''));
    FConnectionOptions.Add(TBooleanOption.Create('DefaultCollations', prDefaultCollations, [TSQLiteConnection], True));
    FConnectionOptions.Add(TBooleanOption.Create('EnableSharedCache', prEnableSharedCache, [TSQLiteConnection], False));
    FConnectionOptions.Add(TStringOption.Create('EncryptionKey', prEncryptionKey, [TSQLiteConnection], ''));
    FConnectionOptions.Add(TBooleanOption.Create('ReadUncommitted', prReadUncommitted, [TSQLiteConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TSQLiteConnection], False));
  end;
  Result := FConnectionOptions;
end;

function TSQLiteUniProvider.GetSQLOptions: TOptionsList;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
  end;
  Result := FSQLOptions;
end;

function TSQLiteUniProvider.GetDataSetOptions: TOptionsList;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TSQLiteRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TSQLiteRecordSet], False));
  end;
  Result := FDataSetOptions;
end;

function TSQLiteUniProvider.GetScriptOptions: TOptionsList;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
  end;
  Result := FScriptOptions;
end;

{ TLiteConnectDialogService }

function TLiteConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := True;
end;

function TLiteConnectDialogService.UsernameEnabled: boolean;
begin
  Result := False;
end;

function TLiteConnectDialogService.PasswordEnabled: boolean;
begin
  Result := False;
end;

function TLiteConnectDialogService.ServerEnabled: boolean;
begin
  Result := False;
end;

{ TLiteFormatter }

constructor TLiteFormatter.Create;
begin
  inherited;

  FFunctions := LiteFunctions;
  FPredefinedMacros := LiteMacros;
end;

class function TLiteUtils.GetConnection(Connection: TCustomDAConnection): TSQLiteConnection;
var
  CRConnection: TCRConnection;
begin
  CRConnection := TDBAccessUtils.GetIConnection(Connection);

  if CRConnection = nil then
    raise Exception.Create(SConnectionIsClosed)
  else if not (CRConnection is TSQLiteConnection) then
    raise Exception.CreateFmt(SIncorrectConnectionType, ['TSQLiteConnection', CRConnection.ClassName]);

  Result := TSQLiteConnection(CRConnection);
end;

class procedure TLiteUtils.RegisterCollation(Connection: TCustomDAConnection; Name: _string; LiteCollation: TLiteCollation);
begin
  GetConnection(Connection).GetCollationManager.RegisterCollation(Name, LiteCollation);
end;

class procedure TLiteUtils.UnRegisterCollation(Connection: TCustomDAConnection; Name: _string);
begin
  GetConnection(Connection).GetCollationManager.UnRegisterCollation(Name);
end;

class procedure TLiteUtils.RegisterAnsiCollation(Connection: TCustomDAConnection; Name: _string; LiteAnsiCollation: TLiteAnsiCollation);
begin
  GetConnection(Connection).GetCollationManager.RegisterAnsiCollation(Name, LiteAnsiCollation);
end;

class procedure TLiteUtils.UnRegisterAnsiCollation(Connection: TCustomDAConnection; Name: _string);
begin
  GetConnection(Connection).GetCollationManager.UnRegisterAnsiCollation(Name);
end;

class procedure TLiteUtils.RegisterWideCollation(Connection: TCustomDAConnection; Name: _string; LiteWideCollation: TLiteWideCollation);
begin
  GetConnection(Connection).GetCollationManager.RegisterWideCollation(Name, LiteWideCollation);
end;

class procedure TLiteUtils.UnRegisterWideCollation(Connection: TCustomDAConnection; Name: _string);
begin
  GetConnection(Connection).GetCollationManager.UnRegisterWideCollation(Name);
end;

class procedure TLiteUtils.RegisterFunction(Connection: TCustomDAConnection; Name: _string; ParamCount: Integer; LiteFunction: TLiteFunction);
begin
  GetConnection(Connection).GetFunctionManager.RegisterFunction(Name, ParamCount, LiteFunction);
end;

class procedure TLiteUtils.UnRegisterFunction(Connection: TCustomDAConnection; Name: _string; ParamCount: Integer);
begin
  GetConnection(Connection).GetFunctionManager.UnRegisterFunction(Name, ParamCount);
end;

class procedure TLiteUtils.EncryptDatabase(Connection: TCustomDAConnection; NewKey: _string);
begin
  GetConnection(Connection).EncryptDatabase(NewKey);
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TSQLiteUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TSQLiteUniProvider);

  LiteFunctions := _TStringList.Create;
  //LiteFunctions.AddObject('USER', TObject(PChar('')));
  LiteFunctions.AddObject('CHAR_LENGTH', TObject(PChar('LENGTH(%s)')));
  //LiteFunctions.AddObject('LOCATE', TObject(PChar('STRPOS(%1:s, %0:s)')));
  LiteFunctions.AddObject('SUBSTRING', TObject(PChar('SUBSTR(%s, %s, %s)')));
  LiteFunctions.AddObject('CONCAT', TObject(PChar('%s || %s')));
  //LiteFunctions.AddObject('CHAR', TObject(PChar('CHR(%s)')));
  LiteFunctions.AddObject('TRIM', TObject(PChar('TRIM(%s)')));
  //LiteFunctions.AddObject('TRUNCATE', TObject(PChar('TRUNC(%s, %s)')));
  //LiteFunctions.AddObject('CEILING', TObject(PChar('CEILING(%s)')));
  // Date-time
  LiteFunctions.AddObject('CURRENT_DATE', TObject(PChar('CURRENT_DATE')));
  //LiteFunctions.AddObject('YEAR', TObject(PChar('EXTRACT(YEAR FROM %s)')));
  //LiteFunctions.AddObject('MONTH', TObject(PChar('EXTRACT(MONTH FROM %s)')));
  //LiteFunctions.AddObject('DAY', TObject(PChar('EXTRACT(DAY FROM %s)')));
  LiteFunctions.AddObject('DATEADD', TObject(PChar('DATE(%2:s, ''%1:s %0:s'')')));
  //LiteFunctions.AddObject('DATEDIFF', TObject(PChar('EXTRACT(%s FROM (%2:s - %1:s))')));
  // Date-time literals
  LiteFunctions.AddObject('__DATE_TIME_LITERAL', TObject(PChar('%s')));
  LiteFunctions.AddObject('__DATE_LITERAL', TObject(PChar('%s')));
  LiteFunctions.AddObject('__TIME_LITERAL', TObject(PChar('%s')));
  // CONVERT functions
  LiteFunctions.AddObject('TODATE', TObject(PChar('CAST(%s AS VARCHAR)')));
  LiteFunctions.AddObject('TONUMBER', TObject(PChar('CAST(%s AS DOUBLE)')));
  LiteFunctions.AddObject('TOCHAR', TObject(PChar('CAST(%s AS VARCHAR)')));

  LiteMacros := _TStringList.Create;
  LiteMacros.AddObject('PROVIDER', TObject(PChar('SQLite')));
  LiteMacros.AddObject('SQLITE', TObject(PChar('')));
  // DataType macros
  LiteMacros.AddObject('DATETIME', TObject(PChar('TEXT')));
  LiteMacros.AddObject('DOUBLE', TObject(PChar('DOUBLE')));
  LiteMacros.AddObject('VARCHAR', TObject(PChar('VARCHAR')));

finalization
  UniProviders.UnRegisterProvider(TSQLiteUniProvider);

  LiteFunctions.Free;
  LiteMacros.Free;

end.
