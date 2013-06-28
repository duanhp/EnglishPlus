
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I DB2Dac.inc}
unit DB2UniProvider;
{$ENDIF}

interface

uses
  SysUtils, Classes, Variants, DB, CRAccess, CRConnectionPool, MemData,
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess, MemUtils, CRParser,
  DAScript, DADump, Uni, UniProvider, ODBCUniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TDB2UniProvider = class(TODBCUniProvider)
  public
    class function GetProviderName: string; override;

    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;
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
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;

    function GetConnectionOptions: TOptionsList; override;
    function GetSQLOptions: TOptionsList; override;
    function GetDataSetOptions: TOptionsList; override;
  end;

  TDB2Formatter = class(TUniSqlFormatter)
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
  DAConsts, UniConsts,
{$IFNDEF UNIDACPRO}
  ODBCClasses, ODBCParser, ODBCServices, DB2Classes, DB2Services,
  DB2ConnectionPool;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCServicesUni, DB2ClassesUni, DB2ServicesUni,
  DB2ConnectionPoolUni;
{$ENDIF}

var
  DB2Functions, DB2Macros: _TStringList;

class function TDB2UniProvider.GetProviderName: string;
begin
  Result := 'DB2';
end;

function TDB2UniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TDB2UniProvider.IsPortSupported: boolean;
begin
  Result := True;
end;

function TDB2UniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TDB2ConnectionParameters;
end;

function TDB2UniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TDB2ConnectionPoolManager;
end;

function TDB2UniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TDB2Connection;
end;

function TDB2UniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TDB2ServerEnumerator;
end;

function TDB2UniProvider.GetTransactionClass: TCRTransactionClass;
begin
  Result := TDB2Transaction;
end;

function TDB2UniProvider.GetCommandClass: TCRCommandClass;
begin
  Result := TDB2Command;
end;

function TDB2UniProvider.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TDB2RecordSet;
end;

function TDB2UniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomDB2DataSetService;
end;

function TDB2UniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TDB2ScriptProcessor;
end;

function TDB2UniProvider.GetLoaderClass: TCRLoaderClass;
begin
  Result := TDB2Loader;
end;

function TDB2UniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TDB2Formatter;
end;

function TDB2UniProvider.GetConnectionOptions: TOptionsList;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TDB2Connection, TDB2ConnectionParameters], 15));
    FConnectionOptions.Add(TStringOption.Create('FunctionPath', prFunctionPath, [TDB2Connection, TDB2ConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('Schema', prSchema, [TDB2Connection, TDB2ConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TDB2Connection], False));
  end;
  Result := FConnectionOptions;
end;

function TDB2UniProvider.GetSQLOptions: TOptionsList;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TDB2Command], 0));
  end;
  Result := FSQLOptions;
end;

function TDB2UniProvider.GetDataSetOptions: TOptionsList;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TDB2RecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TDB2RecordSet], True));
    FDataSetOptions.Add(TStringOption.Create('KeySequence', prKeySequence, [TCustomDB2DataSetService], ''));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TDB2RecordSet], False));
    FDataSetOptions.Add(TEnumeratorOption.Create('SequenceMode', prSequenceMode, [TCustomDB2DataSetService], Variant(smPost), TypeInfo(TDB2SequenceMode)));
  end;
  Result := FDataSetOptions;
end;

{ TDB2Formatter }

constructor TDB2Formatter.Create;
begin
  inherited;

  FFunctions := DB2Functions;
  FPredefinedMacros := DB2Macros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TDB2UniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TDB2UniProvider);

  DB2Functions := _TStringList.Create;
  DB2Functions.AddObject('USER', TObject(PChar('USER')));
  DB2Functions.AddObject('CHAR_LENGTH', TObject(PChar('LENGTH(%s)')));
  DB2Functions.AddObject('LOCATE', TObject(PChar('POSSTR(%1:s, %0:s)')));
  DB2Functions.AddObject('SUBSTRING', TObject(PChar('SUBSTR(%s, %s, %s)')));
  DB2Functions.AddObject('CONCAT', TObject(PChar('%s || %s')));
  DB2Functions.AddObject('CHAR', TObject(PChar('CHR(%s)')));
  DB2Functions.AddObject('TRIM', TObject(PChar('TRIM(%s)')));
  DB2Functions.AddObject('TRUNCATE', TObject(PChar('TRUNCATE(%s, %s)')));
  DB2Functions.AddObject('CEILING', TObject(PChar('CEILING(%s)')));
  // Date-time
  DB2Functions.AddObject('CURRENT_DATE', TObject(PChar('CURRENT_DATE')));
  DB2Functions.AddObject('YEAR', TObject(PChar('YEAR(%s)')));
  DB2Functions.AddObject('MONTH', TObject(PChar('MONTH(%s)')));
  DB2Functions.AddObject('DAY', TObject(PChar('DAY(%s)')));
  //DB2Functions.AddObject('DATEADD', TObject(PChar('DATE(%2:s, ''%1:s %0:s'')')));
  //DB2Functions.AddObject('DATEDIFF', TObject(PChar('EXTRACT(%s FROM (%2:s - %1:s))')));
  // Date-time literals
  DB2Functions.AddObject('__DATE_TIME_LITERAL', TObject(PChar('CAST(%s AS TIMESTAMP)')));
  DB2Functions.AddObject('__DATE_LITERAL', TObject(PChar('CAST(%s AS DATE)')));
  DB2Functions.AddObject('__TIME_LITERAL', TObject(PChar('CAST(%s AS TIME)')));
  // CONVERT functions
  DB2Functions.AddObject('TODATE', TObject(PChar('CAST(%s AS TIMESTAMP)')));
  DB2Functions.AddObject('TONUMBER', TObject(PChar('CAST(%s AS DOUBLE)')));
  DB2Functions.AddObject('TOCHAR', TObject(PChar('CAST(%s AS VARCHAR)')));

  DB2Macros := _TStringList.Create;
  DB2Macros.AddObject('PROVIDER', TObject(PChar('DB2')));
  DB2Macros.AddObject('DB2', TObject(PChar('')));
  // DataType macros
  DB2Macros.AddObject('DATETIME', TObject(PChar('TIMESTAMP')));
  DB2Macros.AddObject('DOUBLE', TObject(PChar('DOUBLE')));
  DB2Macros.AddObject('VARCHAR', TObject(PChar('VARCHAR')));

finalization
  UniProviders.UnRegisterProvider(TDB2UniProvider);

  DB2Functions.Free;
  DB2Macros.Free;

end.
