
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I ASEDac.inc}
unit ASEUniProvider;
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
  TASEUniProvider = class(TODBCUniProvider)
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
    function GetMetaDataClass: TCRMetaDataClass; override;
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;

    function GetConnectionOptions: TOptionsList; override;
    function GetSQLOptions: TOptionsList; override;
    function GetDataSetOptions: TOptionsList; override;
  end;

  TASEConnectDialogService = class(TConnectDialogService)
  public
  end;

  TASEFormatter = class(TUniSqlFormatter)
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
  ODBCClasses, ODBCParser, ODBCServices, ASEClasses, ASEServices,
  ASEParser, ASEConnectionPool;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCServicesUni, ASEClassesUni, ASEServicesUni,
  ASEParserUni, ASEConnectionPoolUni;
{$ENDIF}

var
  ASEFunctions, ASEMacros: _TStringList;

class function TASEUniProvider.GetProviderName: string;
begin
  Result := 'ASE';
end;

function TASEUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TASEUniProvider.IsPortSupported: boolean;
begin
  Result := True;
end;

function TASEUniProvider.IsInOutParamSupported: boolean;
begin
  Result := True;
end;

function TASEUniProvider.IsPoolingSupported: boolean;
begin
  Result := True;
end;

function TASEUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TASEParser;
end;

function TASEUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TASEConnectionParameters;
end;

function TASEUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TASEConnectionPoolManager;
end;

function TASEUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TASEConnection;
end;

function TASEUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TASEServerEnumerator;
end;

function TASEUniProvider.GetTransactionClass: TCRTransactionClass;
begin
  Result := TASETransaction;
end;

function TASEUniProvider.GetCommandClass: TCRCommandClass;
begin
  Result := TASECommand;
end;

function TASEUniProvider.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TASERecordSet;
end;

function TASEUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomASEDataSetService;
end;

function TASEUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TASEScriptProcessor;
end;

function TASEUniProvider.GetLoaderClass: TCRLoaderClass;
begin
  Result := TASELoader;
end;

function TASEUniProvider.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TASEMetaData;
end;

function TASEUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TASEConnectDialogService;
end;

function TASEUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TASEFormatter;
end;

function TASEUniProvider.GetConnectionOptions: TOptionsList;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TASEConnection], 15));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TASEConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('AnsiNull', prAnsiNull, [TASEConnection], True));
    FConnectionOptions.Add(TStringOption.Create('ApplicationName', prApplicationName, [TASEConnection], ''));
  end;
  Result := FConnectionOptions;
end;

function TASEUniProvider.GetSQLOptions: TOptionsList;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TASECommand], 0));
  end;
  Result := FSQLOptions;
end;

function TASEUniProvider.GetDataSetOptions: TOptionsList;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TASERecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TASERecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TASERecordSet], False));
  end;
  Result := FDataSetOptions;
end;

{ TASEFormatter }

constructor TASEFormatter.Create;
begin
  inherited;

  FFunctions := ASEFunctions;
  FPredefinedMacros := ASEMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TASEUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TASEUniProvider);

  ASEFunctions := _TStringList.Create;
  ASEFunctions.AddObject('USER', TObject(PChar('USER')));
  ASEFunctions.AddObject('CHAR_LENGTH', TObject(PChar('CHAR_LENGTH(%s)')));
  ASEFunctions.AddObject('LOCATE', TObject(PChar('CHARINDEX(%s, %s)')));
  ASEFunctions.AddObject('SUBSTRING', TObject(PChar('SUBSTRING(%s, %s, %s)')));
  ASEFunctions.AddObject('CONCAT', TObject(PChar('%s + %s')));
  ASEFunctions.AddObject('CHAR', TObject(PChar('CHAR(%s)')));
  ASEFunctions.AddObject('TRIM', TObject(PChar('LTRIM(RTRIM(%s))')));
  ASEFunctions.AddObject('TRUNCATE', TObject(PChar('ROUND(%s, %s)')));
  ASEFunctions.AddObject('CEILING', TObject(PChar('CEILING(%s)')));
  // Date-time
  ASEFunctions.AddObject('CURRENT_DATE', TObject(PChar('GETDATE')));
  ASEFunctions.AddObject('YEAR', TObject(PChar('YEAR(%s)')));
  ASEFunctions.AddObject('MONTH', TObject(PChar('MONTH(%s)')));
  ASEFunctions.AddObject('DAY', TObject(PChar('DAY(%s)')));
  ASEFunctions.AddObject('DATEADD', TObject(PChar('DATEADD(%s, %s, %s)')));
  ASEFunctions.AddObject('DATEDIFF', TObject(PChar('DATEDIFF(%s, %s, %s)')));
  // Date-time literals
  ASEFunctions.AddObject('__DATE_TIME_LITERAL', TObject(PChar('CONVERT(DATETIME, %s)')));
  ASEFunctions.AddObject('__DATE_LITERAL', TObject(PChar('CONVERT(DATETIME, %s)')));
  ASEFunctions.AddObject('__TIME_LITERAL', TObject(PChar('CONVERT(DATETIME, %s)')));
  // CONVERT functions
  ASEFunctions.AddObject('TODATE', TObject(PChar('CONVERT(DATETIME, %s)')));
  ASEFunctions.AddObject('TONUMBER', TObject(PChar('CONVERT(FLOAT(53), %s)')));
  ASEFunctions.AddObject('TOCHAR', TObject(PChar('CONVERT(VARCHAR, %s, 20)')));

  ASEMacros := _TStringList.Create;
  ASEMacros.AddObject('PROVIDER', TObject(PChar('ASE')));
  ASEMacros.AddObject('ASE', TObject(PChar('')));
  // DataType macros
  ASEMacros.AddObject('DATETIME', TObject(PChar('DATETIME')));
  ASEMacros.AddObject('DOUBLE', TObject(PChar('FLOAT(53)')));
  ASEMacros.AddObject('VARCHAR', TObject(PChar('VARCHAR')));

finalization
  UniProviders.UnRegisterProvider(TASEUniProvider);

  ASEFunctions.Free;
  ASEMacros.Free;

end.
