
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I ADSDac.inc}
unit AdvantageUniProvider;
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
  TAdvantageUniProvider = class(TODBCUniProvider)
  public
    class function GetProviderName: string; override;

    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;
    function IsInOutParamSupported: boolean; override;
    function IsPoolingSupported: boolean; override;

    function GetConnectionParametersClass: TCRConnectionParametersClass; override;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; override;
    function GetConnectionClass: TCRConnectionClass; override;
    function GetCommandClass: TCRCommandClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetDataSetServiceClass: TDADataSetServiceClass; override;
    function GetScriptProcessorClass: TDAScriptProcessorClass; override;
    function GetLoaderClass: TCRLoaderClass; override;
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;

    function GetConnectionOptions: TOptionsList; override;
    function GetSQLOptions: TOptionsList; override;
    function GetDataSetOptions: TOptionsList; override;
  end;

  TADSConnectDialogService = class(TConnectDialogService)
  public
    function UseDatabaseHistory: boolean; override;
    function ServerEnabled: boolean; override;
  end;

  TADSFormatter = class(TUniSqlFormatter)
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
  ODBCClasses, ODBCParser, ODBCServices, ADSClasses, ADSServices,
  ADSConnectionPool;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCServicesUni, ADSClassesUni, ADSServicesUni,
  ADSConnectionPoolUni;
{$ENDIF}

var
  ADSFunctions, ADSMacros: _TStringList;

class function TAdvantageUniProvider.GetProviderName: string;
begin
  Result := 'Advantage';
end;

function TAdvantageUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TAdvantageUniProvider.IsPortSupported: boolean;
begin
  Result := False;
end;

function TAdvantageUniProvider.IsInOutParamSupported: boolean;
begin
  Result := False;
end;

function TAdvantageUniProvider.IsPoolingSupported: boolean;
begin
  Result := True;
end;

function TAdvantageUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TADSConnectionParameters;
end;

function TAdvantageUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TADSConnectionPoolManager;
end;

function TAdvantageUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TADSConnection;
end;

function TAdvantageUniProvider.GetCommandClass: TCRCommandClass;
begin
  Result := TADSCommand;
end;

function TAdvantageUniProvider.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TADSRecordSet;
end;

function TAdvantageUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomADSDataSetService;
end;

function TAdvantageUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TADSScriptProcessor;
end;

function TAdvantageUniProvider.GetLoaderClass: TCRLoaderClass;
begin
  Result := TADSLoader;
end;

function TAdvantageUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TADSConnectDialogService;
end;

function TAdvantageUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TADSFormatter;
end;

function TAdvantageUniProvider.GetConnectionOptions: TOptionsList;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TADSConnection, TADSConnectionParameters], 15));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TADSConnection], False));
    FConnectionOptions.Add(TStringOption.Create('ServerTypes', prADSServerTypes, [TADSConnection, TADSConnectionParameters], 'ADS,AIS'));
  end;
  Result := FConnectionOptions;
end;

function TAdvantageUniProvider.GetSQLOptions: TOptionsList;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TADSCommand], 0));
  end;
  Result := FSQLOptions;
end;

function TAdvantageUniProvider.GetDataSetOptions: TOptionsList;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TADSRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TADSRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TADSRecordSet], False));
  end;
  Result := FDataSetOptions;
end;

{ TADSConnectDialogService }

function TADSConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := True;
end;

function TADSConnectDialogService.ServerEnabled: boolean;
begin
  Result := False;
end;

{ TADSFormatter }

constructor TADSFormatter.Create;
begin
  inherited;

  FFunctions := ADSFunctions;
  FPredefinedMacros := ADSMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TAdvantageUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TAdvantageUniProvider);

  ADSFunctions := _TStringList.Create;
  ADSFunctions.AddObject('USER', TObject(PChar('USER')));
  ADSFunctions.AddObject('CHAR_LENGTH', TObject(PChar('LENGTH(%s)')));
  ADSFunctions.AddObject('LOCATE', TObject(PChar('POSSTR(%1:s, %0:s)')));
  ADSFunctions.AddObject('SUBSTRING', TObject(PChar('SUBSTR(%s, %s, %s)')));
  ADSFunctions.AddObject('CONCAT', TObject(PChar('%s || %s')));
  ADSFunctions.AddObject('CHAR', TObject(PChar('CHR(%s)')));
  ADSFunctions.AddObject('TRIM', TObject(PChar('TRIM(%s)')));
  ADSFunctions.AddObject('TRUNCATE', TObject(PChar('TRUNCATE(%s, %s)')));
  ADSFunctions.AddObject('CEILING', TObject(PChar('CEILING(%s)')));
  // Date-time
  ADSFunctions.AddObject('CURRENT_DATE', TObject(PChar('CURRENT_DATE')));
  ADSFunctions.AddObject('YEAR', TObject(PChar('YEAR(%s)')));
  ADSFunctions.AddObject('MONTH', TObject(PChar('MONTH(%s)')));
  ADSFunctions.AddObject('DAY', TObject(PChar('DAY(%s)')));
  //ADSFunctions.AddObject('DATEADD', TObject(PChar('DATE(%2:s, ''%1:s %0:s'')')));
  //ADSFunctions.AddObject('DATEDIFF', TObject(PChar('EXTRACT(%s FROM (%2:s - %1:s))')));
  // Date-time literals
  ADSFunctions.AddObject('__DATE_TIME_LITERAL', TObject(PChar('CAST(%s AS TIMESTAMP)')));
  ADSFunctions.AddObject('__DATE_LITERAL', TObject(PChar('CAST(%s AS DATE)')));
  ADSFunctions.AddObject('__TIME_LITERAL', TObject(PChar('CAST(%s AS TIME)')));
  // CONVERT functions
  ADSFunctions.AddObject('TODATE', TObject(PChar('CAST(%s AS TIMESTAMP)')));
  ADSFunctions.AddObject('TONUMBER', TObject(PChar('CAST(%s AS DOUBLE)')));
  ADSFunctions.AddObject('TOCHAR', TObject(PChar('CAST(%s AS VARCHAR)')));

  ADSMacros := _TStringList.Create;
  ADSMacros.AddObject('PROVIDER', TObject(PChar('Advantage')));
  ADSMacros.AddObject('ADVANTAGE', TObject(PChar('')));
  // DataType macros
  ADSMacros.AddObject('DATETIME', TObject(PChar('TIMESTAMP')));
  ADSMacros.AddObject('DOUBLE', TObject(PChar('DOUBLE')));
  ADSMacros.AddObject('VARCHAR', TObject(PChar('VARCHAR')));

finalization
  UniProviders.UnRegisterProvider(TAdvantageUniProvider);

  ADSFunctions.Free;
  ADSMacros.Free;

end.
