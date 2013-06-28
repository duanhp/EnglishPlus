
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I AccessDac.inc}
unit AccessUniProvider;
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
  TAccessUniProvider = class(TODBCUniProvider)
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
    function GetLoaderClass: TCRLoaderClass; override;
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;

    function GetConnectionOptions: TOptionsList; override;
    function GetSQLOptions: TOptionsList; override;
    function GetDataSetOptions: TOptionsList; override;
  end;

  TAccessConnectDialogService = class(TConnectDialogService)
  public
    function UseDatabaseHistory: boolean; override;
    function ServerEnabled: boolean; override;
  end;

  TAccessFormatter = class(TUniSqlFormatter)
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
  ODBCClasses, ODBCParser, ODBCServices, AccessClasses, AccessServices;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCServicesUni, AccessClassesUni, AccessServicesUni;
{$ENDIF}

var
  AccessFunctions, AccessMacros: _TStringList;

class function TAccessUniProvider.GetProviderName: string;
begin
  Result := 'Access';
end;

function TAccessUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TAccessUniProvider.IsPortSupported: boolean;
begin
  Result := False;
end;

function TAccessUniProvider.IsInOutParamSupported: boolean;
begin
  Result := False;
end;

function TAccessUniProvider.IsPoolingSupported: boolean;
begin
  Result := False;
end;

function TAccessUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TCRConnectionParameters;
end;

function TAccessUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TCRConnectionPoolManager;
end;

function TAccessUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TAccessConnection;
end;

function TAccessUniProvider.GetCommandClass: TCRCommandClass;
begin
  Result := TAccessCommand;
end;

function TAccessUniProvider.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TAccessRecordSet;
end;

function TAccessUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomAccessDataSetService;
end;

function TAccessUniProvider.GetLoaderClass: TCRLoaderClass;
begin
  Result := TAccessLoader;
end;

function TAccessUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TAccessConnectDialogService;
end;

function TAccessUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TAccessFormatter;
end;

function TAccessUniProvider.GetConnectionOptions: TOptionsList;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TAccessConnection], 15));
    FConnectionOptions.Add(TBooleanOption.Create('ExclusiveLock', prExclusiveLock, [TAccessConnection], False));
    FConnectionOptions.Add(TIntegerOption.Create('ExtendedAnsiSQL', prExtendedAnsiSQL, [TAccessConnection], 0));
    FConnectionOptions.Add(TStringOption.Create('SystemDatabase', prSystemDatabase, [TAccessConnection], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TAccessConnection], False));
  end;
  Result := FConnectionOptions;
end;

function TAccessUniProvider.GetSQLOptions: TOptionsList;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TAccessCommand], 0));
  end;
  Result := FSQLOptions;
end;

function TAccessUniProvider.GetDataSetOptions: TOptionsList;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TAccessRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TAccessRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TAccessRecordSet], True));
  end;
  Result := FDataSetOptions;
end;

{ TAccessConnectDialogService }

function TAccessConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := True;
end;

function TAccessConnectDialogService.ServerEnabled: boolean;
begin
  Result := False;
end;

{ TAccessFormatter }

constructor TAccessFormatter.Create;
begin
  inherited;

  FFunctions := AccessFunctions;
  FPredefinedMacros := AccessMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TAccessUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TAccessUniProvider);

  AccessFunctions := _TStringList.Create;
  AccessFunctions.AddObject('USER', TObject(PChar('USER')));
  AccessFunctions.AddObject('CHAR_LENGTH', TObject(PChar('LEN(%s)')));
  AccessFunctions.AddObject('LOCATE', TObject(PChar('POSSTR(%1:s, %0:s)')));
  AccessFunctions.AddObject('SUBSTRING', TObject(PChar('SUBSTR(%s, %s, %s)')));
  AccessFunctions.AddObject('CONCAT', TObject(PChar('%s + %s')));
  AccessFunctions.AddObject('CHAR', TObject(PChar('CHAR(%s)')));
  AccessFunctions.AddObject('TRIM', TObject(PChar('TRIM(%s)')));
  AccessFunctions.AddObject('TRUNCATE', TObject(PChar('TRUNC(%s, %s)')));
  AccessFunctions.AddObject('CEILING', TObject(PChar('CEILING(%s)')));
  // Date-time
  AccessFunctions.AddObject('CURRENT_DATE', TObject(PChar('CURRENT_DATE')));
  AccessFunctions.AddObject('YEAR', TObject(PChar('YEAR(%s)')));
  AccessFunctions.AddObject('MONTH', TObject(PChar('MONTH(%s)')));
  AccessFunctions.AddObject('DAY', TObject(PChar('DAY(%s)')));
  //AccessFunctions.AddObject('DATEADD', TObject(PChar('DATE(%2:s, ''%1:s %0:s'')')));
  //AccessFunctions.AddObject('DATEDIFF', TObject(PChar('EXTRACT(%s FROM (%2:s - %1:s))')));
  // Date-time literals
  AccessFunctions.AddObject('__DATE_TIME_LITERAL', TObject(PChar('(%s)')));
  AccessFunctions.AddObject('__DATE_LITERAL', TObject(PChar('(%s)')));
  AccessFunctions.AddObject('__TIME_LITERAL', TObject(PChar('(%s)')));
  // CONVERT functions
  AccessFunctions.AddObject('TODATE', TObject(PChar('(%s)')));
  AccessFunctions.AddObject('TONUMBER', TObject(PChar('(%s)')));
  AccessFunctions.AddObject('TOCHAR', TObject(PChar('(%s)')));

  AccessMacros := _TStringList.Create;
  AccessMacros.AddObject('PROVIDER', TObject(PChar('Access')));
  AccessMacros.AddObject('ACCESS', TObject(PChar('')));
  // DataType macros
  AccessMacros.AddObject('DATETIME', TObject(PChar('DATE')));
  AccessMacros.AddObject('DOUBLE', TObject(PChar('DOUBLE')));
  AccessMacros.AddObject('VARCHAR', TObject(PChar('TEXT')));

finalization
  UniProviders.UnRegisterProvider(TAccessUniProvider);

  AccessFunctions.Free;
  AccessMacros.Free;

end.
