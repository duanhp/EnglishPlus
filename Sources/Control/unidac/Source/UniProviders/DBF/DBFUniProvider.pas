
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I DBFDac.inc}
unit DBFUniProvider;
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
  TDBFUniProvider = class(TODBCUniProvider)
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

  TDBFConnectDialogService = class(TConnectDialogService)
  public
    function UseDatabaseHistory: boolean; override;
    function ServerEnabled: boolean; override;
  end;

  TDBFFormatter = class(TUniSqlFormatter)
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
  ODBCClasses, ODBCParser, ODBCServices, DBFClasses, DBFServices;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCServicesUni, DBFClassesUni, DBFServicesUni;
{$ENDIF}

var
  DBFFunctions, DBFMacros: _TStringList;

class function TDBFUniProvider.GetProviderName: string;
begin
  Result := 'DBF';
end;

function TDBFUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TDBFUniProvider.IsPortSupported: boolean;
begin
  Result := False;
end;

function TDBFUniProvider.IsInOutParamSupported: boolean;
begin
  Result := False;
end;

function TDBFUniProvider.IsPoolingSupported: boolean;
begin
  Result := False;
end;

function TDBFUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TCRConnectionParameters;
end;

function TDBFUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TCRConnectionPoolManager;
end;

function TDBFUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TDBFConnection;
end;

function TDBFUniProvider.GetCommandClass: TCRCommandClass;
begin
  Result := TDBFCommand;
end;

function TDBFUniProvider.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TDBFRecordSet;
end;

function TDBFUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomDBFDataSetService;
end;

function TDBFUniProvider.GetLoaderClass: TCRLoaderClass;
begin
  Result := TDBFLoader;
end;

function TDBFUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TDBFConnectDialogService;
end;

function TDBFUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TDBFFormatter;
end;

function TDBFUniProvider.GetConnectionOptions: TOptionsList;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TStringOption.Create('CollatingSequence', prCollatingSequence, [TDBFConnection], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TDBFConnection], 15));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TDBFConnection], False));
  end;
  Result := FConnectionOptions;
end;

function TDBFUniProvider.GetSQLOptions: TOptionsList;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TDBFCommand], 0));
  end;
  Result := FSQLOptions;
end;

function TDBFUniProvider.GetDataSetOptions: TOptionsList;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TDBFRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TDBFRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TDBFRecordSet], True));
  end;
  Result := FDataSetOptions;
end;

{ TDBFConnectDialogService }

function TDBFConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := True;
end;

function TDBFConnectDialogService.ServerEnabled: boolean;
begin
  Result := False;
end;

{ TDBFFormatter }

constructor TDBFFormatter.Create;
begin
  inherited;

  FFunctions := DBFFunctions;
  FPredefinedMacros := DBFMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TDBFUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TDBFUniProvider);

  DBFFunctions := _TStringList.Create;
  DBFFunctions.AddObject('USER', TObject(PChar('USER')));
  DBFFunctions.AddObject('CHAR_LENGTH', TObject(PChar('LEN(%s)')));
  DBFFunctions.AddObject('LOCATE', TObject(PChar('POSSTR(%1:s, %0:s)')));
  DBFFunctions.AddObject('SUBSTRING', TObject(PChar('SUBSTR(%s, %s, %s)')));
  DBFFunctions.AddObject('CONCAT', TObject(PChar('%s + %s')));
  DBFFunctions.AddObject('CHAR', TObject(PChar('CHAR(%s)')));
  DBFFunctions.AddObject('TRIM', TObject(PChar('TRIM(%s)')));
  DBFFunctions.AddObject('TRUNCATE', TObject(PChar('TRUNC(%s, %s)')));
  DBFFunctions.AddObject('CEILING', TObject(PChar('CEILING(%s)')));
  // Date-time
  DBFFunctions.AddObject('CURRENT_DATE', TObject(PChar('CURRENT_DATE')));
  DBFFunctions.AddObject('YEAR', TObject(PChar('YEAR(%s)')));
  DBFFunctions.AddObject('MONTH', TObject(PChar('MONTH(%s)')));
  DBFFunctions.AddObject('DAY', TObject(PChar('DAY(%s)')));
  //DBFFunctions.AddObject('DATEADD', TObject(PChar('DATE(%2:s, ''%1:s %0:s'')')));
  //DBFFunctions.AddObject('DATEDIFF', TObject(PChar('EXTRACT(%s FROM (%2:s - %1:s))')));
  // Date-time literals
  DBFFunctions.AddObject('__DATE_TIME_LITERAL', TObject(PChar('(%s)')));
  DBFFunctions.AddObject('__DATE_LITERAL', TObject(PChar('(%s)')));
  DBFFunctions.AddObject('__TIME_LITERAL', TObject(PChar('(%s)')));
  // CONVERT functions
  DBFFunctions.AddObject('TODATE', TObject(PChar('(%s)')));
  DBFFunctions.AddObject('TONUMBER', TObject(PChar('(%s)')));
  DBFFunctions.AddObject('TOCHAR', TObject(PChar('(%s)')));

  DBFMacros := _TStringList.Create;
  DBFMacros.AddObject('PROVIDER', TObject(PChar('DBF')));
  DBFMacros.AddObject('DBF', TObject(PChar('')));
  // DataType macros
  DBFMacros.AddObject('DATETIME', TObject(PChar('DATE')));
  DBFMacros.AddObject('DOUBLE', TObject(PChar('DOUBLE')));
  DBFMacros.AddObject('VARCHAR', TObject(PChar('TEXT')));

finalization
  UniProviders.UnRegisterProvider(TDBFUniProvider);

  DBFFunctions.Free;
  DBFMacros.Free;

end.
