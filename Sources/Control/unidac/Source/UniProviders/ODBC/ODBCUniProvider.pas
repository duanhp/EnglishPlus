
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I ODBCDac.inc}
unit ODBCUniProvider;
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
  TODBCUniProvider = class(TUniProvider)
  public
    class function GetProviderName: string; override;

    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;
    function IsInOutParamSupported: boolean; override;

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
  end;

  TODBCConnectDialogService = class(TConnectDialogService)
  end;

  TODBCFormatter = class(TUniSqlFormatter)
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
  ODBCClasses, ODBCParser, ODBCServices, ODBCConnectionPool;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCServicesUni, ODBCConnectionPoolUni;
{$ENDIF}

var
  ODBCFunctions, ODBCMacros: _TStringList;

class function TODBCUniProvider.GetProviderName: string;
begin
  Result := 'ODBC';
end;

function TODBCUniProvider.IsDatabaseSupported: boolean;
begin
  Result := False;
end;

function TODBCUniProvider.IsPortSupported: boolean;
begin
  Result := False;
end;

function TODBCUniProvider.IsInOutParamSupported: boolean;
begin
  Result := True;
end;

function TODBCUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TODBCParser;
end;

function TODBCUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TODBCConnectionParameters;
end;

function TODBCUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TODBCConnectionPoolManager;
end;

function TODBCUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TODBCConnection;
end;

function TODBCUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TODBCServerEnumerator;
end;

function TODBCUniProvider.GetTransactionClass: TCRTransactionClass;
begin
  Result := TODBCTransaction;
end;

function TODBCUniProvider.GetCommandClass: TCRCommandClass;
begin
  Result := TODBCCommand;
end;

function TODBCUniProvider.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TODBCRecordSet;
end;

function TODBCUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomODBCDataSetService;
end;

function TODBCUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TODBCScriptProcessor;
end;

function TODBCUniProvider.GetLoaderClass: TCRLoaderClass;
begin
  Result := TODBCLoader;
end;

function TODBCUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomODBCDumpProcessor;
end;

function TODBCUniProvider.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TODBCMetaData;
end;

function TODBCUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TODBCConnectDialogService;
end;

function TODBCUniProvider.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := TCustomODBCDataTypesMap;
end;

function TODBCUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TODBCFormatter;
end;

function TODBCUniProvider.GetParamObjectClass(Param: TDAParam): TClass;
begin
  case Param.DataType of
    ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}:
      Result := TCompressedBlob;
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

function TODBCUniProvider.CreateParamObject(Param: TDAParam): TSharedObject;
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

procedure TODBCUniProvider.SetObjectProps(Obj: TObject; Options: _TStrings; SetAllProps: boolean);
begin
  inherited;
end;

function TODBCUniProvider.GetConnectionOptions: TOptionsList;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TODBCConnection, TODBCConnectionParameters], 15));
    FConnectionOptions.Add(TEnumeratorOption.Create('DSNType', prDSNType, [TODBCConnection, TODBCConnectionParameters], Variant(ntAuto), TypeInfo(TDSNType)));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TODBCConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('DetectFieldsOnPrepare', prDetectFieldsOnPrepare, [TODBCConnection], True));
  end;
  Result := FConnectionOptions;
end;

function TODBCUniProvider.GetSQLOptions: TOptionsList;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TODBCCommand], 0));
  end;
  Result := FSQLOptions;
end;

function TODBCUniProvider.GetDataSetOptions: TOptionsList;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TODBCRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TODBCRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TODBCRecordSet], True));
  end;
  Result := FDataSetOptions;
end;

{ TODBCFormatter }

constructor TODBCFormatter.Create;
begin
  inherited;

  FFunctions := ODBCFunctions;
  FPredefinedMacros := ODBCMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TODBCUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TODBCUniProvider);

  ODBCFunctions := _TStringList.Create;
  ODBCFunctions.AddObject('USER', TObject(PChar('USER')));
  ODBCFunctions.AddObject('CHAR_LENGTH', TObject(PChar('LENGTH(%s)')));
  ODBCFunctions.AddObject('LOCATE', TObject(PChar('POSSTR(%1:s, %0:s)')));
  ODBCFunctions.AddObject('SUBSTRING', TObject(PChar('SUBSTR(%s, %s, %s)')));
  ODBCFunctions.AddObject('CONCAT', TObject(PChar('%s || %s')));
  ODBCFunctions.AddObject('CHAR', TObject(PChar('CHR(%s)')));
  ODBCFunctions.AddObject('TRIM', TObject(PChar('TRIM(%s)')));
  ODBCFunctions.AddObject('TRUNCATE', TObject(PChar('TRUNCATE(%s, %s)')));
  ODBCFunctions.AddObject('CEILING', TObject(PChar('CEILING(%s)')));
  // Date-time
  ODBCFunctions.AddObject('CURRENT_DATE', TObject(PChar('CURRENT_DATE')));
  ODBCFunctions.AddObject('YEAR', TObject(PChar('YEAR(%s)')));
  ODBCFunctions.AddObject('MONTH', TObject(PChar('MONTH(%s)')));
  ODBCFunctions.AddObject('DAY', TObject(PChar('DAY(%s)')));
  //ODBCFunctions.AddObject('DATEADD', TObject(PChar('DATE(%2:s, ''%1:s %0:s'')')));
  //ODBCFunctions.AddObject('DATEDIFF', TObject(PChar('EXTRACT(%s FROM (%2:s - %1:s))')));
  // Date-time literals
  ODBCFunctions.AddObject('__DATE_TIME_LITERAL', TObject(PChar('CAST(%s AS TIMESTAMP)')));
  ODBCFunctions.AddObject('__DATE_LITERAL', TObject(PChar('CAST(%s AS DATE)')));
  ODBCFunctions.AddObject('__TIME_LITERAL', TObject(PChar('CAST(%s AS TIME)')));
  // CONVERT functions
  ODBCFunctions.AddObject('TODATE', TObject(PChar('CAST(%s AS TIMESTAMP)')));
  ODBCFunctions.AddObject('TONUMBER', TObject(PChar('CAST(%s AS DOUBLE)')));
  ODBCFunctions.AddObject('TOCHAR', TObject(PChar('CAST(%s AS VARCHAR)')));

  ODBCMacros := _TStringList.Create;
  ODBCMacros.AddObject('PROVIDER', TObject(PChar('ODBC')));
  ODBCMacros.AddObject('ODBC', TObject(PChar('')));
  // DataType macros
  ODBCMacros.AddObject('DATETIME', TObject(PChar('TIMESTAMP')));
  ODBCMacros.AddObject('DOUBLE', TObject(PChar('DOUBLE')));
  ODBCMacros.AddObject('VARCHAR', TObject(PChar('VARCHAR')));

finalization
  UniProviders.UnRegisterProvider(TODBCUniProvider);

  ODBCFunctions.Free;
  ODBCMacros.Free;

end.
