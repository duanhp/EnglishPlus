
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I UniDac.inc}

unit UniProvider;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, Variants, CRAccess, CRConnectionPool, DB, MemData,
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess,
  DAScript, DADump, MemUtils, CRParser, TypInfo;

type
  TOptionsList = class;
  TConnectDialogService = class;
  TUniSqlFormatter = class;

  TConnectDialogServiceClass = class of TConnectDialogService;
  TUniSqlFormatterClass = class of TUniSqlFormatter;

  TUniProvider = class(TComponent)
  protected
    FConnectionOptions: TOptionsList;
    FSQLOptions: TOptionsList;
    FDataSetOptions: TOptionsList;
    FScriptOptions: TOptionsList;
    FLoaderOptions: TOptionsList;
    FDumpOptions: TOptionsList;
    FAlerterOptions: TOptionsList;
  public
    destructor Destroy; override;

    class function GetProviderName: string; virtual; // CLR does not support abstract class methods

    function IsDatabaseSupported: boolean; virtual;
    function IsPortSupported: boolean; virtual;
    function IsDataSetNeedTransaction: boolean; virtual;
    function IsInOutParamSupported: boolean; virtual;
    function IsPoolingSupported: boolean; virtual;
    function NeedRecreateProcCall: boolean; virtual;

    function GetConnectionParametersClass: TCRConnectionParametersClass; virtual; abstract;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; virtual; abstract;
    function GetConnectionClass: TCRConnectionClass; virtual; abstract;
    function GetServerEnumeratorClass: TCRServerEnumeratorClass; virtual; abstract;
    function GetTransactionClass: TCRTransactionClass; virtual; abstract;
    function GetParserClass: TSQLParserClass; virtual; abstract;
    function GetCommandClass: TCRCommandClass; virtual; abstract;
    function GetRecordSetClass: TCRRecordSetClass; virtual; abstract;
    function GetDataSetServiceClass: TDADataSetServiceClass; virtual; abstract;
    function GetScriptProcessorClass: TDAScriptProcessorClass; virtual; abstract;
    function GetLoaderClass: TCRLoaderClass; virtual; abstract;
    function GetAlerterClass: TCRAlerterClass; virtual;
    function GetDumpProcessorClass: TDADumpProcessorClass; virtual; abstract;
    function GetDataTypesMap: TDataTypesMapClass; virtual; abstract;
    function GetMetaDataClass: TCRMetaDataClass; virtual; abstract;
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; virtual; abstract;
    function GetSqlFormatterClass: TUniSqlFormatterClass; virtual; abstract;
    function GetParamObjectClass(Param: TDAParam): TClass; virtual;
    function CreateParamObject(Param: TDAParam): TSharedObject; virtual;

    procedure SetObjectProps(Obj: TObject; Options: _TStrings; SetAllProps: boolean); virtual;

    function GetConnectionOptions: TOptionsList; virtual; abstract;
    function GetSQLOptions: TOptionsList; virtual; abstract;
    function GetDataSetOptions: TOptionsList; virtual; abstract;
    function GetScriptOptions: TOptionsList; virtual;
    function GetLoaderOptions: TOptionsList; virtual;
    function GetDumpOptions: TOptionsList; virtual;
    function GetAlerterOptions: TOptionsList; virtual;
    function DefaultTableSchema: _string; virtual;
  end;

  TUniProviderClass = class of TUniProvider;

  TUniProviderDesc = class
  private
    FProviderName: string;
    FProviderShortName: string;
    FPackageName: string;
    FAssemblyName: string;
    FSiblingProduct: string;

    FProvider: TUniProvider;

    function GetUnitName: string;
    function GetUnitNameCLR: string;
    function GetProviderComponentName: string;
  public
    property ProviderName: string read FProviderName;
    property ProviderShortName: string read FProviderShortName;

    property PackageName: string read FPackageName;
    property AssemblyName: string read FAssemblyName;

    property SiblingProduct: string read FSiblingProduct;

    property ProviderUnitName: string read GetUnitName;
    property ProviderUnitNameCLR: string read GetUnitNameCLR;
    property ProviderComponentName: string read GetProviderComponentName;

    property Provider: TUniProvider read FProvider;
  end;

  TUniProviders = class(TThreadList)
  private
    function FindProviderDesc(ProviderName: string): TUniProviderDesc;
    procedure RegisterProviderDesc(ProviderName, ProviderShortName,
      PackageName, AssemblyName, SiblingProduct: string);
  public
    destructor Destroy; override;

    procedure RegisterProvider(UniProviderClass: TUniProviderClass);
    procedure UnRegisterProvider(UniProviderClass: TUniProviderClass);

    function GetProviderDesc(ProviderName: string): TUniProviderDesc;
    function GetProvider(ProviderName: string): TUniProvider;

    procedure GetProviderNames(Names: TStrings);
  end;

  TConnectDialogService = class
  public
    constructor Create; virtual;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    function GetConnectMode: integer; virtual;
    function UseDatabaseHistory: boolean; virtual;
    function GetDefaultDatabase: _string; virtual;
    function UsernameEnabled: boolean; virtual;
    function PasswordEnabled: boolean; virtual;
    function ServerEnabled: boolean; virtual;
    function DatabaseEnabled: boolean; virtual;
    function PortEnabled: boolean; virtual;
  end;

  TCRDummyAlerter = class(TCRAlerter)
  public
    procedure SendEvent(const EventName, Message: _string); override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TOnAssignValue = procedure(InternalObject: TObject; Value: variant) of object;
  TSetPropFunc = function(Prop: integer; const Value: variant): boolean of object;
  TOnGetValuesList = procedure(List: _TStrings) of object;
  TClassArray = array of TClass;

  TOption = class
  private
    FOptionName: _string;
    FInternalIndex: integer;
    FInternalClasses: TClassArray;
    FDefaultValue: variant;
    FOnGetValuesList: TOnGetValuesList;
    FOnAssignValue: TOnAssignValue;
  protected

    procedure ValidationError(const Value: _string);

    procedure InternalGetValuesList(List: _TStrings); virtual;
  public
    constructor Create(const OptionName: _string; InternalIndex: integer; InternalClasses: array of TClass; DefaultValue: variant); overload;

    function GetDefaultValue: variant;

    function GetAsString(const Value: variant): _string; virtual;
    function GetAsNative(const Value: _string): variant; virtual;

    function CheckValue(const Value: _string): boolean; virtual;
    procedure Validate(const Value: _string); virtual;
    procedure GetValuesList(List: _TStrings); virtual;

    property OptionName: _string read FOptionName;
    property InternalIndex: integer read FInternalIndex;
    property InternalClasses: TClassArray read FInternalClasses;

    property OnGetValuesList: TOnGetValuesList read FOnGetValuesList write FOnGetValuesList;
    property OnAssignValue: TOnAssignValue read FOnAssignValue write FOnAssignValue;
  end;

  TIntegerOption = class(TOption)
  public
    function GetAsString(const Value: variant): _string; override;
    function GetAsNative(const Value: _string): variant; override;

    function CheckValue(const Value: _string): boolean; override;
  end;

  TStringOption = class(TOption)
  end;

  TBooleanOption = class(TOption)
  protected
    procedure InternalGetValuesList(List: _TStrings); override;
  public
    function GetAsString(const Value: variant): _string; override;
    function GetAsNative(const Value: _string): variant; override;

    function CheckValue(const Value: _string): boolean; override;
  end;

  TEnumeratorOption = class(TOption)
  private
    FTypeInfo: PTypeInfo;
    FMinValue: integer;
    FMaxValue: integer;
    FInternalType: boolean;
  protected
    procedure InternalGetValuesList(List: _TStrings); override;
  public
    constructor Create(const OptionName: _string; InternalIndex: integer; InternalClasses: array of TClass; DefaultValue: variant; TypeInfo: PTypeInfo);

    function GetAsString(const Value: variant): _string; override;
    function GetAsNative(const Value: _string): variant; override;

    function CheckValue(const Value: _string): boolean; override;
  end;

  TOptionsList = class(_TStringList)
  private
    FPrefix: _string;
    
    function GetOption(Index: integer): TOption;
    procedure SetOption(Index: integer; const Value: TOption);
  public
    constructor Create(const Prefix: _string);
    destructor Destroy; override;

    procedure Add(Value: TOption); reintroduce;
    function OptionByName(const Name: _string): TOption;

    procedure ImportOptions(Source: _TStrings; DestObject: TObject; SetPropFunc: TSetPropFunc; SetAll: boolean);
    procedure ExportDefOptions(Dest: _TStrings);

    function GetValueByName(Source: _TStrings; const Name: _string): variant;

    property Prefix: _string read FPrefix;
    property Items[Index: integer]: TOption read GetOption write SetOption; default;
  end;

  _TStringArray = array of _string;

  TUniSqlFormatter = class
  protected
    FFunctions: _TStringList;
    FPredefinedMacros: _TStringList;
    FUserMacroNames: _TStringList;
    FUserMacroValues: _TStringList;
    FParserClass: TSQLParserClass;

    function LeftQuote: _char; virtual;
    function RightQuote: _char; virtual;
    function IsDefinedMacro(const MacroName: _string): boolean;
    function Parse(Parser: TSQLParser; const EndChar: _string = ''): _string;
    function ProcessFunction(const Body: _string): _string;
    function GetFunction(const FunctionName: _string; const Params: _TStringArray): _string; virtual;
    function ProcessDate(const Body: _string): _string;
    function ProcessTime(const Body: _string): _string;
    function ProcessTimestamp(const Body: _string): _string;
    function ProcessMacro(const MacroName, Body: _string): _string;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetUserMacros(Names, Values: _TStringList);
    function CheckIfCondition(const Body: _string): boolean;
    procedure Expand(var SQL: _string);
    procedure SetParserClass(Value: TSQLParserClass);
  end;

  procedure FillOptionsList(const OptionPrefix: _string; OptionsList: TOptionsList; List: _TStrings);
  procedure GetOptionValuesList(const OptionName: _string; OptionsList: TOptionsList; List: _TStrings);
  procedure ExtractOption(const Str: _string; var OptionPrefix, OptionName, OptionValue: _string);
  procedure WriteOptions(OptionsList: TOptionsList; List: _TStrings; DestClass: TClass; SetPropFunc: TSetPropFunc);

var
  UniProviders: TUniProviders;
  
implementation

uses
  DAConsts, UniConsts;

{ TUniProvider }

destructor TUniProvider.Destroy;
begin
  FConnectionOptions.Free;
  FSQLOptions.Free;
  FDataSetOptions.Free;
  FScriptOptions.Free;
  FLoaderOptions.Free;
  FDumpOptions.Free;

  inherited;
end;

class function TUniProvider.GetProviderName: string;
begin
  Assert(False);
  Result := '';
end;

function TUniProvider.IsDatabaseSupported: boolean;
begin
  Result := False;
end;

function TUniProvider.IsPortSupported: boolean;
begin
  Result := False;
end;

function TUniProvider.IsDataSetNeedTransaction: boolean;
begin
  Result := False;
end;

function TUniProvider.IsInOutParamSupported: boolean;
begin
  Result := True;
end;

function TUniProvider.IsPoolingSupported: boolean;
begin
  Result := True;
end;

function TUniProvider.NeedRecreateProcCall: boolean;
begin
  Result := False;
end;

function TUniProvider.GetAlerterClass: TCRAlerterClass;
begin
  Result := TCRDummyAlerter;
end;

function TUniProvider.GetParamObjectClass(Param: TDAParam): TClass;
begin
  case Param.DataType of
    ftBlob:
      Result := TCompressedBlob;
    ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}:
      Result := TBlob;
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

function TUniProvider.CreateParamObject(Param: TDAParam): TSharedObject;
begin
  case Param.DataType of
    ftBlob:
      Result := TCompressedBlob.Create;
    ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}: begin
      Result := TBlob.Create;
    {$IFDEF VER10P}
      if Param.DataType = ftWideMemo then
        TBlob(Result).IsUnicode := True;
    {$ENDIF}
    end;
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

procedure TUniProvider.SetObjectProps(Obj: TObject; Options: _TStrings; SetAllProps: boolean);
begin
  if Obj is TCRConnection then
    GetConnectionOptions.ImportOptions(Options, Obj, TCRConnection(Obj).SetProp, SetAllProps)
  else
  if Obj is TCRConnectionParameters then
    GetConnectionOptions.ImportOptions(Options, Obj, TCRConnectionParameters(Obj).SetProp, SetAllProps)
  else
  if Obj is TCRServerEnumerator then
    GetConnectionOptions.ImportOptions(Options, Obj, TCRServerEnumerator(Obj).SetProp, SetAllProps)
  else
  if Obj is TConnectDialogService then
    GetConnectionOptions.ImportOptions(Options, Obj, TConnectDialogService(Obj).SetProp, SetAllProps)
  else
  if Obj is TCRCommand then
    GetSQLOptions.ImportOptions(Options, Obj, TCRCommand(Obj).SetProp, SetAllProps)
  else
  if Obj is TCRRecordSet then begin
    GetDataSetOptions.ImportOptions(Options, Obj, TCRRecordSet(Obj).SetProp, SetAllProps);
    GetDataSetOptions.ImportOptions(Options, TCRRecordSet(Obj).GetCommand, TCRRecordSet(Obj).GetCommand.SetProp, SetAllProps);
  end
  else
  if Obj is TDADataSetService then
    GetDataSetOptions.ImportOptions(Options, Obj, TDADataSetService(Obj).SetProp, SetAllProps)
  else
  if Obj is TDAScriptProcessor then
    GetScriptOptions.ImportOptions(Options, Obj, TDAScriptProcessor(Obj).SetProp, SetAllProps)
  else
  if Obj is TCRLoader then
    GetLoaderOptions.ImportOptions(Options, Obj, TCRLoader(Obj).SetProp, SetAllProps)
  else
  if Obj is TDADumpProcessor then
    GetDumpOptions.ImportOptions(Options, Obj, TDADumpProcessor(Obj).SetProp, SetAllProps)
  else
  if Obj is TCRAlerter then
    GetAlerterOptions.ImportOptions(Options, Obj, TCRAlerter(Obj).SetProp, SetAllProps);
end;

function TUniProvider.GetScriptOptions: TOptionsList;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
  end;
  Result := FScriptOptions;
end;

function TUniProvider.GetLoaderOptions: TOptionsList;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
  end;
  Result := FLoaderOptions;
end;

function TUniProvider.GetDumpOptions: TOptionsList;
begin
  if FDumpOptions = nil then begin
    FDumpOptions := TOptionsList.Create(GetProviderName);
  end;
  Result := FDumpOptions;
end;

function TUniProvider.GetAlerterOptions: TOptionsList;
begin
  if FAlerterOptions = nil then begin
    FAlerterOptions := TOptionsList.Create(GetProviderName);
  end;
  Result := FAlerterOptions;
end;

function TUniProvider.DefaultTableSchema: _string;
begin
  Result := '';
end;

{ TUniProviderDesc }

function TUniProviderDesc.GetUnitName: string;
begin
  Result := FProviderShortName + 'UniProvider';
end;

function TUniProviderDesc.GetUnitNameCLR: string;
begin
  Result := 'Devart.UniDac.Oracle.' + GetUnitName;
end;

function TUniProviderDesc.GetProviderComponentName: string;
begin
  Result := 'T' + GetUnitName;
end;

{ TUniProviders }

function TUniProviders.FindProviderDesc(ProviderName: string): TUniProviderDesc;
var
  List: TList;
  i: integer;
begin
  List := LockList;
  try
    Result := nil;
    for i := 0 to List.Count - 1 do
      if CompareText(TUniProviderDesc(List[i]).ProviderName, ProviderName) = 0 then begin
        Result := TUniProviderDesc(List[i]);
        Break;
      end;  
  finally
    UnlockList;
  end;
end;

procedure TUniProviders.RegisterProviderDesc(ProviderName, ProviderShortName,
  PackageName, AssemblyName, SiblingProduct: string);
var
  UniProviderDesc: TUniProviderDesc;
begin
  UniProviderDesc := FindProviderDesc(ProviderName);
  if UniProviderDesc = nil then begin
    UniProviderDesc := TUniProviderDesc.Create;
    UniProviderDesc.FProviderName := ProviderName;
    UniProviderDesc.FProviderShortName := ProviderShortName;
    UniProviderDesc.FPackageName := PackageName;
    UniProviderDesc.FAssemblyName := AssemblyName;
    UniProviderDesc.FSiblingProduct := SiblingProduct;

    Add(UniProviderDesc);
  end;
end;

destructor TUniProviders.Destroy;
var
  List: TList;
  i: integer;
begin
  List := LockList;
  try
    for i := 0 to List.Count - 1 do
      TUniProviderDesc(List[i]).Free;
  finally
    UnlockList;
  end;

  inherited;
end;

procedure TUniProviders.RegisterProvider(UniProviderClass: TUniProviderClass);
var
  UniProviderDesc: TUniProviderDesc;
begin
  LockList;
  try
    UniProviderDesc := GetProviderDesc(UniProviderClass.GetProviderName);
    UniProviderDesc.FProvider := UniProviderClass.Create(nil);
  finally
    UnLockList;
  end;  
end;

procedure TUniProviders.UnRegisterProvider(UniProviderClass: TUniProviderClass);
var
  UniProviderDesc: TUniProviderDesc;
begin
  LockList;
  try
    UniProviderDesc := GetProviderDesc(UniProviderClass.GetProviderName);
    UniProviderDesc.FProvider.Free;
    UniProviderDesc.FProvider := nil;
  finally
    UnLockList;
  end;  
end;

function TUniProviders.GetProviderDesc(ProviderName: string): TUniProviderDesc;
begin
  Result := FindProviderDesc(ProviderName);
  if Result = nil then
    raise Exception.Create(Format(SInvalidProviderName, [ProviderName]));
end;

function TUniProviders.GetProvider(ProviderName: string): TUniProvider;
var
  UniProviderDesc: TUniProviderDesc;
begin
  UniProviderDesc := FindProviderDesc(ProviderName);

  if UniProviderDesc <> nil then
    Result := UniProviderDesc.Provider
  else
    Result := nil;
end;

procedure TUniProviders.GetProviderNames(Names: TStrings);
var
  List: TList;
  i: integer;
begin
  List := LockList;
  try
    Names.Clear;
    for i := 0 to List.Count - 1 do
      with TUniProviderDesc(List[i]) do
        if Provider <> nil then
          Names.Add(ProviderName);
  finally
    UnlockList;
  end;
end;

{ TConnectDialogService }

constructor TConnectDialogService.Create;
begin
  inherited;
end;

function TConnectDialogService.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TConnectDialogService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TConnectDialogService.GetConnectMode: integer;
begin
  Result := 1;
end;

function TConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := False;
end;

function TConnectDialogService.GetDefaultDatabase: _string;
begin
  Result := '';
end;

function TConnectDialogService.UsernameEnabled: boolean;
begin
  Result := True;
end;

function TConnectDialogService.PasswordEnabled: boolean;
begin
  Result := True;
end;

function TConnectDialogService.ServerEnabled: boolean;
begin
  Result := True;
end;

function TConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := True;
end;

function TConnectDialogService.PortEnabled: boolean;
begin
  Result := True;
end;

{ TCRDummyAlerter }

procedure TCRDummyAlerter.SendEvent(const EventName, Message: _string);
begin
  raise Exception.Create(SAlertsNotSupported);
end;

procedure TCRDummyAlerter.Start;
begin
  raise Exception.Create(SAlertsNotSupported);
end;

procedure TCRDummyAlerter.Stop;
begin
  raise Exception.Create(SAlertsNotSupported);
end;

{ TOption }

constructor TOption.Create(const OptionName: _string; InternalIndex: integer; InternalClasses: array of TClass; DefaultValue: variant);
var
  i: integer;
begin
  inherited Create;

  FOptionName := OptionName;
  FInternalIndex := InternalIndex;
  FDefaultValue := DefaultValue;

  SetLength(FInternalClasses, Length(InternalClasses));
  for i := 0 to High(InternalClasses) do
    FInternalClasses[i] := InternalClasses[i];
end;

function TOption.GetAsNative(const Value: _string): variant;
begin
  Result := Value;
end;

function TOption.GetAsString(const Value: variant): _string;
begin
  Result := Value;
end;

function TOption.GetDefaultValue: variant;
begin
  Result := FDefaultValue;
end;

procedure TOption.InternalGetValuesList(List: _TStrings);
begin

end;

procedure TOption.GetValuesList(List: _TStrings);
begin
  if Assigned(FOnGetValuesList) then
    FOnGetValuesList(List)
  else
    InternalGetValuesList(List);  
end;

procedure TOption.ValidationError(const Value: _string);
begin
  raise Exception.Create('Invalid value: ' +  Value + ' for option ' + OptionName);
end;

function TOption.CheckValue(const Value: _string): boolean;
begin
  Result := True;
end;

procedure TOption.Validate(const Value: _string);
var
  i: integer;
  List: _TStringList;
begin
  if Assigned(FOnGetValuesList) then begin
    List := _TStringList.Create;
    try
      for i := 0 to List.Count - 1 do
        if _SameText(List[i], Value) then
          Exit; 
      ValidationError(Value);
    finally
      List.Free;
    end;
  end
  else
    if not CheckValue(Value) then
      ValidationError(Value);
end;

{ TIntegerOption }

function TIntegerOption.GetAsString(const Value: variant): _string;
begin
  Result := IntToStr(Value);
end;

function TIntegerOption.GetAsNative(const Value: _string): variant;
begin
  Result := StrToInt(Trim(Value));
end;

function TIntegerOption.CheckValue(const Value: _string): boolean;
begin
  Result := StrToIntDef(Trim(Value), -MaxInt) <> -MaxInt;
end;

{ TBooleanOption }

function TBooleanOption.GetAsString(const Value: variant): _string;
begin
  if Boolean(Value) then
    Result := 'True'
  else
    Result := 'False';
end;

function TBooleanOption.GetAsNative(const Value: _string): variant;
begin
  if _SameText(Trim(Value), 'True') then
    Result := True
  else
    Result := False;
end;

function TBooleanOption.CheckValue(const Value: _string): boolean;
begin
  Result := _SameText(Trim(Value), 'True') or _SameText(Trim(Value), 'False');
end;

procedure TBooleanOption.InternalGetValuesList(List: _TStrings);
begin
  List.Add('False');
  List.Add('True');
end;

{ TEnumeratorOption }

constructor TEnumeratorOption.Create(const OptionName: _string; InternalIndex: integer;
  InternalClasses: array of TClass; DefaultValue: variant; TypeInfo: PTypeInfo);
var
  TypeData: PTypeData;
begin
  inherited Create(OptionName, InternalIndex, InternalClasses, DefaultValue);

  FTypeInfo := TypeInfo;
  TypeData := GetTypeData(FTypeInfo);
  if TypeData <> nil then begin
    FMinValue := TypeData{$IFNDEF CLR}^{$ENDIF}.MinValue;
    FMaxValue := TypeData{$IFNDEF CLR}^{$ENDIF}.MaxValue;
  end;

  FInternalType := (FTypeInfo.Name <> '') and (FTypeInfo.Name[1] = '_');
end;

function TEnumeratorOption.GetAsString(const Value: variant): _string;
begin
  if FInternalType then
    Result := Copy(GetEnumName(FTypeInfo, Integer(Value)), 2, MaxInt)
  else
    Result := GetEnumName(FTypeInfo, Integer(Value));
end;

function TEnumeratorOption.GetAsNative(const Value: _string): variant;
var
  RealValue: _string;
begin
  Assert(FMaxValue <= High(Byte));
  RealValue := Trim(Value);
  if FInternalType then
    RealValue := '_' + RealValue;
  VarCast(Result, GetEnumValue(FTypeInfo, RealValue), varByte);
end;

function TEnumeratorOption.CheckValue(const Value: _string): boolean;
var
  RealValue: _string;
begin
  RealValue := Trim(Value);
  if FInternalType then
    RealValue := '_' + RealValue;
  Result := GetEnumValue(FTypeInfo, RealValue) >= 0;
end;

procedure TEnumeratorOption.InternalGetValuesList(List: _TStrings);
var
  i: integer;
begin
  for i := FMinValue to FMaxValue do
    if FInternalType then
      List.Add(Copy(GetEnumName(FTypeInfo, i), 2, MaxInt))
    else
      List.Add(GetEnumName(FTypeInfo, i));
end;

{ TOptionsList }

constructor TOptionsList.Create(const Prefix: _string);
begin
  inherited Create;

  Sorted := True;
  FPrefix := Prefix;
end;

destructor TOptionsList.Destroy;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Objects[i].Free;

  inherited;
end;

procedure TOptionsList.Add(Value: TOption);
begin
  AddObject(Value.OptionName, Value);
end;

function TOptionsList.OptionByName(const Name: _string): TOption;
var
  i: integer;
begin
  i := IndexOf(Name);
  if i <> -1 then
    Result := TOption(Objects[i])
  else
    Result := nil;
end;

procedure TOptionsList.ImportOptions(Source: _TStrings; DestObject: TObject; SetPropFunc: TSetPropFunc; SetAll: boolean);
var
  i, k, j: integer;
  Option: TOption;
  UseDefault: boolean;
  Value: Variant;
  OptionPrefix, OptionName, OptionValue: _string;
begin
  for i := 0 to Count - 1 do
    for k := 0 to High(Items[i].InternalClasses) do begin
      if DestObject.ClassType = Items[i].InternalClasses[k] then begin
        Option := Items[i];
        UseDefault := True;

        for j := 0 to Source.Count - 1 do begin
          ExtractOption(Source[j], OptionPrefix, OptionName, OptionValue);
          if ((OptionPrefix = '') or _SameText(Prefix, OptionPrefix)) and
            _SameText(Option.OptionName, OptionName)
          then begin
            UseDefault := False;
            Break;
          end;
        end;

        if not UseDefault then begin
          Option.Validate(OptionValue);
          Value := Option.GetAsNative(OptionValue);
        end
        else
          Value := Option.GetDefaultValue;

        if Assigned(Option.OnAssignValue) then
          Option.OnAssignValue(DestObject, Value)
        else
          SetPropFunc(Option.InternalIndex, Value);

        break;
      end;
    end;
end;

procedure TOptionsList.ExportDefOptions(Dest: _TStrings);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Dest.Add(FPrefix + '.' + Items[i].OptionName + '=' + Items[i].GetAsString(Items[i].GetDefaultValue));
end;

function TOptionsList.GetValueByName(Source: _TStrings; const Name: _string): variant;
var
  i: integer;
  Option: TOption;
  OptionPrefix, OptionName, OptionValue: _string;
begin
  Result := Unassigned;
  Option := OptionByName(Name);
  if Option <> nil then begin
    for i := 0 to Source.Count - 1 do begin
      ExtractOption(Source[i], OptionPrefix, OptionName, OptionValue);
      if ((OptionPrefix = '') or _SameText(Prefix, OptionPrefix)) and
        _SameText(OptionName, Name)
      then begin
        Option.Validate(OptionValue);
        Result := Option.GetAsNative(OptionValue);
        exit;
      end;
    end;
    Result := Option.GetDefaultValue;
  end;
end;

function TOptionsList.GetOption(Index: integer): TOption;
begin
  Result := TOption(inherited Objects[Index]);
end;

procedure TOptionsList.SetOption(Index: integer; const Value: TOption);
begin
  inherited Objects[Index] := Value;
end;

{ TUniSqlFormatter }

const
  BeginMacroSt = '{';
  EndMacroSt = '}';
  FunctionPrefix = 'fn';
  DatePrefix = 'date';
  TimePrefix = 'time';
  TimestampPrefix = 'timestamp';
  IfPrefix = 'if';
  ElsePrefix = 'else';
  EndIfPrefix = 'endif';
  DateTimeLiteralFunctionName = '__DATE_TIME_LITERAL';
  DateLiteralFunctionName = '__DATE_LITERAL';
  TimeLiteralFunctionName = '__TIME_LITERAL';

constructor TUniSqlFormatter.Create;
begin
  inherited;
  FParserClass := TSQLParser;
end;

destructor TUniSqlFormatter.Destroy;
begin
  inherited;
end;

procedure TUniSqlFormatter.SetUserMacros(Names, Values: _TStringList);
begin
  FUserMacroNames := Names;
  FUserMacroValues := Values;
end;

procedure TUniSqlFormatter.Expand(var SQL: _string);
var
  Parser: TSQLParser;
begin
  Parser := FParserClass.Create(SQL);
  try
    Parser.OmitBlank := False;
    Parser.Uppered := False;
    Parser.QuotedString := True;
    SQL := Parse(Parser);
  finally
    Parser.Free;
  end;
end;

function TUniSqlFormatter.LeftQuote: _char;
begin
  Result := '"';
end;

function TUniSqlFormatter.RightQuote: _char;
begin
  Result := '"';
end;

function TUniSqlFormatter.IsDefinedMacro(const MacroName: _string): boolean;
begin
  Result := (FUserMacroNames.IndexOf(MacroName) >= 0) or
    (FPredefinedMacros.IndexOf(MacroName) >= 0);
end;

function TUniSqlFormatter.GetFunction(const FunctionName: _string; const Params: _TStringArray): _string;
begin
  raise Exception.CreateFmt(SUnknownFunction, [FunctionName]);
end;

function TUniSqlFormatter.ProcessFunction(const Body: _string): _string;
var
  Parser: TSQLParser;
  CodeLexem: integer;
  St: _string;
  FuncName: _string;
  Params: _TStringArray;
  ParamCount: Integer;
  CurrPos: integer;
  BracketCount: Integer;
  FuncInd: integer;
begin
  Parser := FParserClass.Create(Body);
  Parser.OmitBlank := True;
  Parser.Uppered := False;
  Parser.QuotedString := True;
  try
    Parser.ToBegin;
    CodeLexem := Parser.GetNext(FuncName);
    if (CodeLexem <> lcIdent) and (CodeLexem <= Parser.SymbolLexems.Count) then
      raise Exception.Create(SEmptyFunctionName);

    CodeLexem := Parser.GetNext(St);
    ParamCount := 0;
    SetLength(Params, 0);
    if St = '(' then begin
      // Parsing params
      CurrPos := Parser.CurrPos;
      BracketCount := 0;
      repeat
        CodeLexem := Parser.GetNext(St);
        if (BracketCount = 0) and ((St = ',') or (St = ')')) then begin
          SetLength(Params, ParamCount + 1);
          Params[ParamCount] := Copy(Body, CurrPos + 1, Parser.CurrPos - CurrPos - 1);
          Inc(ParamCount);
          CurrPos := Parser.CurrPos;
        end
        else
          if St = '(' then
            Inc(BracketCount)
          else
          if St = ')' then
            Dec(BracketCount);
      until CodeLexem = lcEnd;

      if (BracketCount > 0) or (Body[Parser.PrevPos] <> ')') then
        raise Exception.Create(SInvalidBracketCount);
    end
    else
      if CodeLexem <> lcEnd then
        raise Exception.Create(SUnexpectedChar);

    FuncInd := FFunctions.IndexOf(FuncName);
    if FuncInd <> -1 then begin
      Result := PChar(FFunctions.Objects[FuncInd]);
      case ParamCount of
        0: Result := Result;
        1: Result := _Format(Result, [Params[0]]);
        2: Result := _Format(Result, [Params[0], Params[1]]);
        3: Result := _Format(Result, [Params[0], Params[1], Params[2]]);
      else
        raise Exception.CreateFmt(SWrongArgCnt, [FuncName]);
      end;
    end
    else
      Result := GetFunction(FuncName, Params);
  finally
    Parser.Free;
  end;
end;

function TUniSqlFormatter.ProcessDate(const Body: _string): _string;
var
  Date: _string;
  FuncInd: integer;
begin
  Date := Trim(Body);
  if (Length(Date) < 2) or (Date[1] <> '''') or (Date[Length(Date)] <> '''') then
    raise Exception.Create(SInvalidDate);

  FuncInd := FFunctions.IndexOf(DateLiteralFunctionName);
  if FuncInd = -1 then
    Assert(False);
    //raise Exception.Create('');

  Result := Format(PChar(FFunctions.Objects[FuncInd]), [Date]);
end;

function TUniSqlFormatter.ProcessTime(const Body: _string): _string;
var
  Time: _string;
  FuncInd: integer;
begin
  Time := Trim(Body);
  if (Length(Time) < 2) or (Time[1] <> '''') or (Time[Length(Time)] <> '''') then
    raise Exception.Create(SInvalidTime);

  FuncInd := FFunctions.IndexOf(TimeLiteralFunctionName);
  if FuncInd = -1 then
    Assert(False);
    //raise Exception.Create('');

  Result := Format(PChar(FFunctions.Objects[FuncInd]), [Time]);
end;

function TUniSqlFormatter.ProcessTimestamp(const Body: _string): _string;
var
  Timestamp: _string;
  FuncInd: integer;
begin
  Timestamp := Trim(Body);
  if (Length(Timestamp) < 2) or (Timestamp[1] <> '''') or (Timestamp[Length(Timestamp)] <> '''') then
    raise Exception.Create(SInvalidTimestamp);

  FuncInd := FFunctions.IndexOf(DateTimeLiteralFunctionName);
  if FuncInd = -1 then
    Assert(False);
    //raise Exception.Create('');

  Result := Format(PChar(FFunctions.Objects[FuncInd]), [Timestamp]);
end;

function TUniSqlFormatter.ProcessMacro(const MacroName, Body: _string): _string;
var
  Name, NewBody: _string;
  i: integer;
  Parser: TSQLParser;
begin
  Name := Trim(_UpperCase(MacroName));
  NewBody := Body;
  if NewBody <> '' then
    NewBody := ' ' + NewBody;
  i := FUserMacroNames.IndexOf(Name);
  if i >= 0 then
    Result := FUserMacroValues[i] + NewBody
  else begin
    i := FPredefinedMacros.IndexOf(Name);
    if i >= 0 then
      Result := PChar(FPredefinedMacros.Objects[i]) + NewBody
    else
      Result := '';
  end;

  if Result <> '' then begin
    Parser := FParserClass.Create(Result);
    try
      Parser.OmitBlank := False;
      Parser.Uppered := False;
      Parser.QuotedString := True;
      Result := Parse(Parser);
    finally
      Parser.Free;
    end;
  end;
end;

function TUniSqlFormatter.CheckIfCondition(const Body: _string): Boolean;
var
  Name: _string;
begin
  Name := Trim(_UpperCase(Body));
  if Name <> '' then
    Result := IsDefinedMacro(Name)
  else
    // empty condition
    Result := True;
end;

function TUniSqlFormatter.Parse(Parser: TSQLParser; const EndChar: _string = ''): _string;
var
  CodeLexem: integer;
  St: _string;
  MacroName, _MacroName, Body: _string;
  IfCount: integer;
  IfResult: boolean;
  IfResults: array of boolean;
  ElseClauses: array of boolean;

  function GetIfResult: Boolean;
  var
    i: integer;
  begin
    Result := True;
    for i := 0 to IfCount - 1 do
      Result := Result and IfResults[i];
  end;

begin
  Result := '';
  St := '';
  IfCount := 0;
  IfResult := True;
  SetLength(IfResults, 0);
  SetLength(ElseClauses, 0);

  while True do begin
    CodeLexem := Parser.GetNext(St);

    if (CodeLexem = lcEnd) or (St = EndChar) then
      Break;

    if St = BeginMacroSt then begin
      CodeLexem := Parser.GetNext(MacroName);
      if (CodeLexem <> lcIdent) and (CodeLexem <= Parser.SymbolLexems.Count) then
        raise Exception.Create(SEmptyMacroOrFunction);

      CodeLexem := Parser.GetNext(St);
      Body := '';
      if CodeLexem = lcBlank then begin
        Body := Parse(Parser, '}');
      end
      else
        if (CodeLexem <> lcSymbol) or (St <> '}') then
          raise Exception.Create(SUnexpectedChar);

      _MacroName := _LowerCase(MacroName);
      if _MacroName = IfPrefix then begin
        Inc(IfCount);
        SetLength(IfResults, IfCount);
        IfResults[IfCount - 1] := CheckIfCondition(Body);
        IfResult := GetIfResult;
        SetLength(ElseClauses, IfCount);
        ElseClauses[IfCount - 1] := False;
      end
      else
      if _MacroName = ElsePrefix then begin
        if IfCount = 0 then
          raise Exception.Create(SUnexpectedElse);

        if ElseClauses[IfCount - 1] then
          raise Exception.Create(SUnexpectedElse)
        else
          ElseClauses[IfCount - 1] := True;

        IfResults[IfCount - 1] := not IfResults[IfCount - 1];
        IfResult := GetIfResult;
      end
      else
      if _MacroName = EndIfPrefix then begin
        if IfCount = 0 then
          raise Exception.Create(SUnexpectedEndif);

        Dec(IfCount);
        SetLength(IfResults, IfCount);
        IfResult := GetIfResult;
        SetLength(ElseClauses, IfCount);
      end
      else
      if IfResult then begin
        if _MacroName = FunctionPrefix then begin
          Result := Result + ProcessFunction(Body);
        end
        else
        if _MacroName = DatePrefix then begin
          Result := Result + ProcessDate(Body);
        end
        else
        if _MacroName = TimePrefix then begin
          Result := Result + ProcessTime(Body);
        end
        else
        if _MacroName = TimestampPrefix then begin
          Result := Result + ProcessTimestamp(Body);
        end
        else
          Result := Result + ProcessMacro(MacroName, Body);
      end;
    end
    else
      if IfResult then begin
        if (CodeLexem = lcIdent) and
          (Length(St) > 1) and (St[1] = '"') and (St[Length(St)] = '"')
        then begin
          St[1] := LeftQuote;
          St[Length(St)] := RightQuote;
        end
        else
        if (CodeLexem = lcComment) and
          (Length(St) > 1) and (St[1] = '-') and (St[2] = '-')
          and (Pos('*/', St) = 0)
        then
          St := '/*' + Copy(St, 3, MaxInt) + '*/' ;
        Result := Result + St;
      end;
  end;

  if IfCount <> 0 then
    raise Exception.Create(SNotCompletedIF);
end;

procedure TUniSqlFormatter.SetParserClass(Value: TSQLParserClass);
begin
  FParserClass := Value;
end;

function GetUniProviders: TUniProviders;
begin
  Result := UniProviders;
end;

procedure FillOptionsList(const OptionPrefix: _string; OptionsList: TOptionsList; List: _TStrings);
var
  i: integer;
begin
  for i := 0 to OptionsList.Count - 1 do
    List.Add(OptionPrefix + '.' + OptionsList[i].OptionName + '=' + OptionsList[i].GetAsString(OptionsList[i].GetDefaultValue));
end;

procedure GetOptionValuesList(const OptionName: _string; OptionsList: TOptionsList; List: _TStrings);
var
  Option: TOption;
begin
  Option := OptionsList.OptionByName(OptionName);
  if Option <> nil then
    Option.GetValuesList(List);
end;

procedure ExtractOption(const Str: _string; var OptionPrefix, OptionName, OptionValue: _string);
var
  DotPos, EqualPos: integer;
begin
  DotPos := Pos('.', Str);
  EqualPos := Pos('=', Str);
  if DotPos > EqualPos then
    DotPos := 0;

  if (DotPos > 0) and (EqualPos > 0) then
    OptionPrefix := Copy(Str, 1, DotPos - 1);

  if EqualPos > 0 then
    OptionName := Copy(Str, DotPos + 1, EqualPos - DotPos - 1);

  if EqualPos > 0 then
    OptionValue := Copy(Str, EqualPos + 1, Length(Str));
end;

procedure WriteOptions(OptionsList: TOptionsList; List: _TStrings; DestClass: TClass; SetPropFunc: TSetPropFunc);
begin

end;

initialization
  UniProviders := TUniProviders.Create;
  UniProviders.RegisterProviderDesc('Access', 'Access', 'accessprovider', 'Devart.UniDac.Access', '');
  UniProviders.RegisterProviderDesc('Advantage', 'Advantage', 'adsprovider', 'Devart.UniDac.Advantage', '');
  UniProviders.RegisterProviderDesc('ASE', 'ASE', 'aseprovider', 'Devart.UniDac.ASE', '');
  UniProviders.RegisterProviderDesc('DB2', 'DB2', 'db2provider', 'Devart.UniDac.DB2', '');
  UniProviders.RegisterProviderDesc('DBF', 'DBF', 'dbfprovider', 'Devart.UniDac.DBF', '');
  UniProviders.RegisterProviderDesc('InterBase', 'InterBase', 'ibprovider', 'Devart.UniDac.InterBase', 'IBDAC');
  UniProviders.RegisterProviderDesc('MySQL', 'MySQL', 'myprovider', 'Devart.UniDac.MySQL', 'MyDAC');
  UniProviders.RegisterProviderDesc('NexusDB', 'NexusDB', 'nexusprovider', 'Devart.UniDac.NexusDB', '');
  UniProviders.RegisterProviderDesc('ODBC', 'ODBC', 'odbcprovider', 'Devart.UniDac.ODBC', '');
  UniProviders.RegisterProviderDesc('Oracle', 'Oracle', 'oraprovider', 'Devart.UniDac.Oracle', 'ODAC');
  UniProviders.RegisterProviderDesc('PostgreSQL', 'PostgreSQL', 'pgprovider', 'Devart.UniDac.PostgreSQL', 'PgDAC');
  UniProviders.RegisterProviderDesc('SQL Server', 'SQLServer', 'msprovider', 'Devart.UniDac.SQLServer', 'SDAC');
  UniProviders.RegisterProviderDesc('SQLite', 'SQLite', 'liteprovider', 'Devart.UniDac.SQLite', '');

finalization
  UniProviders.Free;

end.
