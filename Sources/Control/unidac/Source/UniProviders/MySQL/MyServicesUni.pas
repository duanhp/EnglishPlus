{$IFNDEF CLR}
{$I MyDac.inc}
unit MyServicesUni;
{$ENDIF}

interface

uses
{$IFDEF VER6P}
  Variants, DateUtils,
{$ENDIF}
{$IFNDEF CLR}
  CLRClasses, CRXml,
{$ELSE}
  System.Text, System.XML, System.Runtime.InteropServices,
{$ENDIF}
  SysUtils, Classes, DB,
  MemUtils, MemData, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF}, CRAccess,
  DBAccess, DADump,
{$IFNDEF UNIDACPRO}
  MyCall, MyClasses;
{$ELSE}
  MyCallUni, MyClassesUni;
{$ENDIF}

const
  prCheckRowVersion = 101;
  prLockType        = 102;
  prAutoIncrementReadOnly = 103;
  prBackupTables    = 104;
  prBackupViews     = 105;
  prBackupData      = 106;
  prAddLock         = 108;
  prDisableKeys     = 109;
  prHexBlob         = 110;
  prUseExtSyntax    = 111;
  prUseDelayedIns   = 112;
  prCommitBatchSize = 113;
  prInsertType      = 114;

type
  TCustomMyDataSetService = class;

  TLockRecordTypeI = (ilrImmediately, ilrDelayed);
  TLockTypeI = (iltRead, iltReadLocal, iltWrite, iltWriteLowPriority);
  _TMyInsertType = (_itInsert, _itInsertIgnore, _itReplaceInto);

  TCustomMySQLGenerator = class(TDASQLGenerator)
  protected
    FDataSetService: TCustomMyDataSetService;

    FLimit: integer;
    FOffset: integer;
    FUseHandler: boolean;
    FHandlerIndex: _string;

    function GetParamInfoClass: TDAParamInfoClass; override;
    procedure AddFieldToCondition(SB: _StringBuilder;
      FieldDesc: TCRFieldDesc;
      const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
    procedure AddParam(SB: _StringBuilder; FieldDesc: TFieldDesc;
      const StatementType: TStatementType;
      const ParamType: TParamType;
      Index: integer = -1;
      Old: boolean = False); override;

    procedure GenerateConditions(SB: _StringBuilder;
      const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean;
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;

    procedure GenerateLockSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;

    //function GetActualFieldName(FldDesc: TCRFieldDesc; IsRefresh: boolean): _string; override;

  public
    constructor Create(AOwner: TDADataSetService); override;
    function GetParamOffset(const Index: integer): integer;

    function GenerateTableSQL(const TableName, OrderFields: _string): _string; override;

    property Limit: integer read FLimit write FLimit;
    property Offset: integer read FOffset write FOffset;
    property UseHandler: boolean read FUseHandler write FUseHandler;
    property HandlerIndex: _string read FHandlerIndex write FHandlerIndex;
  end;

  TCustomMyDataSetUpdater = class(TDADataSetUpdater)
  protected
    FDataSetService: TCustomMyDataSetService;

    function RetunParamsAsFields: boolean; override;
    function GetIdentityFieldValue(var Value: variant): boolean; override;

    procedure CheckUpdateQuery(const StatementType: TStatementType); override;
    procedure SetUpdateQueryOptions(const StatementType: TStatementType); override;
    procedure CheckUpdateSQL(const SQL: _string; const StatementTypes: TStatementTypes;
      out ParamsInfo: TDAParamsInfo; UseGenerator: boolean = True); override;

    function IsRefreshQuickField(FieldDesc: TFieldDesc): boolean; override;
    procedure SaveMaxRefreshQuickValue(FieldDesc: TFieldDesc; const Value: variant); override;

    procedure PrepareUpdate; override;
    function PerformLock: boolean; override;
    function LockCompare(const Value1, Value2: variant): boolean; override;

  public
    constructor Create(AOwner: TDataSetService); override;
  end;

  TCustomMyDataSetService = class(TDADataSetService)
  protected
    FInGetRecCount: boolean;
    FUpdater: TCustomMyDataSetUpdater;

    FCheckRowVersion: boolean;
    FLockType: TLockRecordTypeI; // for GenerateLockSQL only
    FTimestampField: TMySQLFieldDesc;
    FAutoIncrementReadOnly: boolean;

    procedure CreateDataSetUpdater; override;
    procedure SetDataSetUpdater(Value: TDataSetUpdater); override;
    procedure CreateSQLGenerator; override;

    function GetIConnection: TMySQLConnection;
    function GetIRecordSet: TMySQLRecordSet;
    function IsFullRefresh: boolean;

    function DetectCanModify: boolean; override;
    function DetectIdentityField: TCRFieldDesc; override;
    procedure CloseCursor; override;

    function CanUseAllKeyFields: boolean; override;
    procedure FillFieldDescs(out FieldDescs: TFieldDescArray; FillKeyFieldDescs, ForceUseAllFields: boolean);
    procedure FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean); override;
    procedure FillDataFieldDescs(out DataFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean); override;

    procedure InitCursor; override;
    procedure FillFieldsDefaultValues; override;
    procedure SetFieldOrigin(Field: TField; FieldDesc: TCRFieldDesc); override;

    function GetRecCount: integer; override;
    procedure BreakExec; override;
    function Executing: boolean; override;

    procedure SetNumberRange(FieldDef: TFieldDef); override;

  { XML }
    procedure WriteFieldXMLDataType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: _string; XMLWriter: XMLTextWriter); override;
    procedure WriteFieldXMLAttributeType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: _string;
      XMLWriter: XMLTextWriter); override;

  public
    constructor Create(AOwner: TMemDataSet); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;

    function OpenNext: boolean; override;

    property TimestampField: TMySQLFieldDesc read FTimestampField;
  end;

  TMyServerEnumerator = class (TCRServerEnumerator)
  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure GetServerList(List: _TStrings); override;
  end;

  TCustomMyDataTypesMap = class(TDataTypesMap)
  end;

  TCustomMyDumpProcessor = class(TDADumpProcessor)
  protected
    FBackupTables: boolean;
    FBackupViews: boolean;
    FBackupData: boolean;
    FAddLock: boolean;
    FDisableKeys: boolean;
    FHexBlob: boolean;
    FUseExtSyntax: boolean;
    FUseDelayedIns: boolean;
    FCharset: string;
    FCommitBatchSize: integer;
    FInsertType: _TMyInsertType;

  {$IFDEF VER12P}
    FUseUnicode: boolean;
    procedure Backup(Query: _string); override;
    procedure Add(const Line: _string); override;
  {$ENDIF}
    function CreateQuery: TCustomDADataSet; override;
    procedure AddSettings; override;
    procedure RestoreSettings; override;
    procedure BackupObjects(const Query: _string); override;

  public
    constructor Create(Owner: TDADump); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;
  end;

implementation

uses
  Math, DAConsts, CRParser, DASQLMonitor,
{$IFNDEF UNIDACPRO}
  MyConsts, MyParser;
{$ELSE}
  MyConstsUni, MyParserUni;
{$ENDIF}

{ TCustomMySQLGenerator }

type
  TMyParamInfo = class (TDAParamInfo)
  public
    SB: _StringBuilder;
    Offset: integer;
  end;

constructor TCustomMySQLGenerator.Create(AOwner: TDADataSetService);
begin
  inherited;

  FDataSetService := TCustomMyDataSetService(AOwner);
  FLimit := -1;
end;

function TCustomMySQLGenerator.GetParamInfoClass: TDAParamInfoClass;
begin
  Result := TMyParamInfo;
end;

procedure TCustomMySQLGenerator.AddFieldToCondition(
  SB: _StringBuilder;
  FieldDesc: TCRFieldDesc;
  const StatementType: TStatementType;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);

var
  ActualName: _string;
begin
  Assert(FieldDesc is TMySQLFieldDesc);

  if not Assigned(FieldDesc.TableInfo) then
    Exit;

  if ModifiedFieldsOnly
    and (StatementType = stRefresh)
    and FieldIsNull(FieldDesc, True)
    and (TMySQLFieldDesc(FieldDesc).MySQLType = FIELD_TYPE_TIMESTAMP) then begin // Not 'IS NULL'. Must be compared with '00000000'

    ActualName := GetActualFieldName(FieldDesc, StatementType = stRefresh);
    if SB.Length > 0 then
      SB.Append(' AND ');
    SB.Append(ActualName);
    SB.Append(' = ''00000000''');
    Exit;
  end;

  inherited;
end;

procedure TCustomMySQLGenerator.AddParam(SB: _StringBuilder; FieldDesc: TFieldDesc;
      const StatementType: TStatementType;
      const ParamType: TParamType;
      Index: integer = -1;
      Old: boolean = False);
var
  ParamInfo: TMyParamInfo;
begin
  inherited;

  if FParams <> nil then begin
    ParamInfo := TMyParamInfo(FParamsInfo.Items[FParamsInfo.Count - 1]);
    ParamInfo.Offset := SB.Length - 1;
    ParamInfo.SB := SB;
  end;
end;

procedure TCustomMySQLGenerator.GenerateConditions(SB: _StringBuilder;
      const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean;
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1);
var
  Major: integer;

  function GetTimestampField: TCRFieldDesc;
  var
    i: integer;
  begin
    Result := nil;
    for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
      if TMySQLFieldDesc(KeyAndDataFields.DataFieldDescs[i]).MySQLType = FIELD_TYPE_TIMESTAMP then begin
        if (Result = nil) or TMySQLFieldDesc(KeyAndDataFields.DataFieldDescs[i]).IsCurrentTimestamp then
          Result := KeyAndDataFields.DataFieldDescs[i];
      end;
  end;

  procedure GenerateCondForRQ(TimestampFieldDesc: TFieldDesc);
  var
    TimestampField: TField;
  begin
    if TimestampFieldDesc = nil then
      DatabaseError(STimestampFieldRequired);
    TimestampField := FDataSet.GetField(TimestampFieldDesc);
    if TimestampField = nil then
      DatabaseError(STimestampFieldRequired);

    FCondSB.Append(GetActualFieldName(FDataSet.GetFieldDesc(TimestampField) as TCRFieldDesc, True) + ' >= ' + _string(ValueToSQL(dtDateTime, TMyTableInfo(TMySQLFieldDesc(TimestampFieldDesc).TableInfo).MaxTimestamp, False, False{$IFDEF HAVE_COMPRESS}, False{$ENDIF}, Major)));
  end;

var
  i: integer;
  FldUsed: set of byte;
  TimestampField: TField;
  TimestampFieldDesc: TCRFieldDesc;
begin
  Major := FDataSetService.GetIConnection.ServerPrimaryVer;

  if StatementType = stRefreshQuick then
    GenerateCondForRQ(GetTimestampField)
  else
    if FDataSetService.FCheckRowVersion and ((StatementType = stUpdate) or (StatementType = stDelete)) then begin
      FldUsed := [];
      if Length(KeyAndDataFields.KeyFieldDescs) > 0 then
        for i := 0 to High(KeyAndDataFields.KeyFieldDescs) do begin
          AddFieldToCondition(SB, KeyAndDataFields.KeyFieldDescs[i], StatementType, ModifiedFieldsOnly, Index);
          FldUsed := FldUsed + [KeyAndDataFields.KeyFieldDescs[i].FieldNo];
        end;

      TimestampFieldDesc := GetTimestampField;
      if TimestampFieldDesc = nil then
        TimestampField := nil
      else
        TimestampField := FDataSet.GetField(TimestampFieldDesc);

      // TimestampField may be nil and TimestampField.Value may be unassigned too
      if (TimestampField <> nil) and not TimestampField.IsNull then
        AddFieldToCondition(SB, TimestampFieldDesc, StatementType, ModifiedFieldsOnly, Index)
      else
      begin
        Assert(Length(KeyAndDataFields.DataFieldDescs) > 0);
        for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
          if not IsBlobDataType(KeyAndDataFields.DataFieldDescs[i].DataType) // not "text", or "blob"
            and not (KeyAndDataFields.DataFieldDescs[i].FieldNo in FldUsed) then
            AddFieldToCondition(SB, KeyAndDataFields.DataFieldDescs[i], StatementType, ModifiedFieldsOnly, Index);
      end;
    end
    else
      inherited;
end;

function TCustomMySQLGenerator.GetParamOffset(const Index: integer): integer;
var
  ParamInfo: TMyParamInfo;
begin
  ParamInfo := TMyParamInfo(ParamsInfo.Items[Index]);
  Result := 0;
  if ParamInfo.SB = FFldSB then
    Result := FHeaderSB.Length
  else
  if ParamInfo.SB = FFldParamSB then
    Result := FHeaderSB.Length + FFldSB.Length + FMiddleSB.Length
  else
  if ParamInfo.SB = FCondSB then
    Result := FHeaderSB.Length + FFldSB.Length + FMiddleSB.Length + FFldParamSB.Length
  else
    Assert(False);
  Result := Result + ParamInfo.Offset + 1;
end;

procedure TCustomMySQLGenerator.GenerateLockSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);
begin
  FHeaderSB.Append('SELECT * FROM ');
  FHeaderSB.Append(FTableInfo.TableName);
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('WHERE');
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('  ');
  GenerateConditions(FCondSB, stLock, False, KeyAndDataFields, Index);
  FFooterSB.Append(SLLineSeparator);
  case FDataSetService.FLockType of
    ilrImmediately:
      FFooterSB.Append('FOR UPDATE');
    ilrDelayed:
      FFooterSB.Append('LOCK IN SHARE MODE');
  end;
end;

{function TCustomMySQLGenerator.GetActualFieldName(FldDesc: TCRFieldDesc; IsRefresh: boolean): _string;
var
  SQLObjName: _string;
  SQLObjIdx: integer;
begin
  if not IsRefresh then begin
    Result := FldDesc.Name;
    if Result = '' then
      Result := FldDesc.ActualName;
    Result := QuoteName(Result);
    Exit;
  end;

  SQLObjName := GenerateTableName(FldDesc);

  if SQLObjName <> '' then begin // All
    SQLObjIdx := TMySQLRecordset(FDataSetService.GetIRecordSet).GetSQLObjectIndex(SQLObjName);
    Assert(SQLObjIdx <> - 1);

    if TDBAccessUtils.GetTablesInfo(FDataSet)[SQLObjIdx].TableAlias <> '' then
      Result := TDBAccessUtils.GetTablesInfo(FDataSet)[SQLObjIdx].TableAlias + '.' + QuoteName(FldDesc.ActualName)
    else
      Result := TDBAccessUtils.GetTablesInfo(FDataSet)[SQLObjIdx].TableName + '.' + QuoteName(FldDesc.ActualName);
  end
  else
    Result := QuoteName(FldDesc.ActualName);
end;}

function TCustomMySQLGenerator.GenerateTableSQL(const TableName, OrderFields: _string): _string;
var
  Limit: integer;
  BatchedHandler: boolean;
  Parser: TMyParser;
  Lexem: _string;
  QTableName: _string;
begin
  QTableName := MySQLInfo.NormalizeName(TableName, FDataSet.Options.QuoteNames);

  if not FUseHandler then begin
    Result := 'SELECT * FROM ' + QTableName;

    if OrderFields <> '' then
      Result := Result + ' ORDER BY ' + OrderFields;

    if (FLimit <> -1) or (FOffset <> 0) then
      Result := Result + ' LIMIT ' + IntToStr(FOffset) + ', ' + IntToStr(FLimit);
  end
  else begin
    BatchedHandler := FDataSetService.GetIConnection.IsClient41 and FDataSetService.GetIConnection.IsServer41;
    Result := '';
    if BatchedHandler then
      Result := 'HANDLER ' + QTableName + ' OPEN; ';

    Result := Result + 'HANDLER ' + QTableName + ' READ';

    FHandlerIndex := Trim(FHandlerIndex);
    if FHandlerIndex <> '' then begin
      Result := Result + ' ' + FHandlerIndex;
      Parser := TMyParser.Create(FHandlerIndex);
      try
        Parser.OmitBlank := True;
        Parser.OmitComment := True;
        Parser.GetNextCode;
        Parser.GetNext(Lexem); //+++ char instead of string
        if (Lexem <> '=') and (Lexem <> '<') and (Lexem <> '>') then
          Result := Result + ' FIRST';
      finally
        Parser.Free;
      end;
    end
    else
      Result := Result + ' FIRST';

    if FDataSet.FilterSQL <> '' then
      Result := Result + ' WHERE ' + FDataSet.FilterSQL;

    if FLimit = -1 then
      Limit := MaxInt
    else
      Limit := FLimit;
    Result := Result + ' LIMIT ' + IntToStr(FOffset) + ', ' + IntToStr(Limit);

    if BatchedHandler then begin
      Result := Result + ';';
      Result := Result + ' HANDLER ' + QTableName + ' CLOSE';
    end;
  end;
end;

{ TCustomMyDataSetUpdater }

constructor TCustomMyDataSetUpdater.Create(AOwner: TDataSetService);
begin
  inherited;

  FDataSetService := TCustomMyDataSetService(AOwner);
end;

function TCustomMyDataSetUpdater.RetunParamsAsFields: boolean;
begin
  Result := True;
end;

function TCustomMyDataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
var
  v: variant;
  id: int64;
begin
  Result := False;
  BeginConnection;
  try
    FDataSetService.GetIConnection.GetProp(prLastInsertId, v);
  {$IFDEF VER6P}
    id := v;
  {$ELSE}
    id := PInt64(@TVarData(v).VInteger)^;
  {$ENDIF}
    if id <> 0 then begin
    {$IFDEF VER6P}
      Value := id;
    {$ELSE}
      Value := Integer(id); // Int64 not supported if Delphi5 variants
    {$ENDIF}
      Result := True;
    end;
  finally
    EndConnection;
  end;
end;

procedure TCustomMyDataSetUpdater.CheckUpdateQuery(const StatementType: TStatementType);
begin
  inherited;

  if FUpdateQuery is TCustomDADataSet then begin
    TDBAccessUtils.SetFetchAll(TCustomDADataSet(FUpdateQuery), True);
  end;
end;

procedure TCustomMyDataSetUpdater.SetUpdateQueryOptions(const StatementType: TStatementType);
var
  DestRecordSet, SourceRecordSet: TCRRecordSet;

  procedure CopyPropR(Prop: integer);
  var
    v: variant;
  begin
    SourceRecordSet.GetProp(Prop, v);
    DestRecordSet.SetProp(Prop, v);
  end;

begin
  SourceRecordSet := GetIRecordSet;
  DestRecordSet := TDBAccessUtils.GetIRecordSet(FUpdateQuery as TCustomDADataSet);

  CopyPropR(prEnableBoolean);
end;

procedure TCustomMyDataSetUpdater.CheckUpdateSQL(const SQL: _string;
  const StatementTypes: TStatementTypes; out ParamsInfo: TDAParamsInfo; UseGenerator: boolean = True);
var
  Offset, i: integer;
  ICommand: TMySQLCommand;
begin
  inherited;

  if ParamsInfo = nil then
    Exit;

  if not (((FDataSet.Params.Count > 0) or FDataSetService.IsFullRefresh) and (stRefresh in StatementTypes))
    and not (stRefreshQuick in StatementTypes) and not (stRefreshCheckDeleted in StatementTypes) then begin
    if IsClass(FUpdateQuery, TCustomDADataSet) then
      ICommand := TMySQLCommand(TDBAccessUtils.GetICommand(TCustomDADataSet(FUpdateQuery)))
    else
    if IsClass(FUpdateQuery, TCustomDASQL) then
      ICommand := TMySQLCommand(TDBAccessUtils.GetICommand(TCustomDASQL(FUpdateQuery)))
    else begin
      ICommand := nil;
      Assert(False);
    end;

    for i := 0 to ParamsInfo.Count - 1 do begin
      Offset := TCustomMySQLGenerator(FDataSetService.SQLGenerator).GetParamOffset(i);
      ICommand.SetProp(prParamPosition, Offset);
    end;
  end;
end;

function TCustomMyDataSetUpdater.IsRefreshQuickField(FieldDesc: TFieldDesc): boolean;
begin
  Result := TMySQLFieldDesc(FieldDesc).MySQLType = FIELD_TYPE_TIMESTAMP;
end;

procedure TCustomMyDataSetUpdater.SaveMaxRefreshQuickValue(FieldDesc: TFieldDesc; const Value: variant);
var
  Field: TMySQLFieldDesc;
  CurrValue: TDateTime;
begin
  CurrValue := 0;
  if (not VarIsEmpty(Value)) and (not VarIsNull(Value)) then
    CurrValue := Value;
  Field := TMySQLFieldDesc(GetIRecordSet.FindField(FieldDesc.Name));
  if (Field <> nil) and (Field.TableInfo <> nil) and (Field = TMyTableInfo(Field.TableInfo).TimestampField) then begin
    if TMyTableInfo(Field.TableInfo).MaxTimestamp < CurrValue then
      TMyTableInfo(Field.TableInfo).MaxTimestamp := CurrValue;
  end;
end;

procedure TCustomMyDataSetUpdater.PrepareUpdate;
begin
  if roBeforeEdit in FDataSet.RefreshOptions then
    PerformRefreshRecord;

  inherited;
end;

function TCustomMyDataSetUpdater.PerformLock: boolean;
begin
  if not TDBAccessUtils.GetFetchAll(FDataSet) then
    DatabaseError(SLockVsFetchAll);

  Result := inherited PerformLock;
end;

function TCustomMyDataSetUpdater.LockCompare(const Value1, Value2: variant): boolean;
var
  DateTime1, DateTime2: TDateTime;
  Hour, Min, Sec1, MSec1, Sec2, MSec2: Word;
begin
  Result := inherited LockCompare(Value1, Value2);
  if not Result and (VarType(Value1) = varDate) and (VarType(Value2) = varDate) then begin
    DateTime1 := Value1;
    DecodeTime(DateTime1, Hour, Min, Sec1, MSec1);
    ReplaceTime(DateTime1, EncodeTime(Hour, Min, 0, 0));
    DateTime2 := Value2;
    DecodeTime(DateTime2, Hour, Min, Sec2, MSec2);
    ReplaceTime(DateTime2, EncodeTime(Hour, Min, 0, 0));
    Result := (DateTime1 = DateTime2) and (Abs(Sec1 * 1000 + MSec1 - (Sec2 * 1000 + MSec2)) < 1000);
  end;
end;

{ TCustomMyDataSetService }

constructor TCustomMyDataSetService.Create(AOwner: TMemDataSet);
begin
  inherited;

  FAutoIncrementReadOnly := True;
end;

procedure TCustomMyDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomMyDataSetUpdater.Create(Self));
end;

procedure TCustomMyDataSetService.SetDataSetUpdater(Value: TDataSetUpdater);
begin
  inherited;

  FUpdater := TCustomMyDataSetUpdater(Value);
end;

procedure TCustomMyDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomMySQLGenerator.Create(Self));
end;

function TCustomMyDataSetService.DetectCanModify: boolean;
begin
  Assert(GetIRecordSet <> nil, 'FIRecordSet must be setted to this time');

  Result := inherited DetectCanModify or
    not (FDataSet.ReadOnly or FDataSet.UniDirectional) and
    (FIsAnyFieldCanBeModified or
    (FDataSet.SQLInsert.Count > 0) or
    (FDataSet.SQLUpdate.Count > 0) or
    (FDataSet.SQLDelete.Count > 0));
end;

function TCustomMyDataSetService.CanUseAllKeyFields: boolean;
var
  ByTable: boolean;
begin
// upd is this really nec. here
  ByTable := False;
  if UpdatingTableInfoIdx >= 0 then
{    if Self is TCustomMyTable then
      ByTable := True
    else}
      if not IsFullRefresh then
        ByTable := True;

  Result := not ByTable;
end;

procedure TCustomMyDataSetService.FillFieldDescs(out FieldDescs: TFieldDescArray;
  FillKeyFieldDescs, ForceUseAllFields: boolean);

  procedure ProcessField(FieldDesc: TMySQLFieldDesc);
  begin
    if FillKeyFieldDescs then begin
      if FieldDesc.IsKey then begin
        SetLength(FieldDescs, Length(FieldDescs) + 1);
        FieldDescs[High(FieldDescs)] := FieldDesc;
      end;
    end
    else begin
      if not FieldDesc.ReadOnly then begin
        SetLength(FieldDescs, Length(FieldDescs) + 1);
        FieldDescs[High(FieldDescs)] := FieldDesc;
      end;
    end;
  end;

  procedure CheckPrimaryKeys;
  var
    HasPrimaryKeys: boolean;
    i, j: integer;
  begin
    HasPrimaryKeys := False;
    for i := Length(FieldDescs) - 1 downto 0 do begin
      HasPrimaryKeys := (FieldDescs[i] as TMySQLFieldDesc).IsPrimaryKey;
      if HasPrimaryKeys then
        Break;
    end;

    if HasPrimaryKeys then begin
      j := 0;
      for i := 0 to Length(FieldDescs) - 1 do begin
        if (FieldDescs[i] as TMySQLFieldDesc).IsPrimaryKey then begin
          if i <> j then
            FieldDescs[j] := FieldDescs[i];
          Inc(j);
        end;
      end;
      if Length(FieldDescs) <> j then
        SetLength(FieldDescs, j);
    end;
  end;

var
  Field: TField;
  FieldDesc: TMySQLFieldDesc;
  i: integer;
  IsNeedProcessField: boolean;
begin
  FieldDescs := nil;

  if (FDataSet.Fields.Count = 0) or (GetIRecordSet.Fields.Count = 0) then
    Exit;

  for i := 0 to FDataSet.Fields.Count - 1 do begin
    Field := FDataSet.Fields[i];
    if Field.FieldKind = fkData then begin
      FieldDesc := FDataSet.GetFieldDesc(Field) as TMySQLFieldDesc;

      IsNeedProcessField := ForceUseAllFields or //(Self is TCustomMyTable) or
        (FieldDesc.TableInfo = UpdatingTableInfo) or
        ((FieldDesc.TableInfo = nil) and not FDataSet.Options.SetFieldsReadOnly);

      if IsNeedProcessField then
        ProcessField(FieldDesc);
    end;
  end;

  if FillKeyFieldDescs then
    CheckPrimaryKeys;
end;

procedure TCustomMyDataSetService.FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean);
begin
  if GetKeyFields <> '' then
    inherited FillKeyFieldDescs(KeyFieldDescs, ForceUseAllKeyFields)
  else
    FillFieldDescs(KeyFieldDescs, True, ForceUseAllKeyFields);
end;

procedure TCustomMyDataSetService.FillDataFieldDescs(out DataFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean);
begin
  FillFieldDescs(DataFieldDescs, False, ForceUseAllKeyFields);
end;

function TCustomMyDataSetService.DetectIdentityField: TCRFieldDesc;
var
  RecordSet: TMySQLRecordSet;
  FieldDesc: TMySQLFieldDesc;
  i: integer;
begin
  Result := nil;
  RecordSet := TMySQLRecordSet(GetIRecordSet);
  /// downto to correct set FIdentityField
  for i := 0 to RecordSet.FieldCount - 1 do begin
    FieldDesc := TMySQLFieldDesc(RecordSet.Fields[i]);

    if FieldDesc.FieldDescKind = fdkData then begin
      if FieldDesc.IsAutoIncrement then begin
        Assert((FUpdatingTableInfoIdx >= - 1) and (FUpdatingTableInfoIdx < GetTablesInfo.Count));
        if (FUpdatingTableInfoIdx >= 0) and (FieldDesc.TableInfo <> nil)
          and (FieldDesc.TableInfo.TableName = GetTablesInfo[FUpdatingTableInfoIdx].TableName) then begin
          Result := FieldDesc;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TCustomMyDataSetService.CloseCursor;
begin
  inherited;
  FTimestampField := nil;
end;

procedure TCustomMyDataSetService.InitCursor;
var
  Field: TField;
  FieldDesc: TMySQLFieldDesc;
  i: integer;
begin
  inherited;

  FTimestampField := nil;

  /// downto to correct set FIdentityField
  for i := FDataSet.Fields.Count - 1 downto 0 do begin
    Field := FDataSet.Fields[i];

    if Field.FieldKind = fkData then begin
      FieldDesc := TMySQLFieldDesc(FDataSet.GetFieldDesc(Field));

      if FieldDesc.IsAutoIncrement then begin
        if FDataSet.Options.SetFieldsReadOnly and FAutoIncrementReadOnly then
          Field.ReadOnly := True;

      {$IFNDEF FPC}
        Field.AutoGenerateValue := arAutoInc;
      {$ENDIF}
      end;

      if FieldDesc.MySQLType = FIELD_TYPE_TIMESTAMP then
        if (FTimestampField = nil) or FieldDesc.IsCurrentTimestamp then
          FTimestampField := FieldDesc;
    end;
  end;
end;

procedure TCustomMyDataSetService.FillFieldsDefaultValues;

  // Must be sync with MyClasses DateTimeFromStr!!!
  function DateTimeFromStr(const Def: string; FieldDesc: TMySQLFieldDesc; var Res: TDateTime): boolean;
  //var
    //Len: integer;

    function IntAt(const Off, Len: integer): integer; // return decoded integer from string
    var
      i: integer;
    begin
      Result := 0;
      for i := 1 + Off to Off + Len do
        Result := Result * 10 + (Byte(Def[i]) - $30 {Ord('0')});
    end;

    function Year2: integer;
    begin
      Result := IntAt(0, 2);
      if Result >= 70 then
        Result := 1900 + Result
      else
        Result := 2000 + Result;
    end;

  const
    HoursPerDay = 24;

  var
    i: integer;
    t: TDateTime;
    //s: string;
  begin
    Result := True;
    //Len := Length(Def);

    case TMySQLFieldDesc(FieldDesc).MySQLType of
      FIELD_TYPE_TIMESTAMP:
        case FieldDesc.Length of
          19: // YYYY-MM-DD HH:MM:SS
            Result := TryEncodeDateTime(IntAt(0, 4), IntAt(5, 2), IntAt(8, 2), IntAt(11, 2), IntAt(14, 2), IntAt(17, 2), 0, Res);
          14: // YYYYMMDDHHMMSS
            Result := TryEncodeDateTime(IntAt(0, 4), IntAt(4, 2), IntAt(6, 2), IntAt(8, 2), IntAt(10, 2), IntAt(12, 2), 0, Res);
          12: // YYMMDDHHMMSS
            Result := TryEncodeDateTime(Year2, IntAt(2, 2), IntAt(4, 2), IntAt(6, 2), IntAt(8, 2), IntAt(10, 2), 0, Res);
          10: // YYMMDDHHMM
            Result := TryEncodeDateTime(Year2, IntAt(2, 2), IntAt(4, 2), IntAt(6, 2), IntAt(8, 2), 0, 0, Res);
          8:  // YYYYMMDD
            Result := TryEncodeDate(IntAt(0, 4), IntAt(4, 2), IntAt(6, 2), Res);
          6:  // YYMMDD
            Result := TryEncodeDate(Year2, IntAt(2, 2), IntAt(4, 2), Res);
          4:  // YYMM
            Result := TryEncodeDate(Year2, IntAt(2, 2), 1, Res);
          2:  // YY
            Result := TryEncodeDate(Year2, 1, 1, Res);
          else
            Assert(False, 'Invalid FIELD_TYPE_TIMESTAMP FieldDesc.Length (' + IntToStr(FieldDesc.Length) + ')');
        end;
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE: // YYYY-MM-DD
        Result := TryEncodeDate(IntAt(0, 4), IntAt(5, 2), IntAt(8, 2), Res);
      FIELD_TYPE_TIME: // HH:MM:SS
      begin
        i := 0;
        while Byte(Def[i + 1]) <> Byte(':') do begin
          if i >= 4 then
             DatabaseErrorFmt('Wrong time format. FieldDesc %s, value %s', [FieldDesc.Name, Def]);
          Inc(i);
        end;
        Res := (IntAt(i + 1, 2) * 60 + IntAt(i + 4, 2)) / SecsPerDay; // MM:SS
        if Byte(Def[1]) = Byte('-') then
          Res := - IntAt(1, i - 1) / HoursPerDay - Res
        else
          Res :=   IntAt(0, i)     / HoursPerDay + Res;
      end;
      FIELD_TYPE_DATETIME: // YYYY-MM-DD HH:MM:SS
      begin
        Result := TryEncodeDate(IntAt(0, 4), IntAt(5, 2), IntAt(8, 2), Res);
        if Result then begin
          Result := TryEncodeTime(IntAt(11, 2), IntAt(14, 2), IntAt(17, 2), 0, t);
          if Res >= 0 then
            Res := Res + t
          else
            Res := Res - t;
        end;
      end;
    end;
  end;

var
  MetaData: TDAMetaData;
  TableName, Schema: _string;
  p: integer;
  NameField, DefField: TField;
  Field: TField;
  FieldDesc: TMySQLFieldDesc;
  Def: string;
  dt: TDateTime;
  v: Variant;
begin
  if not FDataSet.Options.DefaultValues or (FDataSet.Fields.Count = 0) or (FUpdatingTableInfoIdx = - 1) then
    Exit;

  TableName := GetTablesInfo[FUpdatingTableInfoIdx].TableNameFull;
  p := Pos('.', TableName);
  if p > 0 then begin
    Schema := Copy(TableName, 1, p - 1);
    TableName := Copy(TableName, p + 1, Length(TableName));
  end
  else begin
    GetIConnection.GetProp(prDatabase, v);
    Schema := v;
  end;

  MetaData := UsedConnection.CreateMetaData;
  try
    MetaData.MetaDataKind := 'Columns';
    MetaData.Restrictions.Text := 'table_schema=' + Schema +
      #13#10'table_name=' + TableName;

    MetaData.Open;
    NameField := MetaData.FieldByName('COLUMN_NAME');
    DefField := MetaData.FieldByName('DEFAULT_VALUE');
    while not MetaData.Eof do begin
      Field := FDataSet.FindField(NameField.AsString);
      if (Field <> nil) and not DefField.IsNull then begin
        Def := DefField.AsString;
        if DefaultExpressionOldBehavior then begin
          FieldDesc := FDataSet.GetFieldDesc(Field) as TMySQLFieldDesc;
          case Field.DataType of
            ftBoolean:
              Field.DefaultExpression := BoolToStr(Def <> '0', True);
            ftFloat, ftBCD{$IFDEF VER6P}, ftFMTBCD{$ENDIF}:
              Field.DefaultExpression := StringReplace(Def, '.', DecimalSeparator, [rfReplaceAll]);
            ftDateTime:
              if (Def <> '') and DateTimeFromStr(Def, FieldDesc, dt) then
                Field.DefaultExpression := DateTimeToStr(dt);
            ftDate:
              if (Def <> '') and DateTimeFromStr(Def, FieldDesc, dt) then
                Field.DefaultExpression := DateToStr(dt);
            ftTime:
              if (Def <> '') and DateTimeFromStr(Def, FieldDesc, dt) then
                Field.DefaultExpression := TimeToStr(dt);
            else
              Field.DefaultExpression := Def;
          end;
        end
        else begin
          if Def <> 'CURRENT_TIMESTAMP' then
            if (Field.DataType in [ftDateTime, ftDate, ftTime]) and
              ((Def = '') or not DateTimeFromStr(Def, FDataSet.GetFieldDesc(Field) as TMySQLFieldDesc, dt)) then
              Def := ''
            else
              Def := AnsiQuotedStr(Def, '''');
          Field.DefaultExpression := Def;
        end;
      end;
      MetaData.Next;
    end;
  finally
    MetaData.Free;
  end;
end;

procedure TCustomMyDataSetService.SetFieldOrigin(Field: TField; FieldDesc: TCRFieldDesc);
var
  TblAlias: _string;
begin
  if FieldDesc.TableInfo <> nil then begin
    TblAlias := FieldDesc.TableInfo.TableAlias;
    if TblAlias = '' then
      TblAlias := FieldDesc.TableInfo.TableName;
    TblAlias := MySQLInfo.NormalizeName(TblAlias, False, True);
    Field.Origin := TblAlias + '.' + FieldDesc.ActualName;
  end
  else
    Field.Origin := FieldDesc.ActualName;
end;

function TCustomMyDataSetService.GetRecCount: integer;
  function GetCount(const s: _string): longint;
  var
    UQ: TCustomDADataSet;
    i: integer;
    MonitorClass: TDASQLMonitorClass;
    MessageID: cardinal;
  begin
    FUpdater.CheckUpdateQuery(stCustom);
    UQ := FUpdater.UpdateQuery as TCustomDADataSet;
    UQ.SQL.Text := s;

    UQ.Macros.Assign(FDataSet.Macros);
    for i := 0 to FDataSet.Params.Count - 1 do
      UQ.Params[i].Assign(FDataSet.Params[i]);

    MonitorClass := TDASQLMonitorClass(TDBAccessUtils.SQLMonitorClass(UsedConnection));
    if not TDBAccessUtils.GetLockDebug(FDataSet) and (MonitorClass.HasMonitor or FDataSet.Debug) then
      MonitorClass.SQLExecute(FDataSet, s, UQ.Params, 'Get RecCount', MessageID, True);

    FInGetRecCount := True;
    try
      UQ.Execute;
      Result := UQ.Fields[0].AsInteger;
    finally
      FInGetRecCount := False;
    end;

    if not TDBAccessUtils.GetLockDebug(FDataSet) and (MonitorClass.HasMonitor or FDataSet.Debug) then
      MonitorClass.SQLExecute(FDataSet, s, UQ.Params, 'Get RecCount', MessageID, False);
  end;

var
  Parser: TMyParser;
  SelectPos: integer;
  FromPos: integer;
  s: _string;
  Lexem: integer;
  DelimiterPos: integer;
  HaveDistinct: boolean;

begin
  Result := 0;
  if (not IsFetchAll and FDataSet.Options.QueryRecCount) // DefaultResultSet with FetchAll = False
    and not ((FDataSet.Params.Count > 0) and (FDataSet.Params[0].ParamType = ptResult)) then begin // Current SQL does not have RETURN parameter
    s := FDataSet.FinalSQL;
    s := {$IFDEF CLR}Devart.Dac.{$ENDIF}DBAccess._SetOrderBy(s, '', TMyParser);
    Parser := TMyParser.Create(s);
    Parser.OmitBlank := True;
    Parser.OmitComment := True;
    try
      if Parser.ToLexem(lxSELECT) <> lcEnd then begin
        SelectPos := Parser.CurrPos;
        Lexem := Parser.GetNextCode;
        HaveDistinct := (Lexem = lxDISTINCT) or (Lexem = lxDISTINCTROW);

        if Parser.ToLexem(lxFROM) <> lcEnd then begin
          FromPos := Parser.CurrPos;

          if Parser.ToLexem(lxLIMIT) <> lcEnd then begin
            if Parser.ToLexem(7) <> lcEnd then // ';'
              DelimiterPos := Parser.CurrPos
            else
              DelimiterPos := MaxInt;
            s := Copy(s, 1, SelectPos) + ' COUNT(*) FROM (SELECT ' + Copy(s, SelectPos + 1, DelimiterPos - 1 - SelectPos) + ') q' + Copy(s, DelimiterPos, MaxInt);
          end
          else
          if HaveDistinct then
            s := Copy(s, 1, SelectPos) + ' COUNT(' + Copy(s, SelectPos + 1, FromPos - 4 - SelectPos) + ')' + Copy(s, FromPos - 4 {length('FROM')}, MaxInt)
          else
            s := Copy(s, 1, SelectPos) + ' COUNT(*)' + Copy(s, FromPos - 4 {length('FROM')}, MaxInt);

          Result := GetCount(s);
        end;
      end;
    finally
      Parser.Free;
    end;
  end
  else
    Result := inherited GetRecCount;
end;

procedure TCustomMyDataSetService.BreakExec;
var
  UQ: TCustomDADataSet;
begin
  if FInGetRecCount then
    if FUpdater.UpdateQuery <> nil then begin
      UQ := FUpdater.UpdateQuery as TCustomDADataSet;
      UQ.BreakExec;
    end;
end;

function TCustomMyDataSetService.Executing: boolean;
begin
  Result := FInGetRecCount;
end;

procedure TCustomMyDataSetService.SetNumberRange(FieldDef: TFieldDef);
var
  Field: TField;
  FieldDesc: TMySQLFieldDesc;
{$IFDEF VER6P}
  e: Extended;
{$ENDIF}

begin
  Field := FDataSet.FindField(FieldDef.Name);
  if (Field <> nil)
    and (Field is TNumericField) then begin
    FieldDesc := TMySQLFieldDesc(FDataSet.GetFieldDesc(Field));
    case FieldDesc.MySQLType of // Must be sync with ConvertMySQLTypeToInternalFormat
      // Integer fields
      FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL: begin // DECIMAL
        if Field is TFloatField then begin
          TFloatField(Field).MaxValue :=
            IntPower(10, FieldDesc.Length - FieldDesc.Scale) -
            IntPower(10, - Integer(FieldDesc.Scale));
          TFloatField(Field).MinValue := - TFloatField(Field).MaxValue;
        end
        else
        if Field is TBCDField then begin
          TBCDField(Field).MaxValue :=
            IntPower(10, FieldDesc.Length - FieldDesc.Scale) -
            IntPower(10, - Integer(FieldDesc.Scale));
          TBCDField(Field).MinValue := - TBCDField(Field).MaxValue;
        end
        else
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        if Field is TFMTBCDField then begin
          e :=
            IntPower(10, FieldDesc.Length - FieldDesc.Scale) -
            IntPower(10, - Integer(FieldDesc.Scale));
          TFMTBCDField(Field).MaxValue := FloatToStr(e);
          TFMTBCDField(Field).MinValue := FloatToStr(- e);
        end
        else
      {$ENDIF}
      {$ENDIF}
          Assert(False, Field.ClassName);
      end;
      FIELD_TYPE_TINY: // TINYINT
        if FieldDesc.IsUnsigned then
        begin
          TIntegerField(Field).MinValue := 0;
          TIntegerField(Field).MaxValue := 255;
        end
        else
        begin
          TIntegerField(Field).MinValue := -128;
          TIntegerField(Field).MaxValue := 127;
        end;

      FIELD_TYPE_SHORT: // SMALLINT
        if FieldDesc.IsUnsigned then
        begin
          TIntegerField(Field).MinValue := 0;
          TIntegerField(Field).MaxValue := 65535;
        end
        else
        begin
          TIntegerField(Field).MinValue := -32768;
          TIntegerField(Field).MaxValue := 32767;
        end;

      FIELD_TYPE_LONG, FIELD_TYPE_LONGLONG: // INT
        if Field is TIntegerField then
          if FieldDesc.IsUnsigned then
          begin
            TIntegerField(Field).MinValue := 0;
            TIntegerField(Field).MaxValue := 0; // 4294967295; - not supported by Delphi
          end
          else
          begin
            TIntegerField(Field).MinValue := -2147483647;
            TIntegerField(Field).MaxValue := 2147483647;
          end;

      // FIELD_TYPE_LONGLONG:; // BIGINT - does not need to set bounds

      FIELD_TYPE_INT24: begin// MEDIUMINT
        // ??? Error in Delphi? Does not set range value
        // May be d5/d6?

        Assert(Field is TIntegerField);
        if FieldDesc.IsUnsigned then
        begin
          TIntegerField(Field).MinValue := 0;
          TIntegerField(Field).MaxValue := 16777215;
        end
        else
        begin
          TIntegerField(Field).MinValue := -8388608;
          TIntegerField(Field).MaxValue := 8388607;
        end;
      end;

      // Float fields
      FIELD_TYPE_FLOAT: // FLOAT
      begin
        TFloatField(Field).Precision := FieldDesc.Length;
        TFloatField(Field).MinValue := -3.402823466E+38;
        TFloatField(Field).MaxValue :=  3.402823466E+38;
      end;
      FIELD_TYPE_DOUBLE: // DOUBLE
      begin
        TFloatField(Field).Precision := FieldDesc.Length;
        TFloatField(Field).MinValue := -1.7976931348623157E+308;
        TFloatField(Field).MaxValue :=  1.7976931348623157E+308;
      end;

      FIELD_TYPE_YEAR:
      begin
        Assert(Field is TIntegerField);
        case FieldDesc.Length of
          2: begin
            TIntegerField(Field).MinValue := 0;
            TIntegerField(Field).MaxValue := 99;
          end;
          4: begin
            TIntegerField(Field).MinValue := 0;
            TIntegerField(Field).MaxValue := 255;
          end;
          else
            Assert(False);
        end;
      end;
    end;
  end;
end;

procedure TCustomMyDataSetService.WriteFieldXMLDataType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: _string; XMLWriter: XMLTextWriter);
begin
  inherited;

  if FieldDesc is TMySQLFieldDesc then begin
    if TMySQLFieldDesc(FieldDesc).IsAutoIncrement
      and not (Field.Required and not Field.ReadOnly) // Already writed in MemDS
    then
      XmlWriter.WriteAttributeString('rs:maybenull', 'false');
  end;
end;

procedure TCustomMyDataSetService.WriteFieldXMLAttributeType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: _string; XMLWriter: XMLTextWriter);
begin
  inherited;

  if FieldDesc is TMySQLFieldDesc then begin
    if TMySQLFieldDesc(FieldDesc).MySQLType = FIELD_TYPE_TIMESTAMP then
      XmlWriter.WriteAttributeString('rs:rowver', 'true');
  end;
end;

function TCustomMyDataSetService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCheckRowVersion:
      FCheckRowVersion := Value;
    prLockType:
      FLockType := TLockRecordTypeI(Value);
    prAutoIncrementReadOnly:
      FAutoIncrementReadOnly := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

// Open next rowset in statement. if rowset not returne theh OpenNext return False. If statement has error, then raised exception
function TCustomMyDataSetService.OpenNext: boolean;
var
  v: variant;
begin
  if not FDataSet.Active then begin
    FDataSet.Open;
    Result := True;
  end
  else begin
    BeginConnection;
    try
      if FDataSet.Prepared then
        DatabaseError(SOpenNextPreparedSQL);
      if IsFetchAll then
        DatabaseError(SOpenNextVsFetchAll);

      FDataSet.Close;
      FDataSet.Unprepare;

      GetIRecordSet.GetProp(prIsCanOpenNext, v);
      Result := v;

      if Result then
        try
          GetIRecordSet.SetProp(prOpenNext, True);
          FDataSet.FieldDefs.Updated := False;
          FDataSet.Open;
        finally
          GetIRecordSet.SetProp(prOpenNext, False);
        end;
    finally
      EndConnection;
    end;
  end;
end;

function TCustomMyDataSetService.GetIConnection: TMySQLConnection;
begin
  Result := TMySQLConnection(TDBAccessUtils.GetIConnection(UsedConnection));
  Assert(Result <> nil); //upd should be error
end;

function TCustomMyDataSetService.GetIRecordSet: TMySQLRecordSet;
begin
  Result := TMySQLRecordSet(inherited GetIRecordSet);
end;

function TCustomMyDataSetService.IsFullRefresh: boolean;
begin
  Result := inherited IsFullRefresh;
end;

{ TMyServerEnumerator }

function TMyServerEnumerator.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := inherited SetProp(Prop, Value);
end;

function TMyServerEnumerator.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := inherited GetProp(Prop, Value);
end;

procedure TMyServerEnumerator.GetServerList(List: _TStrings);
{$IFDEF MSWINDOWS}
var
  List1: TStringList;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  List1 := TStringList.Create;
  try
    CRNetManager.GetServerList(List1, 'mysql');
    AssignStrings(List1, List);
  finally
    List1.Free;
  end;
{$ENDIF}
end;

{ TCustomMyDumpProcessor }

constructor TCustomMyDumpProcessor.Create(Owner: TDADump);
begin
  inherited;

  FBackupData := True;
end;

function TCustomMyDumpProcessor.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prBackupTables:
      FBackupTables := Value;
    prBackupViews:
      FBackupViews := Value;
    prBackupData:
      FBackupData := Value;
    prAddLock:
      FAddLock := Value;
    prDisableKeys:
      FDisableKeys := Value;
    prHexBlob:
      FHexBlob := Value;
    prUseExtSyntax:
      FUseExtSyntax := Value;
    prUseDelayedIns:
      FUseDelayedIns := Value;
    prCommitBatchSize:
      FCommitBatchSize := Value;
    prInsertType:
      FInsertType := _TMyInsertType(Value);
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCustomMyDumpProcessor.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prBackupTables:
      Value := FBackupTables;
    prBackupViews:
      Value := FBackupViews;
    prBackupData:
      Value := FBackupData;
    prAddLock:
      Value := FAddLock;
    prDisableKeys:
      Value := FDisableKeys;
    prHexBlob:
      Value := FHexBlob;
    prUseExtSyntax:
      Value := FUseExtSyntax;
    prUseDelayedIns:
      Value := FUseDelayedIns;
    prCommitBatchSize:
      Value := FCommitBatchSize;
    prInsertType:
      Value := Variant(FInsertType);
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

{$IFDEF VER12P}
procedure TCustomMyDumpProcessor.Backup(Query: _string);
var
  IConnection: TMySQLConnection;
  v: Variant;
  s: AnsiString;
begin
  if GetStream <> nil then begin
    IConnection := TMySQLConnection(TDBAccessUtils.GetIConnection(GetConnection));
    IConnection.GetProp(prUseUnicode, v);
    FUseUnicode := v;
    if FUseUnicode then begin
      s := #$EF#$BB#$BF;
      GetStream.WriteBuffer(s[1], Length(s));
    end;
  end;

  inherited;
end;

procedure TCustomMyDumpProcessor.Add(const Line: _string);
var
  s: AnsiString;
  buf: TBytes;
begin
  if GetStream = nil then
    inherited
  else begin
    if FUseUnicode then begin
      buf := Encoding.UTF8.GetBytes(UTF8Encode(Line + #$D#$A));
      GetStream.WriteBuffer(buf[0], Length(buf));
    end
    else begin
      s := AnsiString(Line + #$D#$A);
      GetStream.WriteBuffer(s[1], Length(s));
    end;
  end;
end;
{$ENDIF}

function TCustomMyDumpProcessor.CreateQuery: TCustomDADataSet;
begin
  Result := GetConnection.CreateDataSet;
  TDBAccessUtils.CheckConnection(Result);
  Result.ReadOnly := True;
  Result.UniDirectional := True;
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prNullForZeroDate, False);
  Result.Options.QueryRecCount := False;
  Result.Options.LongStrings := False; // To prevent MySQL bug
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prCreateConnection, False);
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prEnableBoolean, False);
end;

procedure TCustomMyDumpProcessor.AddSettings;
var
  IConnection: TMySQLConnection;
  v: variant;
begin
  IConnection := TMySQLConnection(TDBAccessUtils.GetIConnection(GetConnection));

  IConnection.GetProp(prUseUnicode, v);
  if Boolean(v) then
    FCharset := 'utf8'
  else begin
    IConnection.GetProp(prCharset, v);
    FCharset := v;
  end;

  if FCharset <> '' then begin
    Add('/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;');
    Add('/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;');
    Add('/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;');
    Add('/*!40101 SET NAMES ' + FCharset + ' */;')
  end;

  if FBackupTables or FBackupData or FBackupViews then begin
    Add('/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;');
    Add('/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE=''NO_AUTO_VALUE_ON_ZERO'' */;');
  end;
end;

procedure TCustomMyDumpProcessor.RestoreSettings;
begin
  if FBackupTables or FBackupData or FBackupViews then begin
    Add('/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;');
    Add('/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;');
  end;

  if FCharset <> '' then begin
    Add('/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;');
    Add('/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;');
    Add('/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;');
  end;
end;

procedure TCustomMyDumpProcessor.BackupObjects(const Query: _string);
var
  IConnection: TMySQLConnection;
  v: variant;
  TableCount: integer;
  UseUnicode: boolean;

  procedure BackupTablesAndData;
  var
    MyQueryView: TCustomDADataSet;

    function GetIsView(const TableName: _string): boolean;
    var
      Database: _string;
      ObjName: _string;
      NewSQL: _string;
    begin
      Result := False;
      if (IConnection.ServerPrimaryVer < 5) or (TableName = '') then
        Exit;

      MySQLInfo.SplitObjectName(TableName, Database, ObjName);
      Database := MySQLInfo.NormalizeName(Database, False, True);
      ObjName := MySQLInfo.NormalizeName(ObjName, False, True);

      if MyQueryView = nil then begin
        MyQueryView := GetConnection.CreateDataSet;
        MyQueryView.ReadOnly := True;
        //MyQueryView.FetchAll := True;

        if Database = '' then begin
          IConnection.GetProp(prDatabase, v);
          Database := v;
        end;

        NewSQL := 'SHOW TABLE STATUS FROM ' + QuoteName(Database);
        if NewSQL <> TrimRight(MyQueryView.SQL.Text) then begin
          MyQueryView.SQL.Text := NewSQL;
          MyQueryView.Open;
        end;
      end;

      if MyQueryView.LocateEx('Name', ObjName, [lxCaseInsensitive]) then
        Result := MyQueryView.FieldByName('Engine').AsString = '';
    end;

    procedure BackupTable(
      TableName: _string;
      TableNum: integer;
      IsView: boolean
    );
    var
      KeyAndDataFields: TKeyAndDataFields;
      NeedBackslashes: boolean;
      Charset: string;

      procedure GetCurrentRow(CurrentRowSB: _StringBuilder);
      var
        OldCurrentRowSBLen: integer;

        procedure ProcessField(Field: TField);
        var
          Value: Variant;
          SValue, s: {$IFDEF VER12P}string{$ELSE}AnsiString{$ENDIF}; //+++
          FieldDesc: TMySQLFieldDesc;
          Blob: TBlob;
        {$IFDEF CLR}
          Buffer: TBytes;
          Bytes: TBytes;
          sb: StringBuilder;
        {$ELSE}
          sbOffset: integer;
        {$ENDIF}
        {$IFDEF VER12P}
          ws: WideString;
        {$ENDIF}
          Piece: PPieceHeader;
        begin
          if CurrentRowSB.Length > OldCurrentRowSBLen then
            CurrentRowSB.Append(', ');

          if Field.DataSet = FQuery then
            FieldDesc := FQuery.GetFieldDesc(Field) as TMySQLFieldDesc
          else
            FieldDesc := FQuery.GetFieldDesc(Field.FieldName) as TMySQLFieldDesc; // Just in case.

          Blob := nil;
          if TDBAccessUtils.GetIRecordSet(FQuery).IsBlobFieldType(FieldDesc.DataType) then
            Blob := TMemDSUtils.GetBlob(FQuery, FieldDesc);

          Value := Unassigned;
          if Field.IsNull then
            CurrentRowSB.Append('NULL')
          else
          begin
            if FHexBlob and ((FieldDesc.IsBinary and (FieldDesc.DataType = dtString)) or
               (FieldDesc.DataType = dtBlob)) then begin
              case FieldDesc.DataType of
                dtString: begin
                  s := Field.AsString;
                {$IFDEF CLR}
                  Buffer := Encoding.Default.GetBytes(s);
                  SetLength(Bytes, Length(Buffer) * 2);
                  BinToHex(Buffer, 0, Bytes, 0, Length(Buffer));
                  SValue := Encoding.Default.GetString(Bytes);
                {$ELSE}
                  SetLength(SValue, Length(s) * 2);
                  BinToHex(IntPtr(s), PAnsiChar(@SValue[1]), Length(s));
                {$ENDIF}
                end;
                dtBlob: begin
                  Piece := Blob.FirstPiece;
                {$IFDEF CLR}
                  sb := StringBuilder.Create;
                  try
                    while IntPtr(Piece) <> nil do begin
                      SetLength(Buffer, Piece.Used);
                      Marshal.Copy(PtrOffset(Piece, sizeof(TPieceHeader)), Buffer, 0, Piece.Used);
                      SetLength(Bytes, Length(Buffer) * 2);
                      BinToHex(Buffer, 0, Bytes, 0, Length(Buffer));
                      sb.Append(Encoding.Default.GetString(Bytes));
                      Piece := Piece.Next;
                    end;
                    SValue := sb.ToString;
                  finally
                    sb.Free;
                  end;
                {$ELSE}
                  SetLength(SValue, Integer(Blob.Size) * 2);
                  sbOffset := 0;
                  while Piece <> nil do begin
                    BinToHex(PtrOffset(Piece, sizeof(TPieceHeader)),
                      {$IFDEF VER12P}PWideChar{$ELSE}PAnsiChar{$ENDIF}(PtrOffset(Pointer(SValue), sbOffset)), Piece.Used);
                    sbOffset := sbOffset + Integer(Piece.Used) * 2;
                    Piece := Piece.Next;
                  end;
                {$ENDIF}
                end;
              end; // case
              if Length(SValue) > 0 then begin
                CurrentRowSB.Append('0x');
                CurrentRowSB.Append(SValue);
              end
              else
                CurrentRowSB.Append('''''');
            end
            else begin
              if Blob = nil then
                Value := Field.AsVariant
              else
              begin
              {$IFDEF CLR}
                Value := Variant(Blob);
              {$ELSE}
                TVarData(Value).VType := varByRef{$IFDEF FPC} or varVariant{$ENDIF};
                TVarData(Value).VPointer := Blob;
              {$ENDIF}
              end;

            {$IFDEF VER12P}
              if UseUnicode and
                (FieldDesc.DataType in [dtWideString, dtExtWideString, dtWideMemo]) and
                (VarType(Value) <> varByRef{$IFDEF FPC} or varVariant{$ENDIF}) and
                (VarType(Value) <> varArray + varByte) then begin
                ws := WideString(Value);
                CurrentRowSB.Append(EscapeAndQuoteWideStr(ws, Length(ws), NeedBackslashes));
              end
              else
            {$ENDIF}
                // Data already is encoded in Unicode, therefore there's no need to encode it once again
                AppendValueToSQL(CurrentRowSB, FieldDesc.DataType, Value, False,
                  (Blob <> nil) and UseUnicode, {$IFDEF HAVE_COMPRESS}False, {$ENDIF}
                  NeedBackslashes, IConnection.ServerPrimaryVer, Charset);
            end;
          end;
        end;

      var
        i: integer;
      begin
        OldCurrentRowSBLen := CurrentRowSB.Length;

        if Length(KeyAndDataFields.DataFieldDescs) = 0 then begin
          for i := 0 to FQuery.FieldCount - 1 do
            ProcessField(FQuery.Fields[i]);
        end
        else
          for i := 0 to Length(KeyAndDataFields.DataFieldDescs) - 1 do
            ProcessField(FQuery.GetField(KeyAndDataFields.DataFieldDescs[i]));
      end;

    var
      InsSize, RecordCount: integer;
      s: _string;
      CurrentRowSB: _StringBuilder;
      InsHeader: _string;
      NeedInsHeader: boolean;
      FieldList, FieldName: _string;
      i: integer;
      SQLSelect1: _string;
      ShortTableName: _string;
      dbName: _string;
      ReplaceTableNameNeed: boolean;
      v: variant;

      procedure GetFieldList(var List: _string);
      var
        j: integer;
        OldActive: boolean;
      begin
        OldActive := FQuery.Active;
        try
          if not FQuery.Active then begin   // in case of BackupQuery
            FQuery.SQL.Text := SQLSelect1;
            FQuery.AddWhere('0=1');
            FQuery.Open;
          end;

          if (TableName = '')
            and (TDBAccessUtils.GetTablesInfo(FQuery).Count > 0) then begin
            TableName := QuoteName(TDBAccessUtils.GetTablesInfo(FQuery)[0].TableName);
            IsView := GetIsView(TableName);
            DoBackupProgress(TableName, 0, 1, 0);
          end;

          TDBAccessUtils.GetKeyAndDataFields(FQuery, KeyAndDataFields, False);
          for j := 0 to Length(KeyAndDataFields.DataFieldDescs) - 1 do begin
            FieldName := QuoteName(KeyAndDataFields.DataFieldDescs[j].Name);
            if List = '' then
              List := FieldName
            else
              List := List + ', ' + FieldName;
          end;

         finally
          if not OldActive then
            FQuery.Close;
         end;
      end;

    begin
      FieldList := '';

      if Query <> '' then begin  // if BackupQuery, no optimization
        SQLSelect1 := Query;
        GetFieldList(FieldList);
      end;

      if (FBackupTables and (not IsView)) or (FBackupViews and IsView) then begin
        ReplaceTableNameNeed := False;
        i := Pos('.', TableName);
        if i <> 0 then begin
          dbName := SQLInfo.UnQuote(Copy(TableName, 1, i - 1));
          IConnection.GetProp(prDatabase, v);
          if dbName <> v then begin
            ReplaceTableNameNeed := True;
            ShortTableName := '`' + SQLInfo.UnQuote(Copy(TableName, i + 1, Length(TableName) - i)) + '`';
            TableName := '`' + dbName + '`' + '.' + ShortTableName;
          end;
        end;

        if FBackupTables or FBackupViews then begin
          if FOwner.Options.GenerateHeader then
            AddLineToSQL(SBHTableStruct, [TableName]);

          if FOwner.Options.AddDrop then begin
            if IsView then
              Add('DROP VIEW IF EXISTS ' + TableName + ';')
            else
              Add('DROP TABLE IF EXISTS ' + TableName + ';');
          end;

          FQuery.SQL.Text := 'SHOW CREATE TABLE ' + TableName;
          FQuery.Execute;

          s := FQuery.Fields[1].AsString + ';';
        {$IFDEF CLR}
          s := StringReplace(s, #$A, LineSeparator, [rfReplaceAll]);
        {$ENDIF}
          if ReplaceTableNameNeed then
            s := StringReplace(s, ShortTableName, TableName, []);

          AddLineToSQL(s);
          Add('');
        end;

        DoBackupProgress(TableName, TableNum, TableCount, 0);
      end;

      if FBackupData and not IsView then begin
        if FOwner.Options.GenerateHeader then
          AddLineToSQL(SBHTableData, [TableName]);

        if FOwner.Options.AddDrop and not FBackupTables then
          Add('TRUNCATE TABLE ' + TableName + ';');

        FQuery.SQL.Text := 'SELECT COUNT(*) FROM ' + TableName;
        FQuery.Execute;
        RecordCount := FQuery.Fields[0].AsInteger;

        DoBackupProgress(TableName, TableNum, TableCount, 0);

        if RecordCount > 0 then begin
          if FAddLock and not FUseDelayedIns then
            Add('LOCK TABLES ' + TableName + ' WRITE;');
          if FDisableKeys then
            Add('/*!40000 ALTER TABLE ' + TableName + ' DISABLE KEYS */;');

          if Query = '' then
            SQLSelect1 := 'SELECT * FROM ' + TableName
          else
            SQLSelect1 := Query;

          FQuery.SQL.Text := SQLSelect1;
          FQuery.Execute;
          if (Query = '') and FOwner.Options.CompleteInsert then
            GetFieldList(FieldList);

          case FInsertType of
            _itInsert:
              if FUseDelayedIns then
                InsHeader := 'INSERT DELAYED INTO '
              else
                InsHeader := 'INSERT INTO ';
            _itInsertIgnore:
              if FUseDelayedIns then
                InsHeader := 'INSERT DELAYED IGNORE INTO '
              else
                InsHeader := 'INSERT IGNORE INTO ';
            _itReplaceInto:
              if FUseDelayedIns then
                InsHeader := 'REPLACE DELAYED INTO '
              else
                InsHeader := 'REPLACE INTO ';
          else
            Assert(False);
          end;

          if FieldList = '' then
            InsHeader := InsHeader + TableName + ' VALUES'
          else
            InsHeader := InsHeader + TableName + '(' + FieldList + ') VALUES';

          if FieldList <> '' then
            TDBAccessUtils.GetKeyAndDataFields(FQuery, KeyAndDataFields, False);

          IConnection.GetProp(prNeedBackslashes, v);
          NeedBackslashes := v;
          IConnection.GetProp(prCharset, v);
          Charset := v;
          CurrentRowSB := _StringBuilder.Create;
          try
            if not FUseExtSyntax then
              while not FQuery.Eof do begin
                DoBackupProgress(TableName, TableNum, TableCount, Trunc(FQuery.RecNo * 100 / RecordCount));

                if (FCommitBatchSize > 0) and (FQuery.RecNo mod FCommitBatchSize = 1) then
                  Add('BEGIN;');

                CurrentRowSB.Length := 0;
                CurrentRowSB.Append(InsHeader);
                CurrentRowSB.Append(' (');
                GetCurrentRow(CurrentRowSB);
                CurrentRowSB.Append(');');

                Add(_string(CurrentRowSB.ToString));
                FQuery.Next;

                if (FCommitBatchSize > 0) and ((FQuery.RecNo mod FCommitBatchSize = 1) or FQuery.Eof) then
                  Add('COMMIT;');
              end
            else
            begin
              InsSize := 0;
              NeedInsHeader := True;
              while not FQuery.Eof do begin
                DoBackupProgress(TableName, TableNum, TableCount, Trunc(FQuery.RecNo * 100 / RecordCount));

                if NeedInsHeader then begin
                  if FCommitBatchSize > 0 then
                    Add('BEGIN;');

                  Add(InsHeader);
                  InsSize := Length(InsHeader) + 2;
                  NeedInsHeader := False;
                end;

                CurrentRowSB.Length := 0;
                CurrentRowSB.Append('  (');
                GetCurrentRow(CurrentRowSB);
                CurrentRowSB.Append(')');
                FQuery.Next;

                Inc(InsSize, CurrentRowSB.Length + 2);
                if FQuery.Eof or (InsSize > 16384 {default net_buffer_length value} - 2 * CurrentRowSB.Length) then begin
                  NeedInsHeader := True;
                  CurrentRowSB.Append(';');

                  if FCommitBatchSize > 0 then
                    CurrentRowSB.Append(#$D#$A'COMMIT;');
                end
                else
                  CurrentRowSB.Append(',');
                Add(_string(CurrentRowSB.ToString));
              end
            end;
          finally
            CurrentRowSB.Free;
          end;

          if FDisableKeys then
            Add('/*!40000 ALTER TABLE ' + TableName + ' ENABLE KEYS */;');
          if FAddLock and not FUseDelayedIns then
            Add('UNLOCK TABLES;');
        end;
      end;
    end;

  var
    i: integer;
    TablesList: _TStringList;
    TableName: _string;
    IsView: Boolean;
    ExactNames: boolean;

  begin
    MyQueryView := nil;
    try
      if Query = '' then begin
        TablesList := nil;
        try
          TablesList := _TStringList.Create;

          if GetTables.Count = 0 then begin
            GetConnection.GetTableNames(TablesList);
            ExactNames := True;
          end
          else begin
            TablesList.Assign(GetTables);
            ExactNames := False;
          end;

          for i := 0 to TablesList.Count - 1 do begin
            if ExactNames then
              TablesList[i] := QuoteName(TablesList[i])
            else
              TablesList[i] := SQLInfo.NormalizeName(TablesList[i], FOwner.Options.QuoteNames);

            TablesList.Objects[i] := TObject(GetIsView(TablesList[i]));
          end;

          for IsView := False to True do begin
            for i := 0 to TablesList.Count - 1 do begin
              if Boolean({$IFDEF FPC}Pointer{$ENDIF}(TablesList.Objects[i])) <> IsView then
                Continue;
              if (not FBackupTables and not FBackupData)
                and (not Boolean({$IFDEF FPC}Pointer{$ENDIF}(TablesList.Objects[i]))) then
                Continue;
              if (not FBackupViews) and (Boolean({$IFDEF FPC}Pointer{$ENDIF}(TablesList.Objects[i]))) then
                Continue;

              TableName := TablesList[i];
              DoBackupProgress(TableName, i, TablesList.Count, 0);

              TableCount := TablesList.Count;
              BackupTable(TableName, i, Boolean({$IFDEF FPC}Pointer{$ENDIF}(TablesList.Objects[i])));
              Add('');
            end;
          end;

        finally
          TablesList.Free;
        end;
      end
      else
      begin
        TableCount := 1;
        BackupTable('', 0, False);
        Add('');
      end;
    finally
      MyQueryView.Free;
    end;
  end;

begin
  IConnection := TMySQLConnection(TDBAccessUtils.GetIConnection(GetConnection));
  IConnection.GetProp(prUseUnicode, v);
  UseUnicode := v;
  CheckQuery;

  if FBackupTables or FBackupData or FBackupViews then
    BackupTablesAndData;
end;

end.
