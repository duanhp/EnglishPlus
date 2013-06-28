
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 1996-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I IbDac.inc}
unit IBCServicesUni;
{$ENDIF}

interface

uses
{$IFDEF VER6P}
  Variants,
{$ENDIF}
{$IFNDEF CLR}
  CLRClasses,
{$ELSE}
  System.Text,
{$ENDIF}
  SysUtils, Classes, DB, MemUtils, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  MemData, DBAccess, CRAccess, DADump,
  {$IFNDEF UNIDACPRO}IBCCall{$ELSE}IBCCallUni{$ENDIF},
  {$IFNDEF UNIDACPRO}IBCClasses{$ELSE}IBCClassesUni{$ENDIF}, DASQLMonitor;

const
  prKeyGenerator    = 101;  // string
  prGeneratorMode   = 102;  // TGeneratorMode
  prGeneratorStep   = 103;  // integer

type
  TCustomIBCDataSetService = class;

  TCustomIBCSQLGenerator = class (TDASQLGenerator)
  protected
    FDataSetService: TCustomIBCDataSetService;

    procedure GenerateLockSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;
    procedure GenerateInsertSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
    function GenerateIndexName(Name: _string): _string; override;
    function DecodeFieldIndex(FieldName: _string): integer; override;

  public
    constructor Create(AOwner: TDADataSetService); override;

    function GenerateTableSQL(const TableName, OrderFields: _string): _string; override;
    function GenerateSelectValues(const ValuesList: _string): _string; override;
  end;

  TCustomIBCDataSetUpdater = class(TDADataSetUpdater)
  protected
    FDataSetService: TCustomIBCDataSetService;

    procedure GetGeneratorVal;
    procedure SetUpdateParsedSQLType(SQLType: integer);
    function SavepointAllowed: boolean; override;
    function CanRefreshByLock: boolean; override;

    function FieldByParamName(var ParamName: _string; var Old: boolean; var AFieldNo: integer): TField; override;
    procedure CheckUpdateQuery(const StatementType: TStatementType); override;
    procedure SetUpdateQueryOptions(const StatementType: TStatementType); override;

    function BatchUpdate: boolean; override;
    function UseParamType: boolean; override; //This function indicates ParamType using in PerformSQL
    function NeedReturnParams: boolean; override;
    function IsNeedEditPreconnect: boolean; override;
    function IsNeedInsertPreconnect: boolean; override;

    procedure PrepareAppend; override;

    function PerformAppend: boolean; override;

  public
    constructor Create(AOwner: TDataSetService); override;

    function GetDefaultExpressionValue(DefExpr: _string; var Value: variant): boolean; override;
  end;

  _TGeneratorMode = (_gmInsert, _gmPost);

  TCustomIBCDataSetService = class(TDADataSetService)
  protected
    FUpdater: TCustomIBCDataSetUpdater;

    FKeyGenerator: _string;
    FGeneratorMode: _TGeneratorMode;
    FGeneratorStep: integer;

    procedure CreateDataSetUpdater; override;
    procedure SetDataSetUpdater(Value: TDataSetUpdater); override;
    procedure CreateSQLGenerator; override;

    function UsedConnection: TCustomDAConnection;
    function IsDMLRefresh: boolean;
    function IsAutoCommit: boolean;
    procedure SetAutoCommit(Value: boolean);

    procedure SetNumberRange(FieldDef: TFieldDef); override;
    function DetectIdentityField: TCRFieldDesc; override;
    function DetectKeyGeneratorField: TField; override;
    function DetectHiddenFields: TFieldArray; override;
    procedure FillFieldsDefaultValues; override;
    function DetectCanModify: boolean; override;
    function GetRecCount: integer; override;
    function PreventPSKeyFields(var PSKeyFields: string): boolean; override;
    function NeedPreparePSExecuteCommand: boolean; override;

  public
    constructor Create(AOwner: TMemDataSet); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;

    function NormalizeStrValue(Value: _string): _string;
    function GetDBKeyList(TableName: _string; IndexName: _string = ''): _string; override;
  end;

  TCustomIBCDataTypesMap = class(TDataTypesMap)
  public
    class function GetFieldType(DataType: Word): TFieldType; override;
    class function GetDataType(FieldType: TFieldType): integer; override;
  end;

  // must use inherited class to 'is' in ChechServerEnumerator works correctly
  TIBCServerEnumerator = class (TCRServerEnumerator)
  end;

  TCustomIBCDumpProcessor = class(TDADumpProcessor)
  protected
    function GetFieldValueForDump(Field: TField): _string; override;
  end;

implementation

uses
  Math, DAConsts, CRParser, {$IFNDEF UNIDACPRO}IBCParser{$ELSE}IBCParserUni{$ENDIF};

{ TCustomIBCSQLGenerator }

constructor TCustomIBCSQLGenerator.Create(AOwner: TDADataSetService);
begin
  inherited;

  FDataSetService := TCustomIBCDataSetService(AOwner);
end;

function TCustomIBCSQLGenerator.GenerateIndexName(Name: _string): _string;
begin
  Result := 'IBC$' + Name;
end;

function TCustomIBCSQLGenerator.DecodeFieldIndex(FieldName: _string): integer;
var
  e: integer;
begin
  Result := -1;
  if (Length(FieldName) >= 5) and (FieldName[1] = 'I') and
    (FieldName[2] = 'B') and (FieldName[3] = 'C') and (FieldName[4] = '$') then begin
    Val(Copy(FieldName, 5, MaxInt), Result, e);
    if e <> 0 then
      Result := -1;
  end;
end;

{$IFNDEF CLR}
type
  _TParam = class(TCollectionItem)
  private
    FParamRef: TParam;
  end;
{$ENDIF}

procedure TCustomIBCSQLGenerator.GenerateLockSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);

var
  FieldDesc: TCRFieldDesc;
  Connection: TCustomDAConnection;
  GDSConnection: TGDSConnection;

  procedure GenerateLockCondition;
  var
    AllFields: TKeyAndDataFields;
    i: integer;
  begin
    //LockSQL where clause should contains all fields to check that there are no changes in locked record
    if (csDesigning in FDataSet.ComponentState) then  //Design-Time generation we should include key fields only (IS NULL issue)
      AllFields.DataFieldDescs := KeyAndDataFields.KeyFieldDescs
    else
      AllFields.DataFieldDescs := KeyAndDataFields.DataFieldDescs; //in Run-Time include all field (IS NULL issue)
    //Include ReadOnly Key Fields
    for i := 0 to High(KeyAndDataFields.KeyFieldDescs) do
      if KeyAndDataFields.KeyFieldDescs[i].ReadOnly then begin//This field is not included in DataFields
        SetLength(AllFields.DataFieldDescs, Length(AllFields.DataFieldDescs) + 1);
        AllFields.DataFieldDescs[High(AllFields.DataFieldDescs)] := KeyAndDataFields.KeyFieldDescs[i];
      end;
    SetLength(AllFields.KeyFieldDescs, 0);   //we should use DataFields to perform IsLargeDataTypeUsed check
    GenerateConditions(FCondSB, stUpdate, False, AllFields);
  end;

begin
  Connection := FDataSetService.UsedConnection;
  if Connection <> nil then
    GDSConnection := TGDSConnection(TDBAccessUtils.GetIConnection(Connection))
  else
    GDSConnection := nil;
  if (GDSConnection <> nil) and
    GDSConnection.IsFBServer and ((GDSConnection.GetMajorServerVersion > 1) or
    ((GDSConnection.GetMajorServerVersion = 1) and (GDSConnection.GetMinorServerVersion >= 5))) then begin // FB1.5 or higher
    FHeaderSB.Append('SELECT NULL FROM ');
    FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.Options.QuoteNames));
    FHeaderSB.Append(LineSeparator + 'WHERE'  + LineSeparator);
    GenerateLockCondition; // FCondSB
    FFooterSB.Append(LineSeparator + 'FOR UPDATE WITH LOCK');
  end
  else
    if High(KeyAndDataFields.DataFieldDescs) > 0 then begin
      FHeaderSB.Append('UPDATE ');
      FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.Options.QuoteNames));
      FHeaderSB.Append(LineSeparator + 'SET' + LineSeparator + '  ');

      FieldDesc := TCRFieldDesc(KeyAndDataFields.DataFieldDescs[0]);

      FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
      FFldSB.Append(' = ');
      AddParam(FFldSB, FieldDesc, stLock, ptUnknown, Index);

      FMiddleSB.Append(LineSeparator + 'WHERE' + LineSeparator + '  ');
      GenerateLockCondition; // FCondSB
    end;
end;

procedure TCustomIBCSQLGenerator.GenerateInsertSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  i: integer;
  Connection: TCustomDAConnection;
  GDSConnection: TGDSConnection;
  FieldDesc: TCRFieldDesc;
  IsGenerated: boolean;

  ParamInfo: TDAParamInfo;
  Param: TDAParam;
  ReturnSB: _StringBuilder;
  FieldName: _string;

  procedure AddFieldToReturning(FieldDesc: TCRFieldDesc);
  begin
    if ReturnSB.Length > 0 then
      ReturnSB.Append(', ');
    FieldName := FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames);
    ReturnSB.Append(FieldName);
    if FParams <> nil then begin //Create Returning params
      Param := TDAParam(FParams.Add);
      with Param do begin
        Name := FDataSetService.QuoteName('RET_' + FieldDesc.Name);
      {$IFNDEF CLR}
        _TParam(Param).FParamRef := Param;
      {$ENDIF}
        ParamType:= ptOutput;
        ParamInfo := TDAParamInfo(FParamsInfo.Add);
        ParamInfo.Field := FDataSet.GetField(FieldDesc);
        ParamInfo.Old := False;
        ParamInfo.ParamIndex := -1;
      end;
    end;
  end;

begin
  FDataSetService.FUpdater.SetUpdateParsedSQLType(SQL_UNKNOWN);
  Connection := FDataSetService.UsedConnection;
  if Connection <> nil then
    GDSConnection := TGDSConnection(TDBAccessUtils.GetIConnection(Connection))
  else
    GDSConnection := nil;
  if (GDSConnection <> nil) and
    GDSConnection.IsFBServer and      //if FB2.0 or higher and
    (GDSConnection.GetMajorServerVersion >= 2) and
    ((FDataSetService.IdentityField <> nil) or
    //(DMLRefresh or generator value insertion at post time)
    FDataSetService.IsDMLRefresh or
    (FDataSetService.KeyGeneratorField <> nil) and (FDataSetService.FGeneratorMode = _gmPost))
  then begin

    //generator value insertion at post time will be used
    IsGenerated := (FDataSetService.KeyGeneratorField <> nil) and (FDataSetService.FGeneratorMode = _gmPost);

    for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
      FieldDesc := KeyAndDataFields.DataFieldDescs[i];

      if IsGenerated and (FieldDesc = KeyAndDataFields.KeyFieldDescs[0]) and
        FieldIsNull(FieldDesc, False)
      then begin //Generator field
        FFldParamSB.Append('NEXT VALUE FOR ' + FDataSetService.NormalizeStrValue(FDataSetService.FKeyGenerator));
        FieldName := FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames);
        FFldSB.Append(FieldName); // Do not call AddFieldToInsertSQL cause of FFldParamSB modification
      end
      else
        if not ModifiedFieldsOnly or not FieldIsNull(FieldDesc, False) then
          AddFieldToInsertSQL(FieldDesc, Index);
    end;

    if FFldSB.Length > 0 then begin
      FHeaderSB.Append('INSERT INTO ');
      FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.Options.QuoteNames));
      FHeaderSB.Append(LineSeparator + '  (');
      // Append FFldSB
      FMiddleSB.Append(')' + LineSeparator + 'VALUES' + LineSeparator + '  (');
      // Append FFldParamSB
      FFooterSB.Append(')');
    end
    else
      FHeaderSB.Append(_Format('INSERT INTO %s () VALUES ()', [
        SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.Options.QuoteNames)]));

    ReturnSB := _StringBuilder.Create(100);
    try
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if (FDataSetService.IsDMLRefresh and not (FieldDesc.DataType in [dtMemo, dtWideMemo, dtBlob])) or
          (IsGenerated and (FieldDesc = KeyAndDataFields.KeyFieldDescs[0]))
        then
          AddFieldToReturning(FieldDesc);
      end;

      if FDataSetService.IdentityField <> nil then
        AddFieldToReturning(FDataSetService.IdentityField);

      if ReturnSB.Length > 0 then begin
        FFooterSB.Append(LineSeparator +  'RETURNING ' + LineSeparator + '  ');
        FFooterSB.Append(ReturnSB);
        FDataSetService.FUpdater.SetUpdateParsedSQLType(SQL_INSERT);
      end;
    finally
      ReturnSB.Free;
    end;
  end
  else
    inherited GenerateInsertSQL(KeyAndDataFields, ModifiedFieldsOnly, Index);

  if FFldSB.Length = 0 then begin
    Clear;
    inherited GenerateInsertSQL(KeyAndDataFields, False, Index);
  end;
end;

function TCustomIBCSQLGenerator.GenerateTableSQL(const TableName, OrderFields: _string): _string;
var
  AOrderFields: _string;
  StartPos, i: integer;
begin
  Result := 'SELECT * FROM ' + FDataSetService.NormalizeStrValue(TableName) + LineSeparator;

  AOrderFields := Trim (OrderFields);
  if AOrderFields <> '' then begin
    StartPos := 1;
    Result := Result + 'ORDER BY ';

    for i := 1 to Length(AOrderFields) do
      if (AOrderFields[i] = ';') or (AOrderFields[i] = ',') then begin
          Result := Result + Copy(AOrderFields, StartPos, i - StartPos);
          Result := Result + ',';
          StartPos := i + 1;
      end
      else
      if (i = Length(AOrderFields)) then
          Result := Result + Copy(AOrderFields, StartPos, i - StartPos + 1);
  end;
  Result := Result + LineSeparator;
end;

function TCustomIBCSQLGenerator.GenerateSelectValues(const ValuesList: _string): _string;
begin
  Result := 'SELECT ' + ValuesList + ' FROM RDB$DATABASE';
end;

{ TCustomIBCDataSetUpdater }

constructor TCustomIBCDataSetUpdater.Create(AOwner: TDataSetService);
begin
  inherited;

  FDataSetService := TCustomIBCDataSetService(AOwner);
end;

procedure TCustomIBCDataSetUpdater.GetGeneratorVal;
var
  GenValue: variant;
  Connection: TGDSConnection;
begin
  Connection := TGDSConnection(TDBAccessUtils.GetIConnection(UsedConnection));
  if not((FDataSetService.FGeneratorMode = _gmPost) and
    Connection.IsFBServer and (Connection.GetMajorServerVersion >= 2) and
    (FDataSet.SQLInsert.Text = '') and //Pass through case when Gen value applied using GetInsertSQL
    ((GetUpdateObject = nil) or ((GetUpdateObject.InsertObject = nil) and (GetUpdateObject.InsertSQL.Text = '')))) and
    (FDataSetService.KeyGeneratorField is TNumericField) then begin
    GenValue := SelectDbValue('Get generator value',
      _Format('SELECT GEN_ID(%s, %d) FROM RDB$DATABASE',[FDataSetService.NormalizeStrValue(FDataSetService.FKeyGenerator), FDataSetService.FGeneratorStep]));
    FDataSetService.KeyGeneratorField.NewValue := GenValue;
  end;
end;

procedure TCustomIBCDataSetUpdater.SetUpdateParsedSQLType(SQLType: integer);
begin
  if FUpdateQuery <> nil then begin
    if FUpdateQuery is TCustomDADataSet then
      TDBAccessUtils.GetICommand(TCustomDADataSet(FUpdateQuery)).SetProp(prParsedSQLType, SQLType)
    else
    if FUpdateQuery is TCustomDASQL then
      TDBAccessUtils.GetICommand(TCustomDASQL(FUpdateQuery)).SetProp(prParsedSQLType, SQLType)
    else
      Assert(False, FUpdateQuery.ClassName);
  end;
end;

function TCustomIBCDataSetUpdater.NeedReturnParams: boolean;
begin
  Result := inherited NeedReturnParams or
    ((FDataSet.State = dsInsert) and (FDataSetService.IsDMLRefresh or (FDataSetService.IdentityField <> nil))); //Db_Key or Returning in insert statements
end;

function TCustomIBCDataSetUpdater.FieldByParamName(var ParamName: _string; var Old: boolean; var AFieldNo: integer): TField;
begin
  //Returning support
  if (CompareText(Copy(ParamName, 1, 4), 'RET_') = 0) and (FDataSet.FindField(ParamName) = nil) then
    ParamName := Copy(ParamName, 5, Length(ParamName) - 4);

  Result := inherited FieldByParamName(ParamName, Old, AFieldNo);
end;

procedure TCustomIBCDataSetUpdater.CheckUpdateQuery(const StatementType: TStatementType);
var
  UpdateDataSet: TCustomDADataSet;
begin
  inherited;

  UpdateDataSet := FUpdateQuery as TCustomDADataSet;
  if StatementType = stLock then
    TDBAccessUtils.SetAutoCommit(UpdateDataSet, False)
  else
    TDBAccessUtils.SetAutoCommit(UpdateDataSet, FDataSetService.IsAutoCommit);
  TDBAccessUtils.SetFetchAll(UpdateDataSet, True);
  UpdateDataSet.Options.QueryRecCount := False;
  UpdateDataSet.Options.QuoteNames := False;
  TDBAccessUtils.GetICommand(UpdateDataSet).SetProp(prQueryRowsAffected, FDataSet.Options.StrictUpdate); //for right RowsAffected behaviour
end;

procedure TCustomIBCDataSetUpdater.SetUpdateQueryOptions(const StatementType: TStatementType);
var
  DestRecordSet, SourceRecordSet: TCRRecordSet;
  DestCommand, SourceCommand: TCRCommand;

  procedure CopyPropR(Prop: integer);
  var
    v: variant;
  begin
    SourceRecordSet.GetProp(Prop, v);
    DestRecordSet.SetProp(Prop, v);
  end;

  procedure CopyPropC(Prop: integer);
  var
    v: variant;
  begin
    SourceCommand.GetProp(Prop, v);
    DestCommand.SetProp(Prop, v);
  end;

begin
  CheckIRecordSet; // can be inactive
  SourceRecordSet := GetIRecordSet;
  DestRecordSet := TDBAccessUtils.GetIRecordSet(FUpdateQuery as TCustomDADataSet);
  SourceCommand := GetICommand;
  DestCommand := TDBAccessUtils.GetICommand(FUpdateQuery as TCustomDADataSet);

  CopyPropR(prFieldsAsString);
  CopyPropC(prCacheBlobs);
  CopyPropC(prStreamedBlobs);
  CopyPropR(prComplexArrayFields);
  CopyPropC(prCacheArrays);
  CopyPropR(prDeferredArrayRead);
  CopyPropR(prBooleanDomainFields);
  CopyPropC(prUseDescribeParams);
end;

function TCustomIBCDataSetUpdater.IsNeedEditPreconnect: boolean;
begin
  Result := not FDataSet.CachedUpdates and (GetLockMode = lmPessimistic);
end;

function TCustomIBCDataSetUpdater.IsNeedInsertPreconnect: boolean;
begin
  Result := FDataSetService.FGeneratorMode = _gmInsert;
end;

function TCustomIBCDataSetUpdater.BatchUpdate: boolean;
begin
  Result := False;
end;

function TCustomIBCDataSetUpdater.UseParamType: boolean;  //This function indicates ParamType using in PerformSQL
begin
  Result := True;
end;

function TCustomIBCDataSetUpdater.GetDefaultExpressionValue(DefExpr: _string; var Value: variant): boolean;
begin
  Result := True;
  DefExpr := Trim(DefExpr);
  if (DefExpr = 'NOW') or
    (Pos('CURRENT_TIME', DefExpr) = 1) or (Pos('CURRENT_TIMESTAMP', DefExpr) = 1) then
    Value := Now
  else
  if (DefExpr = 'TODAY') or (DefExpr = 'CURRENT_DATE') then
    Value := Date
  else
  if (DefExpr = 'TOMORROW') then
    Value := Date + 1
  else
  if (DefExpr = 'YESTERDAY') then
    Value := Date - 1
  else
  if (DefExpr = IBDAC_EMPTY_STRING) then
    Value := ''
  else
    Result := inherited GetDefaultExpressionValue(DefExpr, Value);
end;

procedure TCustomIBCDataSetUpdater.PrepareAppend;
begin
  if (FDataSetService.KeyGeneratorField <> nil) and (FDataSetService.FGeneratorMode = _gmInsert) then
    GetGeneratorVal;
end;

function TCustomIBCDataSetUpdater.PerformAppend: boolean;
var
  OldReturnParams, OldStrictUpdate, Sequenced : boolean;
begin
  OldReturnParams := FDataSet.Options.ReturnParams;
  OldStrictUpdate := FDataSet.Options.StrictUpdate;
  Sequenced := (FDataSet.SQLInsert.Text = '') and
    ((GetUpdateObject = nil) or ((GetUpdateObject.InsertObject = nil) and (GetUpdateObject.InsertSQL.Text = ''))) and
    (FDataSetService.KeyGeneratorField <> nil) and
    TGDSConnection(TDBAccessUtils.GetIConnection(UsedConnection)).IsFBServer and
    (TGDSConnection(TDBAccessUtils.GetIConnection(UsedConnection)).GetMajorServerVersion >= 2);
  if Sequenced then begin
    FDataSet.Options.ReturnParams := True; //To retrieve Generator value
    FDataSet.Options.StrictUpdate := False; //This statement desribed as storedproc so RecordsAffected check will fails
  end;
  try
    if (FDataSetService.KeyGeneratorField <> nil) and (FDataSetService.FGeneratorMode = _gmPost) and not Sequenced then
      GetGeneratorVal;
      
    Result := inherited PerformAppend;
  finally
    if Sequenced then begin
      FDataSet.Options.ReturnParams := OldReturnParams;
      FDataSet.Options.StrictUpdate := OldStrictUpdate;
    end;
  end;
end;

function TCustomIBCDataSetUpdater.SavepointAllowed: boolean;
var
  Connection: TGDSConnection;
begin
  //There is some wrong Unlock behaviour
  //The waiting transaction would't be notified
  //after Rollback to Savepoint (see Fb 1.5 release notes)

  Connection := TGDSConnection(TDBAccessUtils.GetIConnection(UsedConnection));
  //FB 1.5 and IB 7.1
  if Connection.IsFBServer then
    Result := ((Connection.GetMajorServerVersion > 1) or
    ((Connection.GetMajorServerVersion = 1) and (Connection.GetMinorServerVersion >= 5)))
  else
    Result := ((Connection.GetMajorServerVersion > 7) or
    ((Connection.GetMajorServerVersion = 7) and (Connection.GetMinorServerVersion >= 1)));
end;

function TCustomIBCDataSetUpdater.CanRefreshByLock: boolean;
begin
  Result := False;
end;

{ TCustomIBCDataSetService }

constructor TCustomIBCDataSetService.Create(AOwner: TMemDataSet);
begin
  inherited Create(AOwner);

  FGeneratorStep := 1;
  FGeneratorMode := _gmPost;
end;

function TCustomIBCDataSetService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prKeyGenerator:
      FKeyGenerator := Value;
    prGeneratorMode:
      FGeneratorMode := _TGeneratorMode(Value);
    prGeneratorStep:
      FGeneratorStep := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TCustomIBCDataSetService.NormalizeStrValue(Value: _string): _string;
var
  Connection: TGDSConnection;
  SQLDialect: variant;
begin
  if UsedConnection <> nil then
    Connection := TGDSConnection(TDBAccessUtils.GetIConnection(UsedConnection))
  else
    Connection := nil;

  if Connection <> nil then
    Connection.GetProp(prSQLDialect, SQLDialect)
  else
    SQLDialect := 1;

  Result := Trim(Value);
  if SQLDialect = 3 then
    Result := IBCSQLInfo.NormalizeName(Value, FDataSet.Options.QuoteNames)
  else
    Result := _UpperCase(Result);
end;

procedure TCustomIBCDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomIBCDataSetUpdater.Create(Self));
end;

procedure TCustomIBCDataSetService.SetDataSetUpdater(Value: TDataSetUpdater);
begin
  inherited;

  FUpdater := TCustomIBCDataSetUpdater(Value);
end;

procedure TCustomIBCDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomIBCSQLGenerator.Create(Self));
end;

function TCustomIBCDataSetService.UsedConnection: TCustomDAConnection;
begin
  Result := inherited UsedConnection;
end;

function TCustomIBCDataSetService.IsDMLRefresh: boolean;
begin
  Result := inherited IsDMLRefresh;
end;

function TCustomIBCDataSetService.IsAutoCommit: boolean;
begin
  Result := inherited IsAutoCommit;
end;

procedure TCustomIBCDataSetService.SetAutoCommit(Value: boolean);
begin
  inherited SetAutoCommit(Value);
end;

function TCustomIBCDataSetService.DetectIdentityField: TCRFieldDesc;
{$IFNDEF FPC}
var
  i: integer;
  TableInfo: TCRTableInfo;
  FieldDesc: TCRFieldDesc;
  RecordSet: TGDSRecordSet;
{$ENDIF}
begin
  Result := nil;
{$IFNDEF FPC}
  TableInfo := UpdatingTableInfo;
  if TableInfo <> nil then begin
    RecordSet := TGDSRecordSet(GetIRecordSet);
    for i := 0 to RecordSet.FieldCount - 1 do begin
      FieldDesc := TCRFieldDesc(RecordSet.Fields[i]);
      if (FieldDesc.SubDataType = dtDbKey) and (FieldDesc.TableInfo = TableInfo) then begin
        Result := FieldDesc;
        Break;
      end;
    end;
  end;
{$ENDIF}
end;

function TCustomIBCDataSetService.DetectKeyGeneratorField: TField;
var
  FieldName: _string;
  Pos: integer;
begin
  if (Trim(FKeyGenerator) <> '') and (GetKeyFields <> '') then begin
    Pos := 1;
    FieldName := ExtractFieldName(GetKeyFields, Pos);
    Result := FDataSet.FindField(FieldName);
  end
  else
    Result := nil;
end;

function TCustomIBCDataSetService.DetectHiddenFields: TFieldArray;
var
  i: integer;
  FieldDesc: TCRFieldDesc;
  RecordSet: TGDSRecordSet;
  Field: TField;
begin
  Result := nil;
  RecordSet := TGDSRecordSet(GetIRecordSet);
  for i := 0 to RecordSet.FieldCount - 1 do begin
    FieldDesc := TCRFieldDesc(RecordSet.Fields[i]);
    if FieldDesc.SubDataType = dtDbKey then begin
      Field := FDataSet.GetField(FieldDesc);
      if Field <> nil then begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := Field;
      end;
    end;
  end;
end;

procedure TCustomIBCDataSetService.SetNumberRange(FieldDef: TFieldDef);
var
  Field: TField;
  FieldDesc: TFieldDesc;
  Precision: integer;
{$IFDEF VER6P}
  e: extended;
{$ENDIF}
begin
  Field := FDataSet.FindField(FieldDef.Name);
  if (Field <> nil) and (Field is TNumericField) then begin
    FieldDesc := FDataSet.GetFieldDesc(Field);
    case Field.DataType of
      ftInteger: begin
        TIntegerField(Field).MinValue := Low(Integer);
        TIntegerField(Field).MaxValue := High(Integer);
      end;
      ftSmallInt: begin
        TSmallIntField(Field).MinValue := Low(SmallInt);
        TSmallIntField(Field).MaxValue := High(SmallInt);
      end;
      ftLargeInt: begin
        TLargeIntField(Field).MinValue := Low(Int64);
        TLargeIntField(Field).MaxValue := High(Int64);
      end;
      ftBCD: begin
        Precision := FieldDesc.Length - FieldDesc.Scale;
        if Precision > 14 then
          Precision := 14; // max currency precision
        TBCDField(Field).MaxValue := IntPower(10, Precision) -
          IntPower(10, - Integer(FieldDesc.Scale));
        TBCDField(Field).MinValue := - TBCDField(Field).MaxValue;
      end;
    {$IFDEF VER6P}
    {$IFNDEF FPC}
      ftFmtBCD: begin
        e :=
          IntPower(10, FieldDesc.Length - FieldDesc.Scale) -
          IntPower(10, - Integer(FieldDesc.Scale));
        TFMTBCDField(Field).MaxValue := FloatToStr(e);
        TFMTBCDField(Field).MinValue := FloatToStr(- e);
      end;
    {$ENDIF}
    {$ENDIF}
      ftFloat: begin
        TFloatField(Field).MaxValue := 1.7E308;
        TFloatField(Field).MinValue := -1.7E308;
      end
    end;
  end;
end;

procedure TCustomIBCDataSetService.FillFieldsDefaultValues;

  procedure PrepareDefault(const Value: _string; Field: TField);
  var
    DefExpr: _string;
  begin
    DefExpr := Trim(Copy(Trim(Value), 8, MaxInt));
    if DefaultExpressionOldBehavior then begin
      if (DefExpr[1] = '''') and (DefExpr[Length(DefExpr)] = '''') then
        DefExpr := Copy(DefExpr, 2, Length(DefExpr) - 2);
      if (Field is TNumericField) and (DecimalSeparator <> '.') and
         (Pos('.', DefExpr) > 0) then
         DefExpr[Pos('.', DefExpr)] := _char(DecimalSeparator{$IFDEF CLR}[1]{$ENDIF});
      if DefExpr = '' then
        DefExpr := IBDAC_EMPTY_STRING; //To allow empty string in default expresion
    end;
    Field.DefaultExpression := DefExpr;
  end;

  function GetFieldValue(Field: TField): _string;
  begin
  {$IFDEF VER12P}
    if Field is TWideMemoField then
      Result := Field.AsString
    else
      Result := _string(Field.AsAnsiString);
  {$ELSE}
    Result := _VarToStr(Field.Value);
  {$ENDIF}
  end;

var
  Field: TField;
  MonitorClass: TDASQLMonitorClass;
  MessageID: cardinal;
  UpdateQuery: TCustomDADataSet;
begin
  if UpdatingTableInfoIdx = -1 then
    Exit;

  FUpdater.CheckUpdateQuery(stCustom);
  UpdateQuery := TCustomDADataSet(FUpdater.UpdateQuery);
  UpdateQuery.SQL.Text := 'SELECT Rel.RDB$FIELD_NAME, Fld.RDB$COMPUTED_BLR, Rel.RDB$DEFAULT_SOURCE, Fld.RDB$DEFAULT_SOURCE '+
    'FROM RDB$RELATION_FIELDS Rel JOIN RDB$FIELDS Fld ON (Rel.RDB$FIELD_SOURCE = Fld.RDB$FIELD_NAME) '+
    'WHERE (Rel.RDB$RELATION_NAME = ''' + GetTablesInfo[UpdatingTableInfoIdx].TableName + ''' ) AND (  '+
    '(NOT Fld.RDB$COMPUTED_BLR IS NULL)  OR '+
    '(NOT Rel.RDB$DEFAULT_SOURCE IS NULL) OR '+
    '(NOT Fld.RDB$DEFAULT_SOURCE IS NULL))';

  MonitorClass := TDASQLMonitorClass(TDBAccessUtils.SQLMonitorClass(UsedConnection));
  if MonitorClass.HasMonitor or FDataSet.Debug then
    MonitorClass.SQLExecute(FDataSet, UpdateQuery.SQL.Text, UpdateQuery.Params, 'Get Default Values', MessageID, True);

  UpdateQuery.Execute;
  while not UpdateQuery.Eof do begin
    Field := FDataSet.FindField(_VarToStr(UpdateQuery.Fields[0].Value));
    if Field <> nil then begin
      //1. Check Read Only
      if not UpdateQuery.Fields[1].IsNull then
        Field.ReadOnly := True
      else
      //2. Fill field's Default Expression
      //if DefaultValues then
        if not UpdateQuery.Fields[2].IsNull then
          PrepareDefault(GetFieldValue(UpdateQuery.Fields[2]), Field)
        else
          if not UpdateQuery.Fields[3].IsNull then
            PrepareDefault(GetFieldValue(UpdateQuery.Fields[3]), Field);
    end;
    UpdateQuery.Next;
  end;

  if MonitorClass.HasMonitor or FDataSet.Debug then
    MonitorClass.SQLExecute(FDataSet, UpdateQuery.SQL.Text, UpdateQuery.Params, 'Get Default Values', MessageID, False);
end;

function TCustomIBCDataSetService.GetRecCount: integer;

  function SelectNumberValue(const SQL: _string): integer;
  var
    i: integer;
    MonitorClass: TDASQLMonitorClass;
    MessageID: cardinal;
    UpdateQuery: TCustomDADataSet;
  begin
    FUpdater.CheckUpdateQuery(stQuery);
    UpdateQuery := TCustomDADataSet(FUpdater.UpdateQuery);
    UpdateQuery.SQL.Text := SQL;

    for i := 0 to UpdateQuery.Params.Count - 1 do
      UpdateQuery.Params[i].Assign(FDataSet.Params[i]);

    MonitorClass := TDASQLMonitorClass(TDBAccessUtils.SQLMonitorClass(UsedConnection));
    if MonitorClass.HasMonitor or FDataSet.Debug then
      MonitorClass.SQLExecute(FDataSet, SQL, UpdateQuery.Params, 'Get RecordCount', MessageID, True);

    UpdateQuery.Execute;

    Result := UpdateQuery.Fields[0].AsInteger;

    if MonitorClass.HasMonitor or FDataSet.Debug then
      MonitorClass.SQLExecute(FDataSet, SQL, UpdateQuery.Params, 'Get RecordCount', MessageID, False);
  end;

var
  Parser: TIBCParser;
  SelectPos, FromPos, BracketCount, Code: integer;
  s, Lexem: _string;
  Con: TGDSConnection;
begin
  Result := 0;
  s := FDataSet.FinalSQL;
  s := _SetOrderBy(s, '', TIBCParser);

  Con := TGDSConnection(TDBAccessUtils.GetIConnection(UsedConnection));
  Parser := TIBCParser.Create(s);
  Parser.OmitBlank := False;
  Parser.OmitComment := True;
  try
    if Parser.ToLexem(lxSELECT) <> lcEnd then begin
      SelectPos := Parser.CurrPos;

      BracketCount := 0;
      repeat
        Code := Parser.GetNext(Lexem);
        if Code = lcSymbol then begin
          if Lexem = '(' then
            Inc(BracketCount)
          else
            if Lexem = ')' then
              Dec(BracketCount);
        end;
      until ((Code = lxFROM) and (BracketCount = 0)) or (Code = lcEnd);

      if Code <> lcEnd then begin
        FromPos := Parser.CurrPos;
        if Con.IsFBServer and (Con.GetMajorServerVersion >= 2) then
          s := 'SELECT COUNT(*) FROM (' + LineSeparator +
            Copy(s, 1, SelectPos) + ' 1 AS C ' +
            Copy(s, FromPos - 4 {length('FROM')}, MaxInt) + LineSeparator + ')'
        else
          s := _Format('%s COUNT(*)%s', [Copy(s, 1, SelectPos), Copy(s, FromPos - 4 {length('FROM')}, MaxInt)]);
        Result := SelectNumberValue(s)
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function TCustomIBCDataSetService.DetectCanModify: boolean;
begin
  Result := (inherited DetectCanModify or
    not (FDataSet.ReadOnly or FDataSet.UniDirectional) and
    ((FDataSet.SQLInsert.Count > 0) or
    (FDataSet.SQLUpdate.Count > 0) or
    (FDataSet.SQLDelete.Count > 0) or
    FIsAnyFieldCanBeModified));
end;

function TCustomIBCDataSetService.PreventPSKeyFields(var PSKeyFields: string): boolean;
begin
  Result := not FDataSet.Active;
end;

function TCustomIBCDataSetService.NeedPreparePSExecuteCommand: boolean;
begin
  Result := True;
end;

function TCustomIBCDataSetService.GetDBKeyList(TableName: _string; IndexName: _string = ''): _string;
var
  MetaData: TDAMetaData;
  Info: TExtTableInfo;
begin
  BeginConnection; // GetCurrentSchema requires an active connection
  try
    TDBAccessUtils.GetSQLInfo(FDataSet).SplitObjectName(TableName, Info);
    if Info.Schema = '' then
      Info.Schema := GetCurrentSchema;

    MetaData := TDAMetaData.Create(nil);
    try
      MetaData.Connection := UsedConnection;
      TDBAccessUtils.SetTransaction(MetaData, TDBAccessUtils.UsedTransaction(FDataSet));
      MetaData.MetaDataKind := 'constraints';
      MetaData.Restrictions.Text := 'table_catalog=' + Info.Catalog +
        #13#10'table_schema=' + Info.Schema +
        #13#10'table_name=' + Info.Table +
        #13#10'constraint_type=primary key';
      MetaData.Open;
      if not MetaData.Eof then
        IndexName := _VarToStr(MetaData.FieldByName('INDEX_NAME').Value);
      MetaData.Close;
    finally
      MetaData.Free;
    end;

    Result := inherited GetDBKeyList(TableName, IndexName);
  finally
    EndConnection;
  end;
end;

{ TCustomIBCDataTypesMap }

const
{$IFDEF VER5}
  IBCDataTypeMap: array [TFieldType] of word = (
    dtUnknown, dtString, dtSmallint, dtInteger, dtInteger,
    dtBoolean, dtFloat, dtCurrency, dtBCD, dtDate, dtTime, dtDateTime,
    dtBytes, dtVarBytes, dtInteger, dtBlob, dtMemo, 0, 0,
    0, 0, 0, 0, dtFixedChar, dtWideString,
    dtInt64, dtArray, dtArray, 0, 0, 0, 0,
    0, 0, 0, 0);
{$ENDIF}
{$IFDEF VER6P}
  IBCDataTypeMap: array [TFieldType] of word = (
    dtUnknown, dtString, dtSmallint, dtInteger, dtInteger,
    dtBoolean, dtFloat, dtCurrency, dtBCD, dtDate, dtTime, dtDateTime,
    dtBytes, dtVarBytes, dtInteger, dtBlob, dtMemo, 0, 0,
    0, 0, 0, 0, dtFixedChar, dtWideString,
    dtInt64, dtArray, dtArray, 0, 0, 0, 0,
    0, 0, 0, 0, 0, {$IFNDEF FPC}dtFmtBCD{$ELSE}0{$ENDIF}
  {$IFDEF FPC}
    ,dtFixedWideChar, dtWideMemo
  {$ENDIF}
  {$IFDEF VER10P}
    ,dtFixedWideChar, dtWideMemo, 0, 0
  {$IFDEF VER12P}
    , 0, 0, 0, 0, 0, 0, 0
  {$ENDIF}
  {$IFDEF VER14P}
    ,0, 0, 0
  {$ENDIF}
  {$ENDIF}
  );
{$ENDIF}

class function TCustomIBCDataTypesMap.GetDataType(FieldType: TFieldType): integer;
begin
{$IFNDEF VER10P}
{$IFNDEF FPC}
  if Integer(FieldType) = Integer(ftFixedWideChar) then
    Result := dtFixedWideChar
  else
{$ENDIF}
{$ENDIF}
    Result := IBCDataTypeMap[FieldType];
end;

class function TCustomIBCDataTypesMap.GetFieldType(DataType: Word): TFieldType;
begin
  case DataType of
    dtFixedChar:
      Result := ftFixedChar;
    dtFixedWideChar:
      Result := TFieldType(ftFixedWideChar);
  else
    Result := inherited GetFieldType(DataType);
  end;
end;

{ TCustomIBCDumpProcessor }

function TCustomIBCDumpProcessor.GetFieldValueForDump(Field: TField): _string;
const
  BoolStrs: array [boolean] of _string = ('0', '1');
begin
  if not Field.IsNull and (Field.DataType = ftBoolean) then
    Result := BoolStrs[Field.AsBoolean]
  else
    Result := inherited GetFieldValueForDump(Field);
end;

end.
