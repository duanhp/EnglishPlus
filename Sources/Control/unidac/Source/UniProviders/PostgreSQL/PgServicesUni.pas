
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgServicesUni;

{$ENDIF}

interface

uses
  DB, SysUtils, Classes, Variants,  
{$IFNDEF CLR}
  CLRClasses, 
{$ENDIF}
  CRAccess, MemUtils, MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  DBAccess, DADump,
{$IFNDEF UNIDACPRO}
  PgObjects, PgClasses;
{$ELSE}
  PgObjectsUni, PgClassesUni;
{$ENDIF}

const
  prAutoDeleteBlob = 101;
  prKeySequence    = 102;
  prSequenceMode   = 103;

type
  // must be sync with types in PgAccess
  _TSequenceMode = (_smInsert, _smPost);

  TCustomPgDataSetService = class;

  TCustomPgDataTypesMap = class(TDataTypesMap)
  public
    class function GetFieldType(DataType: Word): TFieldType; override;
    class function GetDataType(FieldType: TFieldType): integer; override;
  end;

  TPgParamInfo = class (TDAParamInfo)
  public
    SB: _StringBuilder;
    Position: integer;
    EndPosition: integer;
  end;

  TCustomPgSQLGenerator = class(TDASQLGenerator)
  protected
    FDataSetService: TCustomPgDataSetService;
    FKeyGeneratorFieldDesc: TFieldDesc;

    function GetParamInfoClass: TDAParamInfoClass; override;
    function IsBlobDataType(DataType: word): boolean; override;

    procedure AddParam(SB: _StringBuilder; FieldDesc: TFieldDesc;
      const StatementType: TStatementType;
      const ParamType: TParamType;
      Index: integer = -1;
      Old: boolean = False); override;

    function FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean; override;
    function FieldModified(FieldDesc: TCRFieldDesc; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean; override;

    procedure AddFieldToInsertSQL(FieldDesc: TCRFieldDesc; const Index: integer = -1); override;

    procedure GenerateInsertSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
    procedure GenerateUpdateSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
    procedure GenerateLockSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;
  public
    constructor Create(AOwner: TDADataSetService); override;  

    function GetParamOffset(const Index: integer): integer;
  end;

  TCustomPgDataSetUpdater = class(TDADataSetUpdater)
  private

    procedure GetSequenceNextVal;
  protected
    FDataSetService: TCustomPgDataSetService;

    function GetUpdateObject: TCustomDAUpdateSQL;
    procedure CheckUpdateQuery(const StatementType: TStatementType); override;

    function BatchUpdate: boolean; override;
    function UseParamType: boolean; override;
    function RetunParamsAsFields: boolean; override;

    procedure CheckUpdateSQL(const SQL: _string; const StatementTypes: TStatementTypes;
      out ParamsInfo: TDAParamsInfo; UseGenerator: boolean = True); override;
    procedure SetUpdateQueryOptions(const StatementType: TStatementType); override;

    procedure PrepareAppend; override;
    function PerformAppend: boolean; override;
  public
    constructor Create(AOwner: TDataSetService); override;

    function PerformSQL(const SQL: _string; const StatementTypes: TStatementTypes): boolean; override;
  end;

  TCustomPgDataSetService = class(TDADataSetService)
  private
    FAutoDeleteBlob: boolean;
    FKeySequence: _string;
    FSequenceMode: _TSequenceMode;

    function SequenceRequest(GenFullSelect: Boolean = False): _string;
    function SequenceRequestOnInsert: boolean;
    function SequecenRequestOnPost: boolean;
    function SequenceRequestUseReturning: boolean;

  protected
    FUpdater: TCustomPgDataSetUpdater;

    function GetIRecordSet: TCRRecordSet;
    function UsedConnection: TCustomDAConnection;

    procedure CreateSQLGenerator; override;
    procedure CreateDataSetUpdater; override;
    procedure SetDataSetUpdater(Value: TDataSetUpdater); override;

    procedure InitCursor; override;
    function DetectIdentityField: TCRFieldDesc; override;
    function DetectKeyGeneratorField: TField; override;
    function DetectCanModify: boolean; override;
    function PreventPSKeyFields(var PSKeyFields: string): boolean; override;
    function GetCurrentSchema: _string; override;
    function GetRecCount: integer; override;

    function IsInCacheProcessing: boolean;
    function IsDMLRefresh: boolean;
    function IsFullRefresh: boolean;
  public
    function OpenNext: boolean; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TPgServerEnumerator = class (TCRServerEnumerator)
  public
    procedure GetServerList(List: _TStrings); override;
  end;

  TCustomPgDumpProcessor = class(TDADumpProcessor)
  protected
    FCurSchema: _string;

    procedure CheckTables(const QueryText: _string); override;
    procedure BackupObjects(const QueryText: _string); override;
    function CreateQuery: TCustomDADataSet; override;
    procedure BackupData(const Schema, TableName, QueryText: _string; TableNum, TableCount: integer); reintroduce;

  public
    constructor Create(Owner: TDADump); override;
    destructor Destroy; override;

  end;

implementation

uses
  DAConsts, CRParser, DASQLMonitor,
{$IFNDEF UNIDACPRO}
  PgConsts, PgParser;
{$ELSE}
  PgConstsUni, PgParserUni;
{$ENDIF}

{ TCustomPgDataTypesMap }

class function TCustomPgDataTypesMap.GetFieldType(DataType: Word): TFieldType;
begin
  case DataType of
    dtPgLargeObject:
      Result := ftOraBlob;
  else
    Result := inherited GetFieldType(DataType);
  end;
end;

class function TCustomPgDataTypesMap.GetDataType(FieldType: TFieldType): integer;
begin
  case FieldType of
    ftOraBlob:
      Result := dtPgLargeObject;
  else
    Result := inherited GetDataType(FieldType);
  end;
end;

{ TCustomPgSQLGenerator }

constructor TCustomPgSQLGenerator.Create(AOwner: TDADataSetService);
begin
  inherited Create(AOwner);

  FDataSetService := TCustomPgDataSetService(AOwner);
end;

function TCustomPgSQLGenerator.GetParamOffset(const Index: integer): integer;
var
  ParamInfo: TPgParamInfo;
begin
  ParamInfo := TPgParamInfo(ParamsInfo.Items[Index]);
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
end;

function TCustomPgSQLGenerator.GetParamInfoClass: TDAParamInfoClass;
begin
  Result := TPgParamInfo;
end;

function TCustomPgSQLGenerator.IsBlobDataType(DataType: word): boolean;
begin
  Result := (DataType = dtPgLargeObject) or inherited IsBlobDataType(DataType);
end;

procedure TCustomPgSQLGenerator.AddParam(SB: _StringBuilder; FieldDesc: TFieldDesc;
      const StatementType: TStatementType;
      const ParamType: TParamType;
      Index: integer = -1;
      Old: boolean = False);
var
  ParamName: _string;
  ParamInfo: TPgParamInfo;
  Param: TDAParam;
  p: integer;
  CheckQuoteNeeded: boolean;
begin
  ParamName := FieldDesc.Name;
  CheckQuoteNeeded := True;

  if Old then
    ParamName := 'Old_' + ParamName;

  if Index > - 1 then
    ParamName := IndexedPrefix + IntToStr(Index) + '_' + ParamName;

  if FDataSet.Options.QuoteNames or
    CheckQuoteNeeded and SQLInfo.QuotesNeeded(FieldDesc.Name)
  then
    ParamName := SQLInfo.Quote(ParamName);

  p := SB.Length + 1;
  if (FParams = nil) or (((FDataSet.Params.Count > 0) or FDataSetService.IsFullRefresh) and (StatementType = stRefresh)) then begin
    SB.Append(':');
    SB.Append(ParamName);
  end
  else
    SB.Append('$' + IntToStr(FParams.Count + 1));

  if FParams <> nil then begin
    Param := TDAParam(FParams.Add);
    Param.ParamType := ParamType;
    Param.Name := ParamName;
    ParamInfo := TPgParamInfo(FParamsInfo.Add);
    ParamInfo.Field := FDataSet.GetField(FieldDesc);
    ParamInfo.Old := Old;
    ParamInfo.ParamIndex := Index;
    ParamInfo.Position := p;
    ParamInfo.EndPosition := SB.Length + 1;
    ParamInfo.SB := SB;
  end;
end;

function TCustomPgSQLGenerator.FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean;
var
  Obj: TSharedObject;
begin
  if FieldDesc = FKeyGeneratorFieldDesc then
    Result := False
  else begin
    Result := inherited FieldIsNull(FieldDesc, OldValue, Data, OldRecBuf, NewRecBuf);

    if Result and (FieldDesc.DataType = dtPgLargeObject) then begin
      if OldValue then
        Obj := Data.GetObject(FieldDesc.FieldNo, OldRecBuf)
      else
        Obj := Data.GetObject(FieldDesc.FieldNo, NewRecBuf);

      Result := TPgSQLLargeObject(Obj).OID = 0;
    end;
  end;  
end;

function TCustomPgSQLGenerator.FieldModified(FieldDesc: TCRFieldDesc; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean;
var
  Obj: TSharedObject;
begin
  if (FieldDesc.DataType = dtPgLargeObject) and
    not FDataSetService.IsInCacheProcessing
  then begin
    Obj := Data.GetObject(FieldDesc.FieldNo, NewRecBuf);

    Result := TPgObjectsUtils.GetLargeObjectOIDChanged(TPgSQLLargeObject(Obj));
  end
  else
    Result := False;

  Result := Result or inherited FieldModified(FieldDesc, Data, OldRecBuf, NewRecBuf);
end;

procedure TCustomPgSQLGenerator.AddFieldToInsertSQL(FieldDesc: TCRFieldDesc;
  const Index: integer = -1);
begin
  if FieldDesc = FKeyGeneratorFieldDesc then begin       //TODO
    if FFldSB.Length > 0 then begin
      FFldSB.Append(', ');
      FFldParamSB.Append(', ');
    end;
    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));

    FFldParamSB.Append(FDataSetService.SequenceRequest);

{    if FParams <> nil then begin
      Param := TDAParam(FParams.Add);
      Param.ParamType := ptOutput;
      Param.Name := FieldDesc.ActualName;
      ParamInfo := TDAParamInfo(FParamsInfo.Add);
      ParamInfo.Field := FDataSet.GetField(FieldDesc);
      ParamInfo.Old := False;
      ParamInfo.ParamIndex := Index;
    end;}
  end
  else
    inherited;
end;

procedure TCustomPgSQLGenerator.GenerateInsertSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  i: integer;
  ReturnSB: _StringBuilder;
  FieldDesc: TCRFieldDesc;
begin
  FKeyGeneratorFieldDesc := nil;

  if FDataSetService.SequenceRequestUseReturning then
    FKeyGeneratorFieldDesc := FDataSet.GetFieldDesc(FDataSetService.KeyGeneratorField);

  inherited;

  if FFldSB.Length = 0 then begin
    Clear;
    FHeaderSB.Append('INSERT INTO ');
    FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.Options.QuoteNames));
    FHeaderSB.Append(' DEFAULT VALUES');
  end;

  if FDataSetService.IsDMLRefresh or (FKeyGeneratorFieldDesc <> nil) then begin
    ReturnSB := _StringBuilder.Create(100);
    try
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if (FDataSetService.IsDMLRefresh and
          not (FieldDesc.DataType in [dtMemo, dtWideMemo, dtBlob, dtPgLargeObject])) or
          (FKeyGeneratorFieldDesc = FieldDesc)
        then begin
          if ReturnSB.Length > 0 then
            ReturnSB.Append(', ');
          ReturnSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
        end;
      end;

      if ReturnSB.Length > 0 then begin
        FFooterSB.Append(#$D#$A'RETURNING'#$D#$A'  ');
        FFooterSB.Append(ReturnSB);
      end;
    finally
      ReturnSB.Free;
    end;
  end;
end;

procedure TCustomPgSQLGenerator.GenerateUpdateSQL(const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean; const Index: integer = -1);
var
  i: integer;
  ReturnSB: _StringBuilder;
  FieldDesc: TCRFieldDesc;
begin
  inherited GenerateUpdateSQL(KeyAndDataFields, ModifiedFieldsOnly, Index);

  if FFldSB.Length = 0 then
    Exit;

  if FDataSetService.IsDMLRefresh then begin
    ReturnSB := _StringBuilder.Create(100);
    try
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if not (FieldDesc.DataType in [dtMemo, dtWideMemo, dtBlob, dtPgLargeObject])
        then begin
          if ReturnSB.Length > 0 then
            ReturnSB.Append(', ');
          ReturnSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
        end;
      end;

      if ReturnSB.Length <> 0 then begin
        FFooterSB.Append(#$D#$A'RETURNING'#$D#$A'  ');
        FFooterSB.Append(ReturnSB);
      end;
    finally
      ReturnSB.Free;
    end;
  end;
end;

procedure TCustomPgSQLGenerator.GenerateLockSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);
begin
  FHeaderSB.Append('SELECT * FROM ');
  FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.Options.QuoteNames));
  FMiddleSB.Append(#$D#$A'WHERE'#$D#$A'  ');
  GenerateConditions(FCondSB, stLock, False, KeyAndDataFields, Index);
  FFooterSB.Append(#$D#$A'FOR UPDATE NOWAIT');
end;

{ TCustomPgDataSetUpdater }

constructor TCustomPgDataSetUpdater.Create(AOwner: TDataSetService);
begin
  FDataSetService := TCustomPgDataSetService(AOwner);

  inherited Create(AOwner);
end;

function TCustomPgDataSetUpdater.GetUpdateObject: TCustomDAUpdateSQL;
begin
  Result := inherited GetUpdateObject;
end;

procedure TCustomPgDataSetUpdater.CheckUpdateQuery(const StatementType: TStatementType);
begin
  inherited;
end;

function TCustomPgDataSetUpdater.PerformSQL(const SQL: _string; const StatementTypes: TStatementTypes): boolean;
var
  i: integer;
  FieldDesc: TFieldDesc;
  IsDelete: boolean;
  lo: TPgSQLLargeObject;
begin
  Result := inherited PerformSQL(SQL, StatementTypes);

  IsDelete := stDelete in StatementTypes;

  if IsDelete and FDataSetService.FAutoDeleteBlob then
    for i := 0 to FDataSet.Fields.Count - 1 do begin
      FieldDesc := FDataSet.GetFieldDesc(FDataSet.Fields[i]);
      if FieldDesc.DataType = dtPgLargeObject then begin
        lo := TPgSQLLargeObject(GetIRecordSet.GetObject(FieldDesc.FieldNo, FDataSet.ActiveBuffer));
        if lo.IsCreated then
          lo.UnlinkObject;
      end;
    end;
end;

function TCustomPgDataSetUpdater.BatchUpdate: boolean;
begin
  Result := False;
end;

function TCustomPgDataSetUpdater.UseParamType: boolean;
begin
  Result := True;
end;

function TCustomPgDataSetUpdater.RetunParamsAsFields: boolean;
begin
  Result := True;
end;

procedure TCustomPgDataSetUpdater.CheckUpdateSQL(const SQL: _string;
  const StatementTypes: TStatementTypes; out ParamsInfo: TDAParamsInfo; UseGenerator: boolean = True);
var
  Offset, i: integer;
  ICommand: TPgSQLCommand;
  ParamInfo: TPgParamInfo;
begin
  inherited;

  if ParamsInfo = nil then
    Exit;

  if not (((FDataSet.Params.Count > 0) or FDataSetService.IsFullRefresh) and (stRefresh in StatementTypes)) then begin
    if IsClass(FUpdateQuery, TCustomDADataSet) then
      ICommand := TPgSQLCommand(TDBAccessUtils.GetICommand(TCustomDADataSet(FUpdateQuery)))
    else
    if IsClass(FUpdateQuery, TCustomDASQL) then
      ICommand := TPgSQLCommand(TDBAccessUtils.GetICommand(TCustomDASQL(FUpdateQuery)))
    else begin
      ICommand := nil;
      Assert(False);
    end;

    ICommand.ClearPlaceHolders;
    for i := 0 to ParamsInfo.Count - 1 do begin
      ParamInfo := TPgParamInfo(ParamsInfo.Items[i]);
      Offset := TCustomPgSQLGenerator(FDataSetService.SQLGenerator).GetParamOffset(i);
      ICommand.AddPlaceHolder(Offset + ParamInfo.Position, Offset + ParamInfo.EndPosition, nil);
    end;
  end;
end;

procedure TCustomPgDataSetUpdater.SetUpdateQueryOptions(const StatementType: TStatementType);
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
  CheckIRecordSet; // can be inactive
  SourceRecordSet := GetIRecordSet;
  DestRecordSet := TDBAccessUtils.GetIRecordSet(FUpdateQuery as TCustomDADataSet);

  CopyPropR(prOIDAsInt);
  CopyPropR(prCacheBlobs);
  CopyPropR(prCommandTimeOut);
  CopyPropR(prUnknownAsString);
  CopyPropR(prSimpleQueryExecute);
  CopyPropR(prUseParamTypes);

  DestRecordSet.SetProp(prExtendedFieldsInfo, False);
end;

procedure TCustomPgDataSetUpdater.PrepareAppend;
begin
  if FDataSetService.SequenceRequestOnInsert then
    GetSequenceNextVal;
end;

function TCustomPgDataSetUpdater.PerformAppend: boolean;
var
  OldReturnParams: boolean;

  RequestSeq: boolean;
  RequestSeqInRet: boolean;

begin
  OldReturnParams := FDataSet.Options.ReturnParams;

  RequestSeq := FDataSetService.SequecenRequestOnPost;
  RequestSeqInRet := FDataSetService.SequenceRequestUseReturning;

  if RequestSeqInRet then
    FDataSet.Options.ReturnParams := True; //To retrieve sequence value

  try
    if RequestSeq and not RequestSeqInRet then
      GetSequenceNextVal;
      
    Result := inherited PerformAppend;
  finally
    if RequestSeqInRet then
      FDataSet.Options.ReturnParams := OldReturnParams;
  end;
end;

procedure TCustomPgDataSetUpdater.GetSequenceNextVal;
begin
  Assert(FDataSetService.KeyGeneratorField <> nil);
  FDataSetService.KeyGeneratorField.NewValue := SelectDBValue('Get sequence value',
    FDataSetService.SequenceRequest(True));
end;

{ TCustomPgDataSetService }

function TCustomPgDataSetService.OpenNext: boolean;
var
  Cursor: TPgCursor;
begin
  if not FDataSet.Active then begin
    FDataSet.Open;
    Result := True;
  end
  else begin
    Cursor := TPgSQLCommand(GetIRecordSet.GetCommand).GetNextCursor;
    if Cursor <> nil then begin
      FDataSet.Close;
      TDBAccessUtils.SetCursor(FDataSet, Cursor);
      FDataSet.Open;
      Result := True;
    end
    else
      Result := False;
  end;    
end;

function TCustomPgDataSetService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoDeleteBlob:
      FAutoDeleteBlob := Value;
    prKeySequence:
      FKeySequence := Value;
    prSequenceMode:
      FSequenceMode := _TSequenceMode(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TCustomPgDataSetService.GetIRecordSet: TCRRecordSet;
begin
  Result := inherited GetIRecordSet;
end;

function TCustomPgDataSetService.UsedConnection: TCustomDAConnection;
begin
  Result := inherited UsedConnection;
end;

procedure TCustomPgDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomPgSQLGenerator.Create(Self));
end;

procedure TCustomPgDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomPgDataSetUpdater.Create(Self));
end;

procedure TCustomPgDataSetService.SetDataSetUpdater(Value: TDataSetUpdater);
begin
  inherited;

  FUpdater := TCustomPgDataSetUpdater(Value);
end;

procedure TCustomPgDataSetService.InitCursor;
{$IFNDEF FPC}
var
  i: integer;
  RecordSet: TPgSQLRecordSet;
  FieldDesc: TPgSQLFieldDesc;
  Field: TField;
{$ENDIF}
begin
  inherited;

{$IFNDEF FPC}
  RecordSet := TPgSQLRecordSet(GetIRecordSet);
  for i := 0 to RecordSet.FieldCount - 1 do begin
    FieldDesc := TPgSQLFieldDesc(RecordSet.Fields[i]);
    if FieldDesc.IsAutoIncrement then begin
      Field := FDataSet.GetField(FieldDesc);
      if Field <> nil then
        Field.AutoGenerateValue := arAutoInc;
    end;
  end;
{$ENDIF}
end;

function TCustomPgDataSetService.DetectIdentityField: TCRFieldDesc;
var
  i: integer;
  FieldDesc: TPgSQLFieldDesc;
  RecordSet: TPgSQLRecordSet;
begin
  Result := nil;
  //Search Identity Field
  RecordSet := TPgSQLRecordSet(GetIRecordSet);
  for i := 0 to RecordSet.FieldCount - 1 do begin
    FieldDesc := TPgSQLFieldDesc(RecordSet.Fields[i]);
    if (CompareText(FieldDesc.ActualName, 'OID') = 0) and (FieldDesc.TableCol <= 0) then
      if FieldDesc.TableInfo = UpdatingTableInfo then begin
        Result := FieldDesc;
        break;
      end;
  end;
end;

function TCustomPgDataSetService.DetectKeyGeneratorField: TField;
var
  FieldName: _string;
  Pos: integer;
begin
  if (Trim(FKeySequence) <> '') and (GetKeyFields <> '') then begin
    Pos := 1;
    FieldName := ExtractFieldName(GetKeyFields, Pos);
    Result := FDataSet.FindField(FieldName);
  end
  else
    Result := nil;
end;

function TCustomPgDataSetService.DetectCanModify: boolean;
begin
  Result := inherited DetectCanModify or
    not FDataSet.ReadOnly and FIsAnyFieldCanBeModified;
end;

function TCustomPgDataSetService.PreventPSKeyFields(var PSKeyFields: string): boolean;
begin
  Result := True;
end;

function TCustomPgDataSetService.GetCurrentSchema: _string;
begin
  Result := TPgSQLConnection(TDBAccessUtils.GetIConnection(UsedConnection)).GetCachedSchema;
  // to preserve character case
  Result := '"' + Result + '"';
end;

function TCustomPgDataSetService.GetRecCount: integer;
var
  St: _string;
  UpdateQuery: TCustomDADataSet;
  MonitorClass: TDASQLMonitorClass;
  MessageID: cardinal;
  Parser: TPgParser;
begin
  Result := 0;
  St := FDataSet.FinalSQL;
  St := _SetOrderBy(St, '', TPgParser);
  Parser := TPgParser.Create(St);
  try
    if Parser.ToLexem(lxFOR) <> lcEnd then begin
      St := copy(St, 1, Parser.CurrPos - 3);
    end;
  finally
    Parser.Free;
  end;
  St := 'SELECT count(*) FROM (' + LineSeparator + St + LineSeparator + ') t';

  FUpdater.CheckUpdateQuery(stCustom);
  UpdateQuery := TCustomDADataSet(FUpdater.UpdateQuery);
  UpdateQuery.SQL.Text := St;
  UpdateQuery.Params.Assign(FDataSet.Params);

  MonitorClass := TDASQLMonitorClass(TDBAccessUtils.SQLMonitorClass(UsedConnection));
  if MonitorClass.HasMonitor or FDataSet.Debug then
    MonitorClass.SQLExecute(FDataSet, St, UpdateQuery.Params, 'Get RecordCount', MessageID, True);

  UpdateQuery.Open;
  if not UpdateQuery.EOF then
    Result := UpdateQuery.Fields[0].AsInteger;

  if MonitorClass.HasMonitor or FDataSet.Debug then
    MonitorClass.SQLExecute(FDataSet, St, UpdateQuery.Params, 'Get RecordCount', MessageID, False);
end;

function TCustomPgDataSetService.IsInCacheProcessing: boolean;
begin
  Result := inherited IsInCacheProcessing;
end;

function TCustomPgDataSetService.IsDMLRefresh: boolean;
begin
  Result := inherited IsDMLRefresh;
end;

function TCustomPgDataSetService.IsFullRefresh: boolean;
begin
  Result := inherited IsFullRefresh;
end;

function TCustomPgDataSetService.SequenceRequest(GenFullSelect: Boolean = False): _string;
begin
  if GenFullSelect then
    Result := _Format('SELECT NEXTVAL(%s)', [PgSQLInfo.ToStringConst(FKeySequence)])
  else
    Result := _Format('NEXTVAL(%s)', [PgSQLInfo.ToStringConst(FKeySequence)]);
end;

function TCustomPgDataSetService.SequenceRequestOnInsert: boolean;
begin
  Result := (KeyGeneratorField <> nil) and (FSequenceMode = _smInsert);
end;

function TCustomPgDataSetService.SequecenRequestOnPost: boolean;
begin
  Result := (KeyGeneratorField <> nil) and (FSequenceMode = _smPost);
end;

function TCustomPgDataSetService.SequenceRequestUseReturning: boolean;
begin
  Result :=
    SequecenRequestOnPost and  
    (FDataSet.SQLInsert.Text = '') and
    ((FUpdater.GetUpdateObject = nil) or
    ((FUpdater.GetUpdateObject.InsertObject = nil) and
    (FUpdater.GetUpdateObject.InsertSQL.Text = ''))) and
    TPgSQLConnection(TDBAccessUtils.GetIConnection(UsedConnection)).VersionIsEqualOrHigher(8, 2); 
end;

{ TPgServerEnumerator }

procedure TPgServerEnumerator.GetServerList(List: _TStrings);
{$IFDEF MSWINDOWS}
var
  List1: TStringList;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  List1 := TStringList.Create;
  try
    CRNetManager.GetServerList(List1, 'postgresql');
    AssignStrings(List1, List);
  finally
    List1.Free;
  end;
{$ENDIF}
end;

{ TCustomPgDumpProcessor }

constructor TCustomPgDumpProcessor.Create(Owner: TDADump);
begin
  inherited Create(Owner);
end;

destructor TCustomPgDumpProcessor.Destroy;
begin
  inherited;
end;

procedure TCustomPgDumpProcessor.CheckTables(const QueryText: _string);
var
  TablesInfo: TCRTablesInfo;
begin
  if QueryText <> '' then begin
    CheckQuery;
    GetTables.Clear;
    TablesInfo := TCRTablesInfo.Create(TCRTableInfo);
    try
      SQLInfo.ParseTablesInfo(QueryText, TablesInfo);
      if TablesInfo.Count <> 1 then
        raise Exception.Create(SBackupQueryWrongTableCount);
      GetTables.Add(TablesInfo[0].TableName);
    finally
      TablesInfo.Free;
    end;
  end
  else
    inherited;
end;

procedure TCustomPgDumpProcessor.BackupObjects(const QueryText: _string);
var
  i, p: integer;
  TableName, Schema: _string;
  TablesList: _TStringList;
  ExactNames: boolean;
begin
  CheckQuery;
  TablesList := _TStringList.Create;
  try
    if GetTables.Count = 0 then begin
      GetConnection.GetTableNames(TablesList);
      ExactNames := True;
    end
    else begin
      TablesList.Assign(GetTables);
      ExactNames := False;
    end;

    FCurSchema := '';
    for i := 0 to TablesList.Count - 1 do begin
      TableName := TablesList[i];
      p := Pos('.', TableName);
      if p > 0 then begin
        Schema := SQLInfo.NormalizeName(Copy(TableName, 1, p - 1), FOwner.Options.QuoteNames);
        TableName := Copy(TableName, p + 1, MaxInt);
      end
      else begin
        Schema := TPgSQLConnection(TDBAccessUtils.GetIConnection(GetConnection)).GetCachedSchema;
        Schema := QuoteName(Schema);
      end;

      if ExactNames then
        TableName := QuoteName(TableName)
      else
        TableName := SQLInfo.NormalizeName(TableName, FOwner.Options.QuoteNames);

      if FCurSchema <> Schema then begin
        FCurSchema := Schema;
        Add('');
        Add(_Format('SET search_path = %s, pg_catalog;', [Schema]));
      end;

      BackupData(Schema, TableName, QueryText, i + 1, TablesList.Count);
    end;
  finally
    TablesList.Free;
  end;
end;

function TCustomPgDumpProcessor.CreateQuery: TCustomDADataSet;
begin
  Result := GetConnection.CreateDataSet;
  Result.ReadOnly := True;
  Result.UniDirectional := True;
  TDBAccessUtils.CheckConnection(Result);
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prExtendedFieldsInfo, False);
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prOIDAsInt, True);
end;

procedure TCustomPgDumpProcessor.BackupData(const Schema, TableName, QueryText: _string;
  TableNum, TableCount: integer);
var
  Query: TCustomDADataSet;
  i: integer;
  FieldList, Sql, Header: _string;
begin
  Query := CreateQuery;
  try
    //TDBAccessUtils.CheckConnection(Query);
    TDBAccessUtils.GetIRecordSet(Query).SetProp(prFieldsAsText, True);

    if QueryText <> '' then
      Query.SQL.Text := QueryText
    else
      Query.SQL.Text := 'SELECT * FROM ' + Schema + '.' + TableName;

    if Assigned(FOwner.OnBackupProgress) then
      Query.Options.QueryRecCount := true
    else
      Query.Options.QueryRecCount := false;

    Query.Open;

    Add('');
    if FOwner.Options.AddDrop then
      Add('DELETE FROM ' + TableName + ';');

    if Query.RecordCount = 0 then
      exit;

    Header := 'INSERT INTO ' + TableName;
    if FOwner.Options.CompleteInsert then begin
      FieldList := '';
      for i := 0 to Query.Fields.Count - 1 do begin
        if i > 0 then
          FieldList := FieldList + ', ';
        FieldList := FieldList + QuoteName(Query.Fields[i].FieldName);
      end;
      Header := Header + '(' + FieldList + ')';
    end;
    Header := Header + ' VALUES (';

    while not Query.Eof do begin
      Sql := '';
      for i := 0 to Query.Fields.Count - 1 do begin
        if i > 0 then
          Sql := Sql + ', ';
        if Query.Fields[i].IsNull then
          Sql := Sql + 'NULL'
        else begin
          Sql := Sql + _string(TPgTextConverter.ValueToText(Query.Fields[i].Value, dtString, False));
        end;
      end;
      Sql := Header + Sql + ');';
      Add(Sql);

      DoBackupProgress(TableName, TableNum, TableCount, Trunc((Query.RecNo / Query.RecordCount) * 100));

      Query.Next;
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;

end.
