
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I SQLiteDac.inc}
unit LiteServicesUni;
{$ENDIF}

interface

uses
{$IFNDEF CLR}
  CLRClasses,
{$ELSE}
  System.Text,
{$ENDIF}
  SysUtils, Classes, Variants, DB,
  MemUtils, MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  CRParser, CRAccess, DBAccess, DAScript, DADump,
{$IFNDEF UNIDACPRO}
  LiteCall, LiteClasses, LiteParser;
{$ELSE}
  LiteCallUni, LiteClassesUni, LiteParserUni;
{$ENDIF}

type

{ TCustomLiteSQLGenerator }

  TCustomLiteSQLGenerator = class(TDASQLGenerator)
  protected
    procedure GenerateInsertSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
  end;

{ TCustomLiteDataSetUpdater }

  TCustomLiteDataSetUpdater = class(TDADataSetUpdater)
  protected
    // CLR cross-assembly
    procedure CheckUpdateQuery(const StatementType: TStatementType); override;

    function BatchUpdate: boolean; override;
    procedure SetUpdateQueryOptions(const StatementType: TStatementType); override;
    function GetIdentityFieldValue(var Value: variant): boolean; override;
  end;

{ TCustomLiteDataSetService }

  TCustomLiteDataSetService = class(TDADataSetService)
  protected
    procedure CreateDataSetUpdater; override;
    procedure CreateSQLGenerator; override;

    function IdentityFieldIsData: boolean; override;
    function DetectCanModify: boolean; override;
    function DetectIdentityField: TCRFieldDesc; override;
    function ExtFieldsInfoIsInternal: boolean; override;
    procedure RequestFieldsInfo(Tables: TExtTablesInfo; Columns: TColumnsInfo); override;
    procedure InitUpdatingTableFields; override;
    function PreventPSKeyFields(var PSKeyFields: string): boolean; override;
    function GetRecCount: integer; override;

  public
    constructor Create(AOwner: TMemDataSet); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TCustomLiteDataTypesMap = class(TDataTypesMap)
  end;

  TLiteScriptProcessor = class (TDAScriptProcessor)
  protected
    function GetParserClass: TSQLParserClass; override;
  end;

  TCustomLiteDumpProcessor = class(TDADumpProcessor)
  protected
    function CreateQuery: TCustomDADataSet; override;
    function CreateDataQuery: TCustomDADataSet; override;
    function GetFieldValueForDump(Field: TField): _string; override;
  end;

  TLiteServerEnumerator = class (TCRServerEnumerator)
  end;

implementation

uses
  DAConsts, DASQLMonitor;

{ TCustomLiteSQLGenerator }

procedure TCustomLiteSQLGenerator.GenerateInsertSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean; const Index: integer);
begin
  inherited GenerateInsertSQL(KeyAndDataFields, ModifiedFieldsOnly, Index);

  if FFldSB.Length = 0 then begin
    Clear;
    inherited GenerateInsertSQL(KeyAndDataFields, False, Index);
  end;
end;

{ TCustomLiteDataSetUpdater }

procedure TCustomLiteDataSetUpdater.CheckUpdateQuery(const StatementType: TStatementType);
begin
  inherited;
end;

function TCustomLiteDataSetUpdater.BatchUpdate: boolean;
begin
  Result := False;
end;

procedure TCustomLiteDataSetUpdater.SetUpdateQueryOptions(const StatementType: TStatementType);
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

  //CopyPropR(prEnableLargeint);

  DestRecordSet.SetProp(prExtendedFieldsInfo, False);
end;

function TCustomLiteDataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
  Value := TDBAccessUtils.GetLastInsertId(FUpdateQuery as TCustomDADataSet);
  Result := True;
end;

{ TCustomLiteDataSetService }

constructor TCustomLiteDataSetService.Create(AOwner: TMemDataSet);
begin
  inherited;

end;

function TCustomLiteDataSetService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  {Result := True;
  case Prop of
  else}
    Result := inherited SetProp(Prop, Value);
  //end;
end;

procedure TCustomLiteDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomLiteDataSetUpdater.Create(Self));
end;

procedure TCustomLiteDataSetService.CreateSQLGenerator; 
begin
  SetSQLGenerator(TCustomLiteSQLGenerator.Create(Self));
end;

function TCustomLiteDataSetService.IdentityFieldIsData: boolean;
begin
  Result := True;
end;

function TCustomLiteDataSetService.DetectCanModify: boolean;
begin
  Result := inherited DetectCanModify or
    not FDataSet.ReadOnly and FIsAnyFieldCanBeModified;
end;

function TCustomLiteDataSetService.DetectIdentityField: TCRFieldDesc;
var
  i: integer;
  FieldDesc: TCRFieldDesc;
  RecordSet: TCRRecordSet;
begin
  Result := nil;
  //Search Identity Field
  RecordSet := GetIRecordSet;
  for i := 0 to RecordSet.FieldCount - 1 do begin
    FieldDesc := TCRFieldDesc(RecordSet.Fields[i]);
    if CompareText(FieldDesc.ActualName, 'rowid') = 0 then
      if TCRFieldDesc(FieldDesc).TableInfo = UpdatingTableInfo then begin
        Result := FieldDesc;
        break;
      end;
  end;
end;

function TCustomLiteDataSetService.ExtFieldsInfoIsInternal: boolean;
begin
  Result := IsMetaDataAPIAvailable;
end;

procedure TCustomLiteDataSetService.RequestFieldsInfo(Tables: TExtTablesInfo; Columns: TColumnsInfo);

var
  Restr: _TStringList;
  SQLiteMetaData: TSQLiteMetaData;
  MetaData: TData;
  i, p: integer;
  ColumnInfo, NewColumnInfo: CRAccess.TColumnInfo;
  Located: boolean;
  RecBuf: IntPtr;
  v: variant;

  function Locate1(FieldNo: integer; Value: _string): boolean;
  var
    v: variant;
  begin
    MetaData.SetToBegin;
    while True do begin
      MetaData.GetNextRecord(RecBuf);
      if MetaData.Eof then
        break;
      MetaData.GetFieldAsVariant(FieldNo, RecBuf, v);
      if _SameText(_VarToStr(v), Value) then begin
        Result := True;
        exit;
      end;
    end;
    Result := False;
  end;

begin

  for i := 0 to High(Tables) do
    with Tables[i] do begin
      if FDataSet.Options.FieldsOrigin or (i = FUpdatingTableInfoIdx) then
        Flag := 1
      else
        Flag := 0; // don't query fields for this table
    end;

  SQLiteMetaData := TSQLiteMetaData.Create;
  for i := 0 to High(Tables) do begin
    if Tables[i].Flag = 0 then
      continue;

    Restr := _TStringList.Create;
    try
      Restr.Add('TABLE_NAME=' + Tables[i].Table);
      MetaData := SQLiteMetaData.GetMetaData(GetIConnection, GetIConnection.GetInternalTransaction, 'columns', Restr);
    finally
      Restr.Free;
    end;

    MetaData.AllocRecBuf(RecBuf);
    try
      p := 0;
      while p < Columns.Count do begin
        ColumnInfo := Columns[p];
        if (ColumnInfo.TableIndex <> -1) and (ColumnInfo.TableIndex <> i) then begin
          Inc(p);
          continue;
        end;

        if ColumnInfo.Name = '*' then begin
          MetaData.SetToBegin;
          repeat
            MetaData.GetNextRecord(RecBuf);
            if MetaData.Eof then
              break;

            Inc(p);
            NewColumnInfo := CRAccess.TColumnInfo.Create;
            Columns.Insert(p, NewColumnInfo);
            NewColumnInfo.Table := ColumnInfo.Table;
            NewColumnInfo.TableIndex := i;

            MetaData.GetFieldAsVariant(4, RecBuf, v);
            NewColumnInfo.Name := _VarToStr(v);
            NewColumnInfo.Alias := NewColumnInfo.Name;

            if FDataSet.Options.DefaultValues and (i = FUpdatingTableInfoIdx) then begin
              MetaData.GetFieldAsVariant(11, RecBuf, v);
              NewColumnInfo.Expr := _VarToStr(v);
            end;
          until False;
        end
        else
        if (ColumnInfo.Name <> '') then begin
          if ColumnInfo.TableIndex <> -1 then begin
            if FDataSet.Options.DefaultValues and (i = FUpdatingTableInfoIdx) then
              Located := Locate1(4, ColumnInfo.Name)
            else
              Located := False;
          end
          else
            Located := Locate1(4, ColumnInfo.Name);

          if Located then begin
            if FDataSet.Options.DefaultValues then begin
              MetaData.GetFieldAsVariant(11, RecBuf, v);
              ColumnInfo.Expr := _VarToStr(v);
            end;

            ColumnInfo.TableIndex := i;
          end;
        end;

        inc(p);
      end;
      MetaData.Close;
    finally
      MetaData.FreeRecBuf(RecBuf);
      MetaData.Free;
    end;
  end;  
end;

procedure TCustomLiteDataSetService.InitUpdatingTableFields;
var
  TableName: _string;
  PointPos: integer;
  MetaData: TDAMetadata;
  Field: TField;
  FieldDesc: TFieldDesc;
  DefValue: _string;
  ExtFieldsInfo: boolean;
  v: variant;
begin
  inherited;

  GetIRecordSet.GetProp(prExtendedFieldsInfo, v);
  ExtFieldsInfo := v;

  if not (FDataSet.Options.DefaultValues or ExtFieldsInfo)
    or (UpdatingTableInfoIdx = -1)
  then
    Exit;

  TableName := GetTablesInfo[FUpdatingTableInfoIdx].TableName;
  PointPos := Pos('.', TableName);
  if PointPos > 0 then
    TableName := Copy(TableName, PointPos + 1, Length(TableName));
  MetaData := TDAMetadata.Create(nil);
  try
    MetaData.Connection := UsedConnection;
    MetaData.MetaDataKind := 'Columns';
    MetaData.Restrictions.Text := 'table_name=' + TableName;
    MetaData.Open;
    while not MetaData.EOF do begin
      Field := FDataSet.FindField(MetaData.FieldByName('COLUMN_NAME').AsString);
      if (Field <> nil) then begin
        if MetaData.FieldByName('NULLABLE').AsInteger = 0 then begin
          FieldDesc := FDataSet.GetFieldDesc(Field);
          FieldDesc.IsAutoIncrement := True;
        {$IFNDEF FPC}
          Field.AutoGenerateValue := arAutoInc;
        {$ENDIF}
          FIdentityField := TCRFieldDesc(FieldDesc);
        end;
        if FDataSet.Options.DefaultValues and
          not MetaData.FieldByName('DEFAULT_VALUE').IsNull
        then begin
          DefValue := MetaData.FieldByName('DEFAULT_VALUE').AsString;
          Field.DefaultExpression := DefValue;
        end;
      end;
      MetaData.Next;
    end;
  finally
    MetaData.Free;
  end;
end;

function TCustomLiteDataSetService.PreventPSKeyFields(var PSKeyFields: string): boolean;
begin
  Result := True;
end;

function TCustomLiteDataSetService.GetRecCount: integer;
var
  St: _string;
  UpdateQuery: TCustomDADataSet;
  MonitorClass: TDASQLMonitorClass;
  MessageID: cardinal;
begin
  Result := 0;
  St := FDataSet.FinalSQL;
  St := _SetOrderBy(St, '', TLiteParser);
  St := 'SELECT count(*) FROM (' + LineSeparator + St + LineSeparator + ')';

  TCustomLiteDataSetUpdater(FUpdater).CheckUpdateQuery(stCustom);
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

{ TLiteScriptProcessor }

function TLiteScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TLiteParser;
end;

{ TCustomLiteDumpProcessor }

function TCustomLiteDumpProcessor.CreateQuery: TCustomDADataSet;
begin
  Result := GetConnection.CreateDataSet;
  Result.ReadOnly := True;
  Result.UniDirectional := True;
  TDBAccessUtils.CheckConnection(Result);
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prExtendedFieldsInfo, False);
end;

function TCustomLiteDumpProcessor.CreateDataQuery: TCustomDADataSet;
begin
  Result := CreateQuery;
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prDumpData, True);
end;

function TCustomLiteDumpProcessor.GetFieldValueForDump(Field: TField): _string;
begin
  Result := Field.AsString;
end;

end.
