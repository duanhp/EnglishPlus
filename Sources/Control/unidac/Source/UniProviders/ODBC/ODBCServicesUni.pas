
//////////////////////////////////////////////////
//  ODBC Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ODBCDac.inc}
unit ODBCServicesUni;
{$ENDIF}

interface

uses
{$IFNDEF CLR}
  CLRClasses,
{$ELSE}
  System.Text, System.Runtime.InteropServices,
{$ENDIF}
  SysUtils, Classes, Variants, DB,
  MemUtils, MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  CRParser, CRAccess, DBAccess, DAScript, DADump,
{$IFNDEF UNIDACPRO}
  ODBCClasses, ODBCParser, ODBCCall;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCCallUni;
{$ENDIF}

type

{ TCustomODBCSQLGenerator }

  TCustomODBCSQLGenerator = class(TDASQLGenerator)
  protected
    procedure GenerateLockSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;
    procedure GenerateInsertSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
  end;

{ TCustomODBCDataSetUpdater }

  TCustomODBCDataSetUpdater = class(TDADataSetUpdater)
  protected
    // CLR cross-assembly
    procedure CheckUpdateQuery(const StatementType: TStatementType); override;

    function SavepointAllowed: boolean; override;
    function CanRefreshByLock: boolean; override;
    function BatchUpdate: boolean; override;
    procedure SetUpdateQueryOptions(const StatementType: TStatementType); override;
  end;

{ TCustomODBCDataSetService }

  TCustomODBCDataSetService = class(TDADataSetService)
  protected
    procedure CreateDataSetUpdater; override;
    procedure CreateSQLGenerator; override;
    function KeysFromSpecialColumns: boolean; virtual;
    function DetectCanModify: boolean; override;
    function ExtFieldsInfoIsInternal: boolean; override;
    function GetMetaDataRecordSet: TCRRecordSet; virtual;
    procedure RequestFieldsInfo(Tables: TExtTablesInfo; Columns: TColumnsInfo); override;
    function PreventPSKeyFields(var PSKeyFields: string): boolean; override;
    function GetRecCount: integer; override;

  public
    constructor Create(AOwner: TMemDataSet); override;
    function GetDBKeyList(TableName: _string; IndexName: _string = ''): _string; override;
  end;

  TCustomODBCDataTypesMap = class(TDataTypesMap)
  end;

  TODBCScriptProcessor = class(TDAScriptProcessor)
  protected
    function GetParserClass: TSQLParserClass; override;
  end;

  TCustomODBCDumpProcessor = class(TDADumpProcessor)
  protected
    function CreateQuery: TCustomDADataSet; override;
  end;

  TODBCServerEnumerator = class (TCRServerEnumerator)
  public
    procedure GetServerList(List: _TStrings); override;
  end;

implementation

uses
  DAConsts, DASQLMonitor;

{ TCustomODBCSQLGenerator }

procedure TCustomODBCSQLGenerator.GenerateLockSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);

var
  FieldDesc: TCRFieldDesc;

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

procedure TCustomODBCSQLGenerator.GenerateInsertSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1);
begin
  inherited GenerateInsertSQL(KeyAndDataFields, ModifiedFieldsOnly, Index);

  if FFldSB.Length = 0 then begin
    Clear;
    inherited GenerateInsertSQL(KeyAndDataFields, False, Index);
  end;
end;

{ TCustomODBCDataSetUpdater }

procedure TCustomODBCDataSetUpdater.CheckUpdateQuery(const StatementType: TStatementType);
begin
  inherited;
end;

function TCustomODBCDataSetUpdater.BatchUpdate: boolean;
begin
  Result := False;
end;

function TCustomODBCDataSetUpdater.SavepointAllowed: boolean;
begin
  Result := False;
end;

function TCustomODBCDataSetUpdater.CanRefreshByLock: boolean;
begin
  Result := False;
end;

procedure TCustomODBCDataSetUpdater.SetUpdateQueryOptions(const StatementType: TStatementType);
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

  CopyPropR(prCommandTimeout);

  DestRecordSet.SetProp(prExtendedFieldsInfo, False);
end;

{ TCustomODBCDataSetService }

constructor TCustomODBCDataSetService.Create(AOwner: TMemDataSet);
begin
  inherited;
end;

function TCustomODBCDataSetService.GetDBKeyList(TableName: _string; IndexName: _string = ''): _string;
var
  RecordSet: TODBCMetaDataRecordSet;
  RecBuf: IntPtr;
  Info: TExtTableInfo;
  SQLInfo: TSQLInfo;
  ICon: TODBCConnection;
  v: variant;
begin
  if not KeysFromSpecialColumns then begin
    Result := inherited GetDBKeyList(TableName);
    exit;
  end;

  BeginConnection;
  try
    ICon := TODBCConnection(TDBAccessUtils.GetIConnection(UsedConnection));
    SQLInfo := GetICommand.SQLInfo;

    SQLInfo.SplitObjectName(TableName, Info);
    Info.Table := SQLInfo.NormalizeName(Info.Table, False, True);
    Info.Schema := SQLInfo.NormalizeName(Info.Schema, False, True);
    if Info.Schema = '' then
      Info.Schema := ICon.GetCachedSchema;
    Info.Catalog := SQLInfo.NormalizeName(Info.Catalog, False, True);
    if Info.Catalog = '' then
      Info.Catalog := ICon.GetCachedCatalog;

    RecordSet := TODBCMetaDataRecordSet.Create;
    try
      RecordSet.SetConnection(ICon);

      with TODBCMetaDataCommand(RecordSet.GetCommand) do begin
        MetaDataKind := mkSpecialColumns;
        with MetaDataArgs do begin
          CatalogName := Info.Catalog;
          SchemaName := Info.Schema;
          ObjectName := Info.Table;
          Param1 := SQL_BEST_ROWID;
          Param2 := SQL_SCOPE_SESSION;
          Param3 := SQL_NULLABLE;
        end;
      end;

      RecordSet.Open;
      RecordSet.AllocRecBuf(RecBuf);
      try
        repeat
          RecordSet.GetNextRecord(RecBuf);
          if RecordSet.Eof then
            break;

          if Result <> '' then
            Result := Result + ';';
          RecordSet.GetFieldAsVariant(2, RecBuf, v);
          Result := Result + _VarToStr(v);
        until False;
      finally
        RecordSet.FreeRecBuf(RecBuf);
      end;
      RecordSet.Close;
    finally
      RecordSet.Free;
    end;
  finally
    EndConnection;
  end;
end;

procedure TCustomODBCDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomODBCDataSetUpdater.Create(Self));
end;

procedure TCustomODBCDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomODBCSQLGenerator.Create(Self));
end;

function TCustomODBCDataSetService.KeysFromSpecialColumns: boolean;
begin
  Result := False;
end;

function TCustomODBCDataSetService.DetectCanModify: boolean;
begin
  Result := inherited DetectCanModify or
    not FDataSet.ReadOnly and FIsAnyFieldCanBeModified;
end;

function TCustomODBCDataSetService.ExtFieldsInfoIsInternal: boolean;
begin
  Result := False;
end;

function TCustomODBCDataSetService.GetMetaDataRecordSet: TCRRecordSet;
begin
  Result := TODBCMetaDataRecordSet.Create;
  Result.SetConnection(TDBAccessUtils.GetIConnection(UsedConnection));
end;

procedure TCustomODBCDataSetService.RequestFieldsInfo(Tables: TExtTablesInfo; Columns: TColumnsInfo);
var
  i, p: integer;
  ColumnInfo, NewColumnInfo: TColumnInfo;
  Located: boolean;
  RecordSet: TCRRecordSet;
  RecBuf: IntPtr;
  v: variant;
  SQLInfo: TSQLInfo;
  IdentCase: TIdentCase;

  function Locate1(FieldNo: integer; Value: _string): boolean;
  var
    v: variant;
  begin
    RecordSet.SetToBegin;
    while True do begin
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        break;
      RecordSet.GetFieldAsVariant(FieldNo, RecBuf, v);
      if (IdentCase <> icMixed) and (_VarToStr(v) = Value) or
        (IdentCase = icMixed) and _SameText(_VarToStr(v), Value)
      then begin
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

  SQLInfo := GetICommand.SQLInfo;
  IdentCase := SQLInfo.IdentCase;
  RecordSet := GetMetaDataRecordSet;
  try
    TODBCMetaDataCommand(RecordSet.GetCommand).MetaDataKind := mkColumns;

    for i := 0 to High(Tables) do begin
      if Tables[i].Flag = 0 then
        continue;

      with TODBCMetaDataCommand(RecordSet.GetCommand).MetaDataArgs do begin
        CatalogName := Tables[i].Catalog;
        SchemaName := Tables[i].Schema;
        ObjectName := Tables[i].Table;
        ColumnName := '%';
      end;

      RecordSet.Open;
      RecordSet.AllocRecBuf(RecBuf);
      try
        p := 0;
        while p < Columns.Count do begin
          ColumnInfo := Columns[p];
          if (ColumnInfo.TableIndex <> -1) and (ColumnInfo.TableIndex <> i) then begin
            Inc(p);
            continue;
          end;

          if ColumnInfo.Name = '*' then begin
            RecordSet.SetToBegin;
            repeat
              RecordSet.GetNextRecord(RecBuf);
              if RecordSet.Eof then
                break;

              Inc(p);
              NewColumnInfo := TColumnInfo.Create;
              Columns.Insert(p, NewColumnInfo);
              NewColumnInfo.Table := ColumnInfo.Table;
              NewColumnInfo.TableIndex := i;

              RecordSet.GetFieldAsVariant(4, RecBuf, v);
              NewColumnInfo.Name := _VarToStr(v);
              NewColumnInfo.Alias := NewColumnInfo.Name;

              if FDataSet.Options.DefaultValues and (i = FUpdatingTableInfoIdx) then begin
                RecordSet.GetFieldAsVariant(13, RecBuf, v);
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
                RecordSet.GetFieldAsVariant(13, RecBuf, v);
                ColumnInfo.Expr := _VarToStr(v);
              end;

              ColumnInfo.TableIndex := i;
            end;
          end;

          inc(p);
        end;
      finally
        RecordSet.FreeRecBuf(RecBuf);
      end;
      RecordSet.Close;
    end;
  finally
    RecordSet.Free;
  end;
end;

function TCustomODBCDataSetService.PreventPSKeyFields(var PSKeyFields: string): boolean;
begin
  Result := True;
end;

function TCustomODBCDataSetService.GetRecCount: integer;
var
  St: _string;
  UpdateQuery: TCustomDADataSet;
  MonitorClass: TDASQLMonitorClass;
  MessageID: cardinal;
begin
  Result := 0;
  St := FDataSet.FinalSQL;
  St := _SetOrderBy(St, '', TODBCParser);
  St := 'SELECT count(*) FROM (' + LineSeparator + St + LineSeparator + ')';

  TCustomODBCDataSetUpdater(FUpdater).CheckUpdateQuery(stCustom);
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

{ TODBCScriptProcessor }

function TODBCScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TODBCParser;
end;

{ TCustomODBCDumpProcessor }

function TCustomODBCDumpProcessor.CreateQuery: TCustomDADataSet;
begin
  Result := GetConnection.CreateDataSet;
  Result.ReadOnly := True;
  Result.UniDirectional := True;
  TDBAccessUtils.CheckConnection(Result);
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prExtendedFieldsInfo, False);
end;

{ TODBCServerEnumerator }

procedure TODBCServerEnumerator.GetServerList(List: _TStrings);
const
  BufSize = 256;
var
  Cli: TODBCCli;
  Buf: IntPtr;
  Res, Len, Len2: smallint;
  s: _string;
begin
  List.Clear;

  Cli := GetODBCCli;
  if not Cli.IsInited then
    Cli.InitEnvironment;

  Buf := Marshal.AllocHGlobal(BufSize * SizeOf(_char));
  try
    Res := Cli.SQLDataSources(Cli.SQLHEnv, SQL_FETCH_FIRST, Buf, BufSize, Len, nil, 0, Len2);
    repeat
      if Res = SQL_NO_DATA then
        exit;
      s := PtrToXString(Buf);
      List.Add(s);
      Res := Cli.SQLDataSources(Cli.SQLHEnv, SQL_FETCH_NEXT, Buf, BufSize, Len, nil, 0, Len2);
    until False;
  finally
    Marshal.FreeHGlobal(Buf);
  end;
end;

end.
