
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright (c) 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Odac.inc}
unit OraServicesUni;
{$ENDIF}

interface

uses
  SysUtils, Classes, DB,
{$IFDEF MSWINDOWS}
  Registry, Windows,
{$ENDIF}
{$IFDEF CLR}
  System.Text, System.XML, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses, CRXml,
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  MemUtils, MemData, CRAccess, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  DBAccess, DADump;

const
  prKeySequence        = 101; // string
  prSequenceMode       = 102; // integer
  prTNSPath            = 104; // string

type
  TCustomOraDataSetUpdater = class;
  TCustomOraDataSetService = class;

  // must be sync with types in Ora
  _TSequenceMode = (_smInsert, _smPost);

  TCustomOraDataTypesMap = class(TDataTypesMap)
    class function GetDataType(FieldType: TFieldType): integer; override;
    class function GetFieldType(DataType: Word): TFieldType; override;
  end;

  TCustomOraSQLGenerator = class(TDASQLGenerator)
  protected
    FDataSetService: TCustomOraDataSetService;

    FSeqReturning: boolean;
    FSeqFieldDesc: TCRFieldDesc;
  protected
    FReturnSB,
    FIntoSB: _StringBuilder;

    function IsBlobDataType(DataType: word): boolean; override;
    function IsObjectDataType(DataType: word): boolean; override;
    function FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean; override;

    function GenerateIndexName(Name: _string): _string; override;
    function MaxIdentLength: integer; override;

    function IsSubstituteParamName: boolean; override;
    procedure AddParam(SB: _StringBuilder; FieldDesc: TFieldDesc;
      const StatementType: TStatementType;
      const ParamType: TParamType;
      Index: integer = -1;
      Old: boolean = False); override;

    procedure AddFieldToInsertSQL(FieldDesc: TCRFieldDesc; const Index: integer = -1); override;

    procedure AddFieldToUpdateSQL(FieldDesc: TCRFieldDesc;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;

    procedure GenerateInsertSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;

    procedure GenerateUpdateSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;

    procedure GenerateRefreshSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean); override;

    procedure GenerateLockSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;
  public
    constructor Create(Owner: TDADataSetService); override;

    function GenerateSQL(const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean;
      Params: TDAParams;
      const Index: Integer = -1): _string; override;

    function GenerateTableSQL(const TableName, OrderFields: _string): _string; override;
    function GenerateSelectValues(const ValuesList: _string): _string; override;
  end;

  TCustomOraDataSetUpdater = class(TDADataSetUpdater)
  protected
    FDataSetService: TCustomOraDataSetService;

    procedure GetSequenceNextVal;
    function GetIdentityFieldValue(var Value: variant): boolean; override;

    function IsNeedInsertPreconnect: boolean; override;
    function IsNeedEditPreconnect: boolean; override;
    function IsPreconnected: boolean; override;

    function PrepareBatch(SQL: _string): _string; override;

    procedure CheckUpdateQuery(const StatementType: TStatementType); override;
    procedure SetUpdateQueryOptions(const StatementType: TStatementType); override;
    procedure UpdateExecute(const StatementTypes: TStatementTypes); override;

    procedure PrepareAppend; override;
    function PerformAppend: boolean; override;

    property UpdateQuery: TComponent read FUpdateQuery;
  public
    constructor Create(AOwner: TDataSetService); override;

    function PerformSQL(const SQL: _string; const StatementTypes: TStatementTypes): boolean; override;
    function GetDefaultExpressionValue(DefExpr: _string; var Value: variant): boolean; override;
  end;

  TCustomOraDataSetService = class(TDADataSetService)
  protected
    FUpdater: TCustomOraDataSetUpdater;

    FKeySequence: _string;
    FSequenceMode: _TSequenceMode;
    FScrollableCursor: boolean;

    function GetTemporaryLobUpdate: boolean;

    procedure CreateDataSetUpdater; override;
    procedure SetDataSetUpdater(Value: TDataSetUpdater); override;
    procedure CreateSQLGenerator; override;

    function DetectIdentityField: TCRFieldDesc; override;
    function DetectKeyGeneratorField: TField; override;
    function DetectHiddenFields: TFieldArray; override;
    function DetectCanModify: boolean; override;

    function ExtFieldsInfoIsInternal: boolean; override;
    procedure RequestFieldsInfo(Tables: TExtTablesInfo; Columns: TColumnsInfo); override;

    function GetRecCount: integer; override;
    function GetICommand: TCRCommand;
    procedure CheckIRecordSet;
    function UsedConnection: TCustomDAConnection;
    function IsFullRefresh: boolean;
    function IsDMLRefresh: boolean;
    function IsInCacheProcessing: boolean;
    function GetKeyFields: _string;
    function IsAutoCommit: boolean;

    function CompatibilityMode: boolean; virtual;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function PreventPSKeyFields(var PSKeyFields: string): boolean; override;
    function GetCurrentSchema: _string; override;

  { XML }
    procedure WriteFieldXMLDataType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: _string;
      XMLWriter: XMLTextWriter); override;
    function GetFieldXMLValue(Field: TField; FieldDesc: TFieldDesc): WideString; override;

    property KeyGeneratorField: TField read FKeyGeneratorField;
  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;

    function OpenNext: boolean; override;
    function NeedParamValuesOnPrepare: boolean; override;
  end;

  TOraServerEnumerator = class (TCRServerEnumerator)
  private
    FDirect: boolean;
    FHomeName: string;
    FTNSPath: string;

  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    function GetTNSFileName: string;
    procedure GetServerList(List: _TStrings); override;
  end;

  TCustomOraDumpProcessor = class(TDADumpProcessor)
  end;

implementation

uses
  DAConsts, DASQLMonitor, CRParser,
{$IFNDEF UNIDACPRO}
  OraCall, OraClasses, OraParser;
{$ELSE}
  OraCallUni, OraClassesUni, OraParserUni;
{$ENDIF}

{ TCustomOraDataTypesMap }

const
{$IFDEF VER5}
  OraDataTypeMap: array [TFieldType] of word = (
    dtUnknown, dtString, dtInteger, dtInteger, dtInteger, dtBoolean, dtFloat,
    dtFloat, dtUnknown, dtDateTime, dtDateTime, dtDateTime, dtBytes, dtVarBytes, 0, dtBlob, dtMemo,
    0, 0, 0, 0, 0, dtCursor, dtFixedChar, dtWideString, dtInteger, dtObject, dtArray, dtReference,
    dtTable, dtOraBlob, dtOraClob, 0, 0, 0, 0);
{$ENDIF}
{$IFDEF VER6P}
  OraDataTypeMap: array [TFieldType] of word = (
    dtUnknown, dtString, dtInteger, dtInteger, dtInteger, dtBoolean, dtFloat,
    dtFloat, dtUnknown, dtDateTime, dtDateTime, dtDateTime, dtBytes, dtVarBytes, 0, dtBlob, dtMemo,
    0, 0, 0, 0, 0, dtCursor, dtFixedChar, dtWideString, dtLargeInt, dtObject, dtArray, dtReference,
    dtTable, dtOraBlob, dtOraClob, 0, 0, 0, 0,
    {$IFNDEF FPC}dtSQLTimeStamp, dtFMTBcd{$ELSE}0, 0{$ENDIF}
  {$IFDEF FPC}
    , dtFixedWideChar, dtWideMemo
  {$ENDIF}
  {$IFDEF VER10P}
    , dtFixedWideChar, dtWideMemo, dtTimeStamp, 0
  {$IFDEF VER12P}
    , dtLargeInt, dtInteger, dtInteger, dtFloat, 0, 0, 0
  {$ENDIF}
  {$IFDEF VER14P}
    , 0, 0, dtFloat
  {$ENDIF}
  {$ENDIF}
  );
{$ENDIF}

class function TCustomOraDataTypesMap.GetDataType(FieldType: TFieldType): integer;
begin
{$IFNDEF FPC}
{$IFNDEF VER10P}
  if Integer(FieldType) = Integer(ftFixedWideChar) then
    Result := dtFixedWideChar
  else
{$ENDIF}
{$ENDIF}
    Result := OraDataTypeMap[FieldType];
end;

class function TCustomOraDataTypesMap.GetFieldType(DataType: Word): TFieldType;
begin
  case DataType of
    dtOraBlob:
      Result := ftOraBlob;
    dtOraClob, dtWideOraClob:
      Result := ftOraClob;
    dtFixedChar:
      Result := ftFixedChar;
    dtFixedWideChar:
      Result := TFieldType(ftFixedWideChar);
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    dtSQLTimeStamp:
      Result := ftTimeStamp;
  {$ENDIF}
  {$ENDIF}
    dtUndefined, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ, dtBFile, dtLabel,
    dtIntervalYM, dtIntervalDS, dtNumber, dtXML:
      Result := ftUnknown;
  else
    Result := inherited GetFieldType(DataType);
  end;
end;

{ TCustomOraSQLGenerator }

constructor TCustomOraSQLGenerator.Create(Owner: TDADataSetService);
begin
  inherited Create(Owner);

  FDataSetService := TCustomOraDataSetService(Owner);
end;

function TCustomOraSQLGenerator.IsBlobDataType(DataType: word): boolean;
begin
  Result := DataType in [dtBlob, dtMemo, dtWideMemo, dtOraClob, dtWideOraClob, dtOraBlob];
end;

function TCustomOraSQLGenerator.IsObjectDataType(DataType: word): boolean;
begin
  Result := inherited IsObjectDataType(DataType) or (DataType = dtTable);
end;

function TCustomOraSQLGenerator.FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean;
  Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean;
begin
  if FieldDesc = FSeqFieldDesc then
    Result := False
  else
    Result := inherited FieldIsNull(FieldDesc, OldValue, Data, OldRecBuf, NewRecBuf);
end;

function TCustomOraSQLGenerator.GenerateIndexName(Name: _string): _string;
begin
  Result := '"' + '_' + Name + '"';
end;

function TCustomOraSQLGenerator.MaxIdentLength: integer;
begin
  Result := 30;
end;

function TCustomOraSQLGenerator.IsSubstituteParamName: boolean;
begin
  Result := False;
end;

procedure TCustomOraSQLGenerator.AddParam(SB: _StringBuilder; FieldDesc: TFieldDesc;
  const StatementType: TStatementType; const ParamType: TParamType; Index: integer = -1;
  Old: boolean = False);
begin
  if FieldDesc = FDataSetService.IdentityField then
    Old := True;

  inherited AddParam(SB, FieldDesc, StatementType, ParamType, Index, Old);   
end;

procedure TCustomOraSQLGenerator.AddFieldToInsertSQL(FieldDesc: TCRFieldDesc; const Index: integer = -1);

  procedure AddComma;
  begin
    if FFldSB.Length > 0 then begin
      FFldSB.Append(', ');
      FFldParamSB.Append(', ');
    end;
  end;

begin
  if (FieldDesc.DataType = dtOraBlob) and not (FDataSetService.GetTemporaryLobUpdate) then begin
    AddComma;
    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
    FFldParamSB.Append('EMPTY_BLOB()');
  end
  else
  if (FieldDesc.DataType in [dtOraClob, dtWideOraClob]) and not (FDataSetService.GetTemporaryLobUpdate) then begin
    AddComma;
    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
    FFldParamSB.Append('EMPTY_CLOB()');
  end
  else
  if FSeqReturning and (FieldDesc = FSeqFieldDesc) then begin
    AddComma;
    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
    FFldParamSB.Append(FDataSetService.FKeySequence + '.NEXTVAL');
  end
  else
    inherited;
end;

procedure TCustomOraSQLGenerator.GenerateInsertSQL(const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean; const Index: integer = -1);
var
  Sequenced: boolean;
  FieldDesc: TCRFieldDesc;
  LobReturning: boolean;
  i: integer;

  ReturnSB: _StringBuilder;
  IntoSB: _StringBuilder;

  UsedConnection: TCustomDAConnection;
begin
  // we should not override user's value by the sequenced value
  Sequenced := (FDataSetService.KeyGeneratorField <> nil) and
               (FDataSetService.FSequenceMode = _smPost) and
               FDataSetService.KeyGeneratorField.IsNull;

  if Sequenced then begin
    UsedConnection := FDataSetService.UsedConnection;
    with TOCIConnection(TDBAccessUtils.GetIConnection(UsedConnection)) do
      FSeqReturning := (GetOCICallStyle = OCI80) and (GetOracleVersion >= 8000);

    FSeqFieldDesc := KeyAndDataFields.KeyFieldDescs[0];
    if not FSeqReturning then begin
      FHeaderSB.Append('begin'#$D#$A'  SELECT ');
      FHeaderSB.Append(FDataSetService.FKeySequence);
      FHeaderSB.Append('.NEXTVAL INTO ');
      AddParam(FHeaderSB, FSeqFieldDesc, stInsert, Index);
      FHeaderSB.Append(' FROM Dual;' + #$D#$A#$D#$A + '  ');
    end;
  end;

  ReturnSB := _StringBuilder.Create(100);
  IntoSB := _StringBuilder.Create(100);
  try
    LobReturning := False;
    if not FDataSetService.GetTemporaryLobUpdate then
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
        if KeyAndDataFields.DataFieldDescs[i].DataType in [dtOraBlob, dtOraClob, dtWideOraClob] then
          LobReturning := True;

    inherited GenerateInsertSQL(KeyAndDataFields, ModifiedFieldsOnly, Index);

    if FFldSB.Length = 0 then begin
      Clear;
      inherited GenerateInsertSQL(KeyAndDataFields, False, Index);
    end;

    if FDataSetService.IsDMLRefresh or LobReturning or Sequenced then begin
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if (FDataSetService.IsDMLRefresh and
          not (FieldDesc.DataType in [dtMemo, dtWideMemo, dtBlob])) or
          (LobReturning and (FieldDesc.DataType in [dtOraBlob, dtOraClob, dtWideOraClob]) and
          (not ModifiedFieldsOnly or not FieldIsNull(FieldDesc, False))) or
          ((FSeqFieldDesc = FieldDesc) and FSeqReturning)
        then begin
          if ReturnSB.Length > 0 then begin
            ReturnSB.Append(', ');
            IntoSB.Append(', ');
          end;
          ReturnSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
          AddParam(IntoSB, FieldDesc, stInsert, Index);
        end;
      end;
    end;

    if ReturnSB.Length > 0 then begin
      FFooterSB.Append(#$D#$A'RETURNING'#$D#$A'  ');
      FFooterSB.Append(ReturnSB);
    end;

    if IntoSB.Length > 0 then begin
      FFooterSB.Append(#$D#$A'INTO'#$D#$A'  ');
      FFooterSB.Append(IntoSB);
    end;

    if Sequenced and not FSeqReturning then
      FFooterSB.Append(';'#$D#$A'end;');
  finally
    FSeqFieldDesc := nil;
    FSeqReturning := False;
    ReturnSB.Free;
    IntoSB.Free;
  end;
end;

procedure TCustomOraSQLGenerator.AddFieldToUpdateSQL(FieldDesc: TCRFieldDesc;
  const ModifiedFieldsOnly: boolean; const Index: integer = -1);
begin
  if (FieldDesc.DataType in [dtOraBlob, dtOraClob, dtWideOraClob]) and not (FDataSetService.GetTemporaryLobUpdate) then begin
    if FFldSB.Length > 0 then
      FFldSB.Append(', ');

    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
    FFldSB.Append('=');

    if ModifiedFieldsOnly and FieldIsNull(FieldDesc, False) then
      FFldSB.Append('NULL')
    else begin
      if FieldDesc.DataType = dtOraBlob then
        FFldSB.Append('EMPTY_BLOB()')
      else
        FFldSB.Append('EMPTY_CLOB()');
    end;
  end
  else
    inherited;
end;

procedure TCustomOraSQLGenerator.GenerateUpdateSQL(const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean; const Index: integer = -1);
var
  i: integer;
  LobReturning: boolean;
  FieldDesc: TCRFieldDesc;

  ReturnSB: _StringBuilder;
  IntoSB: _StringBuilder;
begin
  inherited GenerateUpdateSQL(KeyAndDataFields, ModifiedFieldsOnly, Index);

  if FFldSB.Length = 0 then
    Exit;

  LobReturning := False;
  if not FDataSetService.GetTemporaryLobUpdate then
    for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
      if KeyAndDataFields.DataFieldDescs[i].DataType in [dtOraBlob, dtOraClob, dtWideOraClob] then
        LobReturning := True;

  ReturnSB := _StringBuilder.Create(100);
  IntoSB := _StringBuilder.Create(100);
  try
    if FDataSetService.IsDMLRefresh or LobReturning then begin
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if (FDataSetService.IsDMLRefresh and
          not (FieldDesc.DataType in [dtMemo, dtWideMemo, dtBlob, dtOraBlob, dtOraClob, dtWideOraClob])) or
          (LobReturning and (FieldDesc.DataType in [dtOraBlob,dtOraClob,dtWideOraClob]) and
          (not ModifiedFieldsOnly or FieldModified(FieldDesc)))
        then begin
          if ReturnSB.Length > 0 then begin
            ReturnSB.Append(', ');
            IntoSB.Append(', ');
          end;
          ReturnSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
          AddParam(IntoSB, FieldDesc, stUpdate, Index);
        end;
      end;

      if ReturnSB.Length <> 0 then begin
        FFooterSB.Append(#$D#$A'RETURNING'#$D#$A'  ');
        FFooterSB.Append(ReturnSB);
      end;

      if IntoSB.Length > 0 then
        FFooterSB.Append(#$D#$A'INTO'#$D#$A'  ');
        FFooterSB.Append(IntoSB);
      end;
  finally
    ReturnSB.Free;
    IntoSB.Free;
  end;
end;

procedure TCustomOraSQLGenerator.GenerateLockSQL(const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);
begin
  FHeaderSB.Append('SELECT * FROM ');
  FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.Options.QuoteNames));
  FMiddleSB.Append(#$D#$A'WHERE'#$D#$A'  ');
  GenerateConditions(FCondSB, stLock, False, KeyAndDataFields, Index);
  FFooterSB.Append(#$D#$A'FOR UPDATE NOWAIT');
end;

procedure TCustomOraSQLGenerator.GenerateRefreshSQL(const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean);
begin
  if FDesignMode and (FDataSetService.IsFullRefresh or FDataSet.ReadOnly)
  then begin
    GenerateConditions(FHeaderSB, stRefresh, ModifiedFieldsOnly, KeyAndDataFields);
    FHeaderSB.Insert(0, 'WHERE'#$D#$A'  ');
  end
  else
    inherited GenerateRefreshSQL(KeyAndDataFields, ModifiedFieldsOnly);
end;

function TCustomOraSQLGenerator.GenerateSQL(const StatementType: TStatementType;
  const ModifiedFieldsOnly: boolean;
  Params: TDAParams;
  const Index: Integer = -1): _string;
begin
  if FDataSetService.CompatibilityMode then
    Result := ''
  else
    Result := inherited GenerateSQL(StatementType, ModifiedFieldsOnly, Params, Index);
end;

function TCustomOraSQLGenerator.GenerateTableSQL(const TableName, OrderFields: _string): _string;
var
  QuotedTableName, St1: _string;
  i: integer;
  KeyFields: _string;
begin
  FDataSetService.CheckIRecordSet;
  QuotedTableName := SQLInfo.NormalizeName(TableName, FDataSet.Options.QuoteNames);

  KeyFields := FDataSetService.GetKeyFields;
  if (KeyFields = '') and not FDataSet.ReadOnly then begin
    KeyFields := FDataSetService.GetDBKeyList(TableName);
    // we should not fill published fields automatically
    //TDBAccessUtils.SetKeyFields(FDataSet, KeyFields);
  end;

  if (KeyFields = '') or (_UpperCase(KeyFields) = 'ROWID') then
    Result := 'SELECT T.RowId, T.*'#13#10'FROM ' + QuotedTableName + ' T'#13#10
  else
    Result := 'SELECT T.*'#13#10'FROM ' + QuotedTableName + ' T'#13#10;

  if OrderFields <> '' then begin
    St1 := OrderFields;

    for i := 1 to Length(St1) do
      if St1[i] = ';' then
        St1[i] := ',';

    Result := Result + 'ORDER BY ' + St1 + #13#10;
  end;
end;

function TCustomOraSQLGenerator.GenerateSelectValues(const ValuesList: _string): _string;
begin
  Result := 'SELECT ' + ValuesList + ' FROM Dual';
end;

{ TCustomOraUpdater }

constructor TCustomOraDataSetUpdater.Create(AOwner: TDataSetService);
begin
  inherited Create(AOwner);

  FDataSetService := TCustomOraDataSetService(AOwner);
end;

function TCustomOraDataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
var
  Command: TOCICommand;
  RowId: string;
begin
  Command := TOCICommand(TDBAccessUtils.GetICommand(TCustomDADataSet(FUpdateQuery)));
  Assert(Command <> nil);
  RowId := Command.GetRowId;
  Result := RowID <> '';
  if Result then
    Value := RowID;
end;

procedure TCustomOraDataSetUpdater.CheckUpdateQuery(const StatementType: TStatementType);
begin
  inherited;

  TDBAccessUtils.GetICommand(FUpdateQuery as TCustomDADataSet).SetProp(prStoreRowId, True);
end;

procedure TCustomOraDataSetUpdater.SetUpdateQueryOptions(const StatementType: TStatementType);
var
  DestCommand, SourceCommand: TCRCommand;

  procedure CopyPropC(Prop: integer);
  var
    v: variant;
  begin
    SourceCommand.GetProp(Prop, v);
    DestCommand.SetProp(Prop, v);
  end;

begin
  CheckIRecordSet; // can be inactive
  SourceCommand := GetICommand;
  DestCommand := TDBAccessUtils.GetICommand(FUpdateQuery as TCustomDADataSet);

  CopyPropC(prCacheLobs);
  CopyPropC(prFieldsAsString);
  CopyPropC(prRawAsString);
  CopyPropC(prTemporaryLobUpdate);
end;

procedure TCustomOraDataSetUpdater.UpdateExecute(const StatementTypes: TStatementTypes);
var
  i: integer;
  Param: TParam;
  Field: TField;
begin
  if (NeedReturnParams or (stRefresh in StatementTypes)) then
    for i := 0 to TDBAccessUtils.GetParams(FUpdateQuery).Count - 1 do begin
      if not (TDBAccessUtils.GetParams(FUpdateQuery)[i].DataType in [ftOraBlob, ftOraClob]) then
        TDBAccessUtils.GetParams(FUpdateQuery)[i].ParamType := ptInputOutput;
    end;
  // Update ROWID value for successful Refresh after Update on Post or on
  // ApplyUpdaes (UROWID)
  if (stRefresh in StatementTypes) and ((FDataSet.State = dsEdit) or FDataSetService.IsInCacheProcessing)
    and (FDataSetService.IdentityField <> nil)
  then begin
    Param := TDBAccessUtils.GetParams(FUpdateQuery).FindParam('OLD_ROWID');
    Field := FDataSet.GetField(FDataSetService.IdentityField);
    if (Param <> nil) and (Field <> nil) then
      Param.Value := Field.Value;
  end;
  inherited;
end;

function TCustomOraDataSetUpdater.PerformSQL(const SQL: _string; const StatementTypes: TStatementTypes): boolean;
var
  RowId: string;
  Command: TOCICommand;
  Connection: TOCIConnection;
begin
  Result := inherited PerformSQL(SQL, StatementTypes);

  if Result and (stUpdate in StatementTypes) then begin
    RowId := TOCICommand(TDBAccessUtils.GetICommand(TCustomDADataSet(FUpdateQuery))).GetRowId;
    if (RowId <> '') and (RowId[1] = '*') then // UROWID was returned
      SetIdentityFieldValue; // for correct updates when UROWID was changed
  end;

  if Result and (OCIVersion >= 8150) and (OCIVersion < 9000)
    and (stInsert in StatementTypes) and (FDataSetService.IdentityField <> nil)
  then begin
    Command := TOCICommand(TDBAccessUtils.GetICommand(TCustomDADataSet(FUpdateQuery)));
    Connection := TOCIConnection(TDBAccessUtils.GetIConnection(UsedConnection));
    if (Command.GetRowId = '')
      and (Connection.GetOracleVersion > 8000) and (Connection.GetOracleVersion < 8100)
    then begin
      TCustomDADataSet(FUpdateQuery).SQL.Text := 'select 1 from dual where 1=0';
      TCustomDADataSet(FUpdateQuery).Execute;
      TCustomDADataSet(FUpdateQuery).Close;
    end;
  end;
end;

function TCustomOraDataSetUpdater.GetDefaultExpressionValue(DefExpr: _string; var Value: variant): boolean;
begin
  Result := True;
  DefExpr := _UpperCase(Trim(DefExpr));
  if (DefExpr = 'EMPTY_BLOB()') or (DefExpr = 'EMPTY_CLOB()') then
    Value := ''
  else
    Result := inherited GetDefaultExpressionValue(DefExpr, Value);
end;

procedure TCustomOraDataSetUpdater.PrepareAppend;
begin
  if (FDataSetService.KeyGeneratorField <> nil) and (FDataSetService.FSequenceMode = _smInsert) then
    GetSequenceNextVal;
end;

function TCustomOraDataSetUpdater.PerformAppend: boolean;
var
  OldReturnParams: boolean;
begin
  OldReturnParams := FDataSet.Options.ReturnParams;
  if FDataSetService.KeyGeneratorField <> nil then
    if ((FDataSet.SQLInsert.Count > 0) or ((GetUpdateObject <> nil)
      and (GetUpdateObject.InsertSQL.Count > 0))) and
      (FDataSetService.FSequenceMode = _smPost)
    then
      GetSequenceNextVal
    else
      FDataSet.Options.ReturnParams := True;

  try
    Result := inherited PerformAppend;
  finally
    FDataSet.Options.ReturnParams := OldReturnParams;
  end;
end;

procedure TCustomOraDataSetUpdater.GetSequenceNextVal;
var
  OldReturnParams: boolean;
  KeyFieldDescs: TFieldDescArray;
begin
  FDataSetService.GetKeyFieldDescs(KeyFieldDescs);
  if Length(KeyFieldDescs) > 0 then begin
    OldReturnParams := FDataSet.Options.ReturnParams;
    FDataSet.Options.ReturnParams := True;
    try
      PerformSQL('begin' + LineSeparator + '  SELECT ' + FDataSetService.FKeySequence + '.NEXTVAL INTO :' +
        KeyFieldDescs[0].Name +' FROM Dual;' + LineSeparator + 'end;', [stCustom]);
    finally
      FDataSet.Options.ReturnParams := OldReturnParams;
    end;
  end;
end;

function TCustomOraDataSetUpdater.PrepareBatch(SQL: _string): _string;
begin
  Result := 'BEGIN' + #13#10 + SQL + #13#10 + 'END;'
end;

function TCustomOraDataSetUpdater.IsNeedEditPreconnect: boolean;
var
  vHasObjectFields: variant;
begin
  Result := not FDataSet.CachedUpdates and (GetLockMode = lmPessimistic);
  if not Result then begin
    GetIRecordSet.GetProp(prHasObjectFields, vHasObjectFields);
    Result := (vHasObjectFields = True);
  end;
end;

function TCustomOraDataSetUpdater.IsPreconnected: boolean;
var
  vHasObjectFields: variant;
begin
  Result := inherited IsPreconnected;
  if UsedConnection.Options.DisconnectedMode then begin         //in case of Object fields there is pre-connection during
    GetIRecordSet.GetProp(prHasObjectFields, vHasObjectFields); //InitRecord, so we should call EndConnection after post or cancel
    Result := Result or (vHasObjectFields = True);
  end;
end;

function TCustomOraDataSetUpdater.IsNeedInsertPreconnect: boolean;
begin
  Result := (FDataSetService.FSequenceMode = _smInsert);
end;

{ TCustomOraDataSetService }

procedure TCustomOraDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomOraDataSetUpdater.Create(Self));
end;

procedure TCustomOraDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomOraSQLGenerator.Create(Self));
end;

procedure TCustomOraDataSetService.SetDataSetUpdater(Value: TDataSetUpdater);
begin
  inherited;

  FUpdater := TCustomOraDataSetUpdater(Value);
end;

function TCustomOraDataSetService.GetTemporaryLobUpdate: boolean;
var
  PropValue: Variant; 
begin
  GetIRecordSet.GetCommand.GetProp(prTemporaryLobUpdate, PropValue);
  Result := PropValue;
end;

function TCustomOraDataSetService.DetectIdentityField: TCRFieldDesc;
var
  i: integer;
  FieldDesc: TCRFieldDesc;
  RecordSet: TOCIRecordSet;
begin
  Result := nil;
  //Search Identity Field
  RecordSet := TOCIRecordSet(GetIRecordSet);
  for i := 0 to RecordSet.FieldCount - 1 do begin
    FieldDesc := TCRFieldDesc(RecordSet.Fields[i]);
    if CompareText(FieldDesc.ActualName, 'ROWID') = 0 then
      if TCRFieldDesc(FieldDesc).TableInfo = UpdatingTableInfo then begin
        Result := FieldDesc;
        break;
      end;
  end;
end;

function TCustomOraDataSetService.DetectKeyGeneratorField: TField;
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

function TCustomOraDataSetService.DetectHiddenFields: TFieldArray;
var
  i: integer;
  FieldDesc: TCRFieldDesc;
  RecordSet: TOCIRecordSet;
  Field: TField;
begin
  Result := nil;
  RecordSet := TOCIRecordSet(GetIRecordSet);
  for i := 0 to RecordSet.FieldCount - 1 do begin
    FieldDesc := TCRFieldDesc(RecordSet.Fields[i]);
    if CompareText(FieldDesc.ActualName, 'ROWID') = 0 then begin
      Field := FDataSet.GetField(FieldDesc);
      if Field <> nil then begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := Field;
      end;
    end;
  end;
end;

function TCustomOraDataSetService.DetectCanModify: boolean;
begin
  Result := (inherited DetectCanModify or
    not FDataSet.ReadOnly and
    (not CompatibilityMode or
    (FDataSet.SQLInsert.Count > 0) or
    (FDataSet.SQLUpdate.Count > 0) or
    (FDataSet.SQLDelete.Count > 0)) and
     FIsAnyFieldCanBeModified) and
    not (FScrollableCursor); // can't modify scrollable cursor
end;

function TCustomOraDataSetService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prKeySequence:
      FKeySequence := Value;
    prSequenceMode:
      FSequenceMode := _TSequenceMode(Value);
    prScrollableCursor:
      FScrollableCursor := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end; 
end;

function TCustomOraDataSetService.OpenNext: boolean;
var
  Cursor: TCRCursor;
begin
  if not FDataSet.Active then begin
    FDataSet.Open;
    Result := True;
  end
  else begin
    Cursor := TOCICommand(GetIRecordSet.GetCommand).GetNextCursor;
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

function TCustomOraDataSetService.NeedParamValuesOnPrepare: boolean;
begin
  Result := True;
end;

function TCustomOraDataSetService.ExtFieldsInfoIsInternal: boolean;
begin
  Result := False;
end;

procedure TCustomOraDataSetService.RequestFieldsInfo(Tables: TExtTablesInfo; Columns: TColumnsInfo);
var
  UserName: _string;
  Query: TCustomDADataSet;

  function AddDBLink(DBLink: _string): _string;
  begin
    if DBLink <> '' then
      Result := '@' + DBLink;
  end;

  function GetDefExpr(Value: _string; DataType: _string): _string;
  var
    P: integer;
    DeqValue: _string;
  begin
    if DefaultExpressionOldBehavior then begin
      Result := TrimRight(Value);
      DeqValue := _DequotedStr(Result, '''');
      if (Result <> DeqValue) and (_QuotedStr(DeqValue, '''') = Result) then // '1' + '2' should not be dequoted
        Result := DeqValue;
      if (DataType = 'NUMBER') and (DecimalSeparator{$IFDEF CLR}[1]{$ENDIF} <> '.') then begin
        P := Pos('.', Result);
        if P > 0 then
          Result[P] := _char(DecimalSeparator{$IFDEF CLR}[1]{$ENDIF});
      end;
    end
    else
      Result := Value;
  end;

  function GetOwner: _string;
  begin
    Result := TOCIConnection(TDBAccessUtils.GetIConnection(UsedConnection)).GetCachedSchema;
  end;

  procedure GetFieldsInfoServer(var NeedDescribeSynonyms: boolean);
  var
    SQL, Filter: string;
    CurTableName, FieldTableName, CurOwner, FieldOwner, DBLink: _string;
    j, p: integer;
    ColumnInfo, NewColumnInfo: TColumnInfo;
    Located, MoreDBLinks: boolean;
  begin
    NeedDescribeSynonyms := False;
    for j := 0 to High(Tables) do
      Tables[j].Flag := Tables[j].Flag and not 2; // 2 - info is queried
    repeat
      Filter := '';
      MoreDBLinks := False;
      for j := 0 to High(Tables) do
        if (Tables[j].Flag and 1 <> 0) and (Tables[j].Flag and 2 = 0) then begin
          if Filter = '' then
            DBLink := Tables[j].DBLink
          else begin
            if Tables[j].DBLink <> DBLink then begin
              MoreDBLinks := True;
              continue;
            end;
            Tables[j].Flag := Tables[j].Flag or 2;
            Filter := Filter + ' OR';
          end;
          Filter := Filter + ' (table_name = ''' + Tables[j].Table + '''' +
            ' and owner = ''' + Tables[j].Schema + ''')';
        end;

      if Filter = '' then
        exit;

      SQL := 'SELECT owner, table_name, column_name';
      if FDataSet.Options.DefaultValues then
        SQL := SQL + ', data_default, data_type';
      SQL := SQL + ' FROM all_tab_columns' + AddDBLink(DBLink) +
        ' WHERE ' + Filter +
        ' ORDER BY owner, table_name, column_id';

      Query.SQL.Text := SQL;
      try
        Query.Open;
      except
        //Probably no DBLink
      end;

      p := 0;
      while p < Columns.Count do begin
        ColumnInfo := TColumnInfo(Columns[p]);
        if (ColumnInfo.TableIndex = -1) and (ColumnInfo.Table <> '') and
          not FUpdTableIsArtificial or
          not FDataSet.Options.FieldsOrigin and (ColumnInfo.TableIndex <> -1)
          and (ColumnInfo.TableIndex <> FUpdatingTableInfoIdx)
        then
          ColumnInfo.Described := True;

        if ColumnInfo.Described then begin
          Inc(p);
          continue;
        end;

        if ColumnInfo.Name = '*' then begin
          if ColumnInfo.TableIndex <> -1 then begin
            FieldTableName := Tables[ColumnInfo.TableIndex].Table;
            FieldOwner := Tables[ColumnInfo.TableIndex].Schema;
          end;
          NewColumnInfo := nil;
          Query.First;
          while not Query.EOF do begin
            CurOwner := Query.Fields[0].AsString;
            CurTableName := Query.Fields[1].AsString;
            if (ColumnInfo.TableIndex = -1) or
              (CurOwner = FieldOwner) and (CurTableName = FieldTableName)
            then begin
              Inc(p);
              NewColumnInfo := TColumnInfo.Create;
              Columns.Insert(p, NewColumnInfo);
              NewColumnInfo.Used := False;
              NewColumnInfo.Described := True;
              NewColumnInfo.Table := ColumnInfo.Table;
              NewColumnInfo.TableIndex := ColumnInfo.TableIndex;
              NewColumnInfo.Name := Query.Fields[2].AsString;
              NewColumnInfo.Alias := NewColumnInfo.Name;

              if NewColumnInfo.TableIndex = -1 then begin
                for j := 0 to High(Tables) do
                  if (Tables[j].Schema = CurOwner) and (Tables[j].Table = CurTableName) then begin
                    NewColumnInfo.TableIndex := j;
                    break;
                  end;
              end;
              if NewColumnInfo.TableIndex <> -1 then
                Tables[NewColumnInfo.TableIndex].Flag := 0;

              if FDataSet.Options.DefaultValues and (NewColumnInfo.TableIndex = FUpdatingTableInfoIdx) then
                NewColumnInfo.Expr := GetDefExpr(Query.Fields[3].AsString, Query.Fields[4].AsString);
            end
            else
              if (NewColumnInfo <> nil) and (ColumnInfo.TableIndex <> -1) then
                break;
            Query.Next;
          end;
          if (NewColumnInfo <> nil) and (ColumnInfo.TableIndex <> -1) then
            ColumnInfo.Described := True;
        end
        else
          if (ColumnInfo.Name <> '') and (ColumnInfo.Name <> 'ROWID') then begin
            if ColumnInfo.TableIndex <> -1 then begin
              if FDataSet.Options.DefaultValues and (ColumnInfo.TableIndex = FUpdatingTableInfoIdx) then
                Located := Query.Locate('owner;table_name;column_name',
                  VarArrayOf([Tables[ColumnInfo.TableIndex].Schema, Tables[ColumnInfo.TableIndex].Table, ColumnInfo.Name]), [])
              else begin
                ColumnInfo.Described := True;
                Located := False;
              end;
            end
            else
              Located := Query.Locate('column_name', ColumnInfo.Name, []);

            if Located then begin
              ColumnInfo.Described := True;
              if FDataSet.Options.DefaultValues then
                ColumnInfo.Expr := GetDefExpr(Query.Fields[3].AsString, Query.Fields[4].AsString);

              if ColumnInfo.TableIndex = -1 then begin
                CurOwner := Query.Fields[0].AsString;
                CurTableName := Query.Fields[1].AsString;
                for j := 0 to High(Tables) do
                  if (Tables[j].Schema = CurOwner) and (Tables[j].Table = CurTableName) then begin
                    ColumnInfo.TableIndex := j;
                    break;
                  end;
              end;
              if ColumnInfo.TableIndex <> -1 then
                Tables[ColumnInfo.TableIndex].Flag := 0;
            end;
          end
          else
            ColumnInfo.Described := True;

        inc(p);
      end;

      for j := 0 to High(Tables) do
        if (Tables[j].Flag and 1 <> 0) and (Tables[j].DBLink = DBLink) then begin
          if Query.Locate('owner;table_name',
            VarArrayOf([Tables[j].Schema, Tables[j].Table]), [])
          then
            Tables[j].Flag := 0
          else
            NeedDescribeSynonyms := True;
        end;

    until not MoreDBLinks;

    if NeedDescribeSynonyms then begin
      NeedDescribeSynonyms := False;
      for j := 0 to Columns.Count - 1 do
        if not TColumnInfo(Columns[j]).Described then begin
          NeedDescribeSynonyms := True;
          break;
        end;
    end;
  end;

  procedure GetDBLinkSchema;
  var
    SQL: _string;
    i: integer;
  begin
    for i := 0 to High(Tables) do
      if (Tables[i].Flag and 1 <> 0) and
        (Tables[i].DBLink <> '') and (Tables[i].Schema = '')
      then
        SQL := SQL + ' AND db_link = ''' + Tables[i].DBLink + '''';

    if SQL = '' then
      Exit;

    SQL := 'SELECT username, db_link FROM all_db_links' +
           ' WHERE owner IN (''' + GetOwner + ''', ''PUBLIC'') ' +
           SQL +
           ' ORDER BY decode(owner, ''PUBLIC'', 1, 0) ';

    Query.SQL.Text := SQL;
    Query.Open;

    for i := 0 to High(Tables) do
      if (Tables[i].Flag and 1 <> 0) and
        (Tables[i].DBLink <> '') and (Tables[i].Schema = '')
      then
        if Query.Locate('db_link', Tables[i].DBLink, []) then
          Tables[i].Schema := _VarToStr(Query.Fields[0].Value);

    Query.Close;

    for i := 0 to High(Tables) do
      if (Tables[i].Flag and 1 <> 0) and
        (Tables[i].DBLink <> '') and
        ((Tables[i].Schema = '') or (Tables[i].Schema = 'CURRENT_USER'))
      then begin
        if UserName = '' then begin
          UserName := UsedConnection.Username;
          UserName := OCISQLInfo.NormalizeName(UserName, False, True);
          if UserName = '' then begin
            Query.SQL.Text := 'SELECT sys_context(''USERENV'', ''SESSION_USER'') FROM dual';
            Query.Open;
            UserName := _VarToStr(Query.Fields[0].Value);
            Query.Close;
          end;
        end;
        Tables[i].Schema := UserName;
      end;
  end;

  procedure DescribeSynonyms;
  var
    j: integer;
    SQL, Filter, DBLink: _string;
    MoreDBLinks: boolean;
  begin
    for j := 0 to High(Tables) do
      Tables[j].Flag := Tables[j].Flag and not 2;
    repeat
      Filter := '';
      MoreDBLinks := False;
      for j := 0 to High(Tables) do
        if (Tables[j].Flag and 1 <> 0) and (Tables[j].Flag and 2 = 0) then begin
          if Filter = '' then
            DBLink := Tables[j].DBLink
          else begin
            if Tables[j].DBLink <> DBLink then begin
              MoreDBLinks := True;
              continue;
            end;
            Tables[j].Flag := Tables[j].Flag or 2;
            Filter := Filter + ' OR';
          end;
          Filter := Filter + ' (synonym_name = ''' + Tables[j].Table + '''';
          if Tables[j].Flag and 8 = 0 then
            Filter := Filter + ' AND owner IN (''' + Tables[j].Schema + ''',''PUBLIC'') )'
          else
            Filter := Filter + ' AND owner = ''' + Tables[j].Schema + ''')';
        end;

      if Filter = '' then
        Exit;

      SQL := 'SELECT synonym_name, table_owner, table_name, db_link ' +
        'FROM all_synonyms' + AddDBLink(DBLink) +
        ' WHERE ' + Filter +
        ' ORDER BY decode(owner, ''PUBLIC'', 1, 0)';

      Query.SQL.Text := SQL;
      Query.Open;

      for j := 0 to High(Tables) do
        if (Tables[j].Flag and 1 <> 0) and (Tables[j].DBLink = DBLink) then
          if Query.Locate('synonym_name', Tables[j].Table, []) then begin
            if (DBLink <> '') and (_VarToStr(Query.Fields[3].Value) <> '') then
              Tables[j].Flag := 0
            else begin
              Tables[j].Synonym := Tables[j].Table;
              Tables[j].Table := _VarToStr(Query.Fields[2].Value);
              Tables[j].DBLink := _VarToStr(Query.Fields[3].Value);
              Tables[j].Schema := _VarToStr(Query.Fields[1].Value);
              Tables[j].Flag := Tables[j].Flag or 8;
            end
          end
          else
            Tables[j].Flag := 0;

    until not MoreDBLinks;

    GetDBLinkSchema;
  end;

var
  i: integer;
  NeedDescribeSynonyms: boolean;
begin
  for i := 0 to High(Tables) do
    with Tables[i] do begin
      if Schema = '' then
        Schema := GetOwner;
      if FDataSet.Options.FieldsOrigin or (i = FUpdatingTableInfoIdx) then
        Flag := 1
      else
        Flag := 0; // don't query fields for this table
    end;

  Query := UsedConnection.CreateDataSet;
  try
    TDBAccessUtils.SetLockDebug(Query, True);
    repeat
      GetFieldsInfoServer(NeedDescribeSynonyms);

      if NeedDescribeSynonyms then
        DescribeSynonyms;
    until not NeedDescribeSynonyms;
  finally
    Query.Free;
  end;
end;

function TCustomOraDataSetService.GetRecCount: integer;
var
  St: _string;
  UpdateQuery: TCustomDADataSet;
  MonitorClass: TDASQLMonitorClass;
  MessageID: cardinal;
  Parser: TOraParser;
begin
  Result := 0;
  St := FDataSet.FinalSQL;
  St := _SetOrderBy(St, '', TOraParser);
  Parser := TOraParser.Create(St);
  try
    if Parser.ToLexem(lxFOR) <> lcEnd then begin
      St := copy(St, 1, Parser.CurrPos - 3);
    end;
  finally
    Parser.Free;
  end;
  St := 'SELECT Count(*) FROM (' + LineSeparator + St + LineSeparator + ')';

  FUpdater.CheckUpdateQuery(stCustom);
  UpdateQuery := TCustomDADataSet(FUpdater.UpdateQuery);
  UpdateQuery.SQL.Text := St;
  UpdateQuery.Params.Assign(FDataSet.Params);

  UpdateQuery.Prepare;
  if TOCICommand(TDBAccessUtils.GetICommand(UpdateQuery)).GetSQLType = SQL_SELECT then begin
    MonitorClass := TDASQLMonitorClass(TDBAccessUtils.SQLMonitorClass(UsedConnection));
    if MonitorClass.HasMonitor or FDataSet.Debug then
      MonitorClass.SQLExecute(FDataSet, St, UpdateQuery.Params, 'Get RecordCount', MessageID, True);

    UpdateQuery.Open;
    if not UpdateQuery.EOF then
      Result := UpdateQuery.Fields[0].AsInteger;

    if MonitorClass.HasMonitor or FDataSet.Debug then
      MonitorClass.SQLExecute(FDataSet, St, UpdateQuery.Params, 'Get RecordCount', MessageID, False);
  end;
end;

function TCustomOraDataSetService.GetICommand: TCRCommand;
begin
  Result := inherited GetICommand;
end;

procedure TCustomOraDataSetService.CheckIRecordSet;
begin
  inherited;
end;

function TCustomOraDataSetService.UsedConnection: TCustomDAConnection;
begin
  Result := inherited UsedConnection;
end;

function TCustomOraDataSetService.IsFullRefresh: boolean;
begin
  Result := inherited IsFullRefresh;
end;

function TCustomOraDataSetService.IsDMLRefresh: boolean;
begin
  Result := inherited IsDMLRefresh;
end;

function TCustomOraDataSetService.IsInCacheProcessing: boolean;
begin
  Result := inherited IsInCacheProcessing;
end;

function TCustomOraDataSetService.GetKeyFields: _string;
begin
  Result := inherited GetKeyFields;
end;

function TCustomOraDataSetService.IsAutoCommit: boolean;
begin
  Result := inherited IsAutoCommit;
end;

function TCustomOraDataSetService.CompatibilityMode: boolean;
begin
  Result := False;
end;

function TCustomOraDataSetService.GetFieldClass(FieldType: TFieldType): TFieldClass;
{$IFDEF VER10P}
var
  Value: variant;
{$ENDIF}
begin
{$IFDEF VER10P}
  if FieldType = ftOraClob then begin
    TDBAccessUtils.GetIConnection(UsedConnection).GetProp(prUseUnicode, Value);
    if Boolean(Value) then
      Result := TWideMemoField
    else
      Result := TMemoField;
  end
  else
{$ENDIF}
    Result := inherited GetFieldClass(FieldType);
end;

function TCustomOraDataSetService.PreventPSKeyFields(var PSKeyFields: string): boolean;
begin
  Result := True;
end;

function TCustomOraDataSetService.GetCurrentSchema: _string;
begin
  Result := TOCIConnection(TDBAccessUtils.GetIConnection(UsedConnection)).GetCachedSchema;
  // to preserve character case
  Result := '"' + Result + '"';
end;

procedure TCustomOraDataSetService.WriteFieldXMLDataType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: _string;
  XMLWriter: XMLTextWriter);
var
  Length: integer;
begin
  case FieldDesc.Datatype of
    dtNumber: begin
      Length := FieldDesc.Length;
      if FieldDesc.Scale <> 0 then
        Inc(Length);
    end;
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ, dtIntervalYM, dtIntervalDS:
      Length := 50;
  else
    Length := 0;
  end;

  case FieldDesc.Datatype of
    dtNumber, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ, dtIntervalYM, dtIntervalDS: begin
      XmlWriter.WriteAttributeString('dt:type', 'string');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(Length));
      XmlWriter.WriteAttributeString('rs:dbtype', 'str');
    end;
    dtOraBlob, dtBFILE: begin
      XmlWriter.WriteAttributeString('dt:type', 'bin.hex');
      XmlWriter.WriteAttributeString('dt:maxLength', '2147483647');
      XmlWriter.WriteAttributeString('rs:long', 'true');
    end;
    dtOraClob, dtWideOraClob, dtCFILE, dtXML: begin
      XmlWriter.WriteAttributeString('dt:type', 'string');
      XmlWriter.WriteAttributeString('dt:maxLength', '2147483647');
      if FieldDesc.DataType <> dtWideOraClob then
        XmlWriter.WriteAttributeString('rs:dbtype', 'str');
      XmlWriter.WriteAttributeString('rs:long', 'true');
    end;
  else
    inherited;
    Exit;
  end;
  
  if Field.Required and not Field.ReadOnly then
    XmlWriter.WriteAttributeString('rs:maybenull', 'false');
end;

function TCustomOraDataSetService.GetFieldXMLValue(Field: TField; FieldDesc: TFieldDesc): WideString;
var
  Blob: TBlob;
  Piece: PPieceHeader;
{$IFDEF CLR}
  sb: StringBuilder;
  Buffer, Bytes: TBytes;
{$ELSE}
  sbOffset: integer;
  StrValue: string;
{$ENDIF}
begin
  case FieldDesc.DataType of
    dtOraBlob, dtBFILE: begin
      Blob := FDataSet.GetBlob(Field);
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
        Result := sb.ToString;
      finally
        sb.Free;
      end;
    {$ELSE}
      SetLength(StrValue, Blob.Size * 2);
      sbOffset := 0;
      while Piece <> nil do begin
        BinToHex(PtrOffset(Piece, sizeof(TPieceHeader)),
          PChar(PtrOffset(IntPtr(StrValue), sbOffset)), Piece.Used);
        sbOffset := sbOffset + Piece.Used * 2;
        Piece := Piece.Next;
      end;
      Result := StrValue;
    {$ENDIF}
    end;
    dtOraClob, dtWideOraClob, dtCFILE: begin
      Blob := FDataSet.GetBlob(Field);
      Result := Blob.AsWideString; // Unicode CLOBs support
    end;
  else
    Result := inherited GetFieldXMLValue(Field, FieldDesc);
  end;
end;

{ TOraServerEnumerator }

type
  TTNSParser = class (TParser)
  protected
    function IsInlineComment(Ch: _char; Pos: integer): boolean; override;
  end;

function TTNSParser.IsInlineComment(Ch: _char; Pos: integer): boolean;
begin
  Result := (TextLength >= Pos + 1) and (Text[Pos] = '#');
end;

function TOraServerEnumerator.GetTNSFileName: string;
const
  sTnsAdmin = 'TNS_ADMIN';
  sTnsNames = 'tnsnames.ora';
{$IFDEF MSWINDOWS}
  sOraHomeKey = 'SOFTWARE\ORACLE\';
  sNetwork = '\network\admin\';
  sNet80 = '\net80\admin\';
{$ENDIF}
{$IFDEF LINUX}
  sNetwork = '/network/admin/';
{$ENDIF}
var
  i: integer;
{$IFDEF MSWINDOWS}
  RegIniFile: TRegIniFile;
{$ENDIF}
  HomePath, FileName: string;
  TNSFound: boolean;

  function AddPath(Path, FileName: string): string;
  begin
    if (Path <> '') and (Path[Length(Path)] <> '\') then
      Result := Path + '\' + FileName
    else
      Result := Path + FileName;
  end;

begin
  TNSFound := False;

  if FTNSPath <> '' then begin
    FileName := AddPath(FTNSPath, sTnsNames);
    if FileExists(FileName) then
      TNSFound := True;
  end
  else begin
  {$IFDEF MSWINDOWS}
    /// check if TNS_ADMIN environment variable is set
  {$IFNDEF CLR}
    i := GetEnvironmentVariable(sTnsAdmin, nil, 0);
    if i > 0 then begin
      SetLength(FileName, i - 1);
      GetEnvironmentVariable(sTnsAdmin, PChar(FileName), Length(FileName) + 1);
      FileName := AddPath(FileName, sTnsNames);
      if FileExists(FileName) then
        TNSFound := True;
    end;
  {$ENDIF}

    if not TNSFound then begin
      /// look for TNS_ADMIN variable in registry
      if not LoadedOCI then
        DetectOCI;

      RegIniFile := TRegIniFile.Create('', KEY_READ OR KEY_WRITE);
      try
        RegIniFile.RootKey := HKEY_LOCAL_MACHINE;
        if (FHomeName = '') and (OracleHomeCount > 0) and (DefaultOracleHome >= 0) then
          FileName := AddPath(RegIniFile.ReadString(sOraHomeKey +
            OracleHomeKeys[DefaultOracleHome], sTnsAdmin, ''), sTnsNames)
        else
          for i := 0 to OracleHomeCount - 1 do
            if AnsiCompareText(OracleHomeNames[i] , FHomeName) = 0 then begin
              FileName := AddPath(RegIniFile.ReadString(sOraHomeKey +
                OracleHomeKeys[i], sTnsAdmin, ''), sTnsNames);
              break;
            end;
        /// Look 'Global' TNS_ADMIN variable in registry
        if (FileName = sTnsNames) or (FileName = '') then
          FileName := AddPath(RegIniFile.ReadString(sOraHomeKey, sTnsAdmin, ''), sTnsNames);
      finally
        RegIniFile.Free;
      end;

      if FileExists(FileName) then
        TNSFound := True;
    end;
  {$ENDIF}

    if not TNSFound then begin
      /// look for tnsnames.ora in Oracle Home
      if FHomeName = '' then
        HomePath := OracleHomePath
      else
        for i := 0 to OracleHomeCount - 1 do
          if AnsiCompareText(OracleHomeNames[i] , FHomeName) = 0 then begin
            HomePath := OracleHomePaths[i];
            break;
          end;

      if HomePath <> '' then begin
        FileName := HomePath + sNetwork + sTnsNames;
        if FileExists(FileName) then
          TNSFound := True
        else begin
        {$IFNDEF LINUX}
          FileName := HomePath + sNet80 + sTnsNames;
          if FileExists(FileName) then
            TNSFound := True
        {$ENDIF}
        end;
      end;
    end;
  end;

  if TNSFound then
    Result := FileName;
end;

procedure TOraServerEnumerator.GetServerList(List: _TStrings);
var
  i, j: integer;
  IFile, F: TStrings;
  Parser: TTNSParser;
  Code: integer;
  FileName, CurrentDir, St, OldSt, Alias: _string;
  Bracket: integer;
begin
  inherited;

  if FDirect then
    exit;

  FileName := GetTNSFileName;
  if FileName = '' then
    exit;

  F := TStringList.Create;
  Parser := TTNSParser.Create('');
  try
    Parser.OmitBlank := True;
    Parser.OmitComment := True;
    F.LoadFromFile(FileName);

    IFile := TStringList.Create;
    i := 0;
    CurrentDir := GetCurrentDir;
    SetCurrentDir(ExtractFileDir(FileName));
    try
      while i < F.Count do begin
        if Pos('IFILE', AnsiUpperCase(F[i])) > 0 then begin
          FileName := Trim(Copy(F[i], Pos('=',F[i]) + 1, MaxInt));
          if FileExists(FileName) then begin
            IFile.LoadFromFile(FileName);
            F.Delete(i);
            for j := IFile.Count - 1 downto 0 do
               F.Insert(i, IFile[j]);
            Continue;
          end;
        end;
        Inc(i);
      end;
    finally
      SetCurrentDir(CurrentDir);
      IFile.Free;
    end;

    Parser.SetText(F.Text);
    Code := 0;
    St := '';
    Alias := '';
    Bracket := 0;
    List.Clear;
    repeat
      if (Bracket = 0) and ((Code = lcIdent) or (Code = lcNumber) or (St = '-')) then
        if OldSt = '.' then begin
          if (_UpperCase(St) <> 'WORLD') or (OCIVersion > 8100) then
            Alias := Alias + '.' + St
        end
        else
          Alias := Alias + St;

      OldSt := St;

      Code := Parser.GetNext(St);
      if St = '(' then
        Inc(Bracket)
      else
        if St = ')' then
          Dec(Bracket)
        else
          if ((St = '=') or (St = ',')) and (Bracket = 0) then begin
            List.Add(Alias);
            Alias := '';
          end;
    until Code = lcEnd;
  finally
    Parser.Free;
    F.Free;
  end;
end;

function TOraServerEnumerator.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prHomeName:
      Value := FHomeName;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TOraServerEnumerator.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDirect:
      FDirect := Value;
    prHomeName:
      FHomeName := Value;
    prTNSPath:
      FTNSPath := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

end.

