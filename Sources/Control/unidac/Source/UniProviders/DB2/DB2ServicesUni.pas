
//////////////////////////////////////////////////
//  DB2 Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I DB2Dac.inc}
unit DB2ServicesUni;
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
  ODBCClasses, ODBCParser, ODBCCall, ODBCServices, DB2Classes;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCCallUni, ODBCServicesUni, DB2ClassesUni;
{$ENDIF}

const
  prKeySequence        = 101; // string
  prSequenceMode       = 102; // integer

type
  TDB2SequenceMode = (smInsert, smPost);

  TCustomDB2DataSetService = class;

{ TCustomDB2SQLGenerator }

  TCustomDB2SQLGenerator = class(TDASQLGenerator)
  protected
    function GenerateIndexName(Name: _string): _string; override;
    function DecodeFieldIndex(FieldName: _string): integer; override;

  public
    function GenerateSelectValues(const ValuesList: _string): _string; override;
  end;

{  TCustomDB2DataSetUpdater }

  TCustomDB2DataSetUpdater = class(TCustomODBCDataSetUpdater)
  private
    procedure GetSequenceNextVal;

  protected
    FDataSetService: TCustomDB2DataSetService;

    function IsNeedInsertPreconnect: boolean; override;
    procedure PrepareAppend; override;
    function PerformAppend: boolean; override;
    function GetIdentityFieldValue(var Value: variant): boolean; override;

  public
    constructor Create(AOwner: TDataSetService); override;
  end;

{ TCustomDB2DataSetService }

  TCustomDB2DataSetService = class(TCustomODBCDataSetService)
  private
    FKeySequence: _string;
    FSequenceMode: TDB2SequenceMode;

  protected
    procedure CreateDataSetUpdater; override;
    procedure CreateSQLGenerator; override;
    function GetCurrentSchema: _string; override;
    function DetectIdentityField: TCRFieldDesc; override;
    function IdentityFieldIsData: boolean; override;
    function DetectKeyGeneratorField: TField; override;

  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

{ TDB2ScriptProcessor }

  TDB2ScriptProcessor = class (TODBCScriptProcessor)
  protected
    procedure CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean); override;
  end;

{ TDB2ServerEnumerator }

  TDB2ServerEnumerator = class (TCRServerEnumerator)
  public
    procedure GetServerList(List: _TStrings); override;
  end;

implementation

uses
  DAConsts;

{ TCustomDB2SQLGenerator }

function TCustomDB2SQLGenerator.GenerateSelectValues(const ValuesList: _string): _string;
begin
  Result := 'VALUES (' + ValuesList + ')';
end;

function TCustomDB2SQLGenerator.GenerateIndexName(Name: _string): _string;
begin
  Result := 'F__' + Name;
end;

function TCustomDB2SQLGenerator.DecodeFieldIndex(FieldName: _string): integer;
var
  e: integer;
begin
  Result := -1;
  if (Length(FieldName) >= 4) and (Copy(FieldName, 1, 3) = 'F__') then begin
    Val(Copy(FieldName, 4, MaxInt), Result, e);
    if e <> 0 then
      Result := -1;
  end;
end;

{ TCustomDB2DataSetUpdater }

constructor TCustomDB2DataSetUpdater.Create(AOwner: TDataSetService);
begin
  FDataSetService := TCustomDB2DataSetService(AOwner);

  inherited Create(AOwner);
end;

function TCustomDB2DataSetUpdater.IsNeedInsertPreconnect: boolean;
begin
  Result := (FDataSetService.FSequenceMode = smInsert);
end;

procedure TCustomDB2DataSetUpdater.PrepareAppend;
begin
  if (FDataSetService.KeyGeneratorField <> nil) and (FDataSetService.FSequenceMode = smInsert) then
    GetSequenceNextVal;
end;

function TCustomDB2DataSetUpdater.PerformAppend: boolean;
begin
  if (FDataSetService.KeyGeneratorField <> nil) and (FDataSetService.FSequenceMode = smPost) then
    GetSequenceNextVal;

  Result := inherited PerformAppend;
end;

function TCustomDB2DataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
  Value := SelectDBValue('Get identity value', 'VALUES (IDENTITY_VAL_LOCAL())');
  Result := True;
end;

procedure TCustomDB2DataSetUpdater.GetSequenceNextVal;
begin
  Assert(FDataSetService.KeyGeneratorField <> nil);
  FDataSetService.KeyGeneratorField.NewValue := SelectDBValue('Get sequence value',
    'VALUES (NEXT VALUE FOR ' + FDataSetService.FKeySequence + ')');
end;

{ TCustomDB2DataSetService }

function TCustomDB2DataSetService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prKeySequence:
      FKeySequence := Value;
    prSequenceMode:
      FSequenceMode := TDB2SequenceMode(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TCustomDB2DataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomDB2DataSetUpdater.Create(Self));
end;

procedure TCustomDB2DataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomDB2SQLGenerator.Create(Self));
end;

function TCustomDB2DataSetService.GetCurrentSchema: _string;
begin
  Result := TDB2Connection(TDBAccessUtils.GetIConnection(UsedConnection)).GetCachedSchema;
  // to preserve character case
  Result := '"' + Result + '"';
end;

function TCustomDB2DataSetService.DetectIdentityField: TCRFieldDesc;
var
  i: integer;
  FieldDesc: TCRFieldDesc;
  RecordSet: TCRRecordSet;
begin
  RecordSet := GetIRecordSet;
  for i := 0 to RecordSet.FieldCount - 1 do begin
    FieldDesc := TCRFieldDesc(RecordSet.Fields[i]);
    if FieldDesc.IsAutoIncrement and (FieldDesc.TableInfo = UpdatingTableInfo) then begin
      Result := FieldDesc;
      exit;
    end;
  end;
  Result := nil;
end;

function TCustomDB2DataSetService.IdentityFieldIsData: boolean;
begin
  Result := True;
end;

function TCustomDB2DataSetService.DetectKeyGeneratorField: TField;
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

{ TDB2ScriptProcessor }

procedure TDB2ScriptProcessor.CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean);
begin
  if Code = lxBEGIN then
    StatementType := ST_SPECIFIC_SQL;
end;

{ TDB2ServerEnumerator }

procedure TDB2ServerEnumerator.GetServerList(List: _TStrings);
begin
  List.Clear;
end;

end.
