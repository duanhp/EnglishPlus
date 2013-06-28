
//////////////////////////////////////////////////
//  Advantage Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ADSDac.inc}
unit ADSServicesUni;
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
  ODBCClasses, ODBCParser, ODBCCall, ODBCServices, ADSClasses;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCCallUni, ODBCServicesUni, ADSClassesUni;
{$ENDIF}

type

{ TCustomADSSQLGenerator }

  TCustomADSSQLGenerator = class(TDASQLGenerator)
  public
    function GenerateSelectValues(const ValuesList: _string): _string; override;
  end;

{  TCustomADSDataSetUpdater }

  TCustomADSDataSetUpdater = class(TCustomODBCDataSetUpdater)
  protected
    function GetIdentityFieldValue(var Value: variant): boolean; override;
  end;

{ TCustomADSDataSetService }

  TCustomADSDataSetService = class(TCustomODBCDataSetService)
  protected
    procedure CreateDataSetUpdater; override;
    procedure CreateSQLGenerator; override;
    function DetectIdentityField: TCRFieldDesc; override;
    function IdentityFieldIsData: boolean; override;
  end;

{ TADSScriptProcessor }

  TADSScriptProcessor = class (TODBCScriptProcessor)
  protected
    procedure CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean); override;
  end;

implementation

uses
  DAConsts;

{ TCustomADSSQLGenerator }

function TCustomADSSQLGenerator.GenerateSelectValues(const ValuesList: _string): _string;
begin
  Result := 'SELECT ' + ValuesList + ' FROM system.iota';
end;

{ TCustomADSDataSetUpdater }

function TCustomADSDataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
  Value := SelectDBValue('Get identity value', 'SELECT LASTAUTOINC(CONNECTION) FROM system.iota');
  Result := True;
end;

{ TCustomADSDataSetService }

procedure TCustomADSDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomADSDataSetUpdater.Create(Self));
end;

procedure TCustomADSDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomADSSQLGenerator.Create(Self));
end;

function TCustomADSDataSetService.DetectIdentityField: TCRFieldDesc;
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

function TCustomADSDataSetService.IdentityFieldIsData: boolean;
begin
  Result := True;
end;

{ TADSScriptProcessor }

procedure TADSScriptProcessor.CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean);
begin
  if Code = lxBEGIN then
    StatementType := ST_SPECIFIC_SQL;
end;

end.
