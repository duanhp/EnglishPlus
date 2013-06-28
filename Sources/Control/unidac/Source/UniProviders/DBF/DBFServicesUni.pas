
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I DBFDac.inc}
unit DBFServicesUni;
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
  ODBCClasses, ODBCParser, ODBCCall, ODBCServices, DBFClasses;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCCallUni, ODBCServicesUni, DBFClassesUni;
{$ENDIF}

type

{  TCustomDBFDataSetUpdater }

  TCustomDBFDataSetUpdater = class(TCustomODBCDataSetUpdater)
  protected
    function GetIdentityFieldValue(var Value: variant): boolean; override;
  end;

{ TCustomDBFDataSetService }

  TCustomDBFDataSetService = class(TCustomODBCDataSetService)
  protected
    procedure CreateDataSetUpdater; override;
    function DetectIdentityField: TCRFieldDesc; override;
    function IdentityFieldIsData: boolean; override;
  end;

implementation

{ TCustomDBFDataSetUpdater }

function TCustomDBFDataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
  Value := SelectDBValue('Get identity value', 'SELECT @@IDENTITY');
  Result := True;
end;

{ TCustomDBFDataSetService }

procedure TCustomDBFDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomDBFDataSetUpdater.Create(Self));
end;

function TCustomDBFDataSetService.DetectIdentityField: TCRFieldDesc;
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

function TCustomDBFDataSetService.IdentityFieldIsData: boolean;
begin
  Result := True;
end;

end.
