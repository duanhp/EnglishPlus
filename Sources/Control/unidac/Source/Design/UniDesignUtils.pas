{$IFNDEF CLR}
{$I UniDac.inc}

unit UniDesignUtils;
{$ENDIF}

interface

uses
  Classes, SysUtils, MemUtils, DBAccess, DADesignUtils;

type
  TUniDesignUtils = class(TDADesignUtils)
    class function GetProjectName: string; override;

  { Component }
  {$IFDEF UNIDAC}
    class function GetConnectionList: TObject; override;
  {$ENDIF}

  { TDATable support }
    class function GetTableName(Obj: TCustomDADAtaSet): _string; override;
    class procedure SetTableName(Obj: TCustomDADAtaSet; const Value: _string); override;
    class function GetOrderFields(Obj: TCustomDADAtaSet): _string; override;
    class procedure SetOrderFields(Obj: TCustomDADAtaSet; const Value: _string); override;
    class procedure PrepareSQL(Obj: TCustomDADAtaSet); override;

  { TDAStoredProc support}
    class function GetStoredProcName(Obj: TCustomDADataSet): _string; override;
    class procedure SetStoredProcName(Obj: TCustomDADataSet; const Value: _string); override;
  end;

implementation

uses
{$IFDEF UNIDAC}
  UniDesign,
{$ENDIF}
  Uni;

{ TUniDesignUtils }

class function TUniDesignUtils.GetProjectName: string;
begin
  Result := 'UniDAC';
end;

class function TUniDesignUtils.GetOrderFields(
  Obj: TCustomDADAtaSet): _string;
begin
  Assert(Obj is TUniTable, Obj.ClassName);
  Result := TUniTable(Obj).OrderFields;
end;

class procedure TUniDesignUtils.SetOrderFields(Obj: TCustomDADAtaSet;
  const Value: _string);
begin
  Assert(Obj is TUniTable, Obj.ClassName);
  TUniTable(Obj).OrderFields := Value;
end;

class function TUniDesignUtils.GetTableName(Obj: TCustomDADAtaSet): _string;
begin
  Assert(Obj is TUniTable, Obj.ClassName);
  Result := TUniTable(Obj).TableName;
end;

class procedure TUniDesignUtils.SetTableName(Obj: TCustomDADAtaSet;
  const Value: _string);
begin
  Assert(Obj is TUniTable, Obj.ClassName);
  TUniTable(Obj).TableName := Value;
end;

class procedure TUniDesignUtils.PrepareSQL(Obj: TCustomDADAtaSet);
begin
  Assert(Obj is TUniTable, Obj.ClassName);
  TUniTable(Obj).PrepareSQL;
end;

{$IFDEF UNIDAC}
class function TUniDesignUtils.GetConnectionList: TObject;
begin
  Result := TUniConnectionList.Create;
end;
{$ENDIF}

class function TUniDesignUtils.GetStoredProcName(Obj: TCustomDADataSet): _string;
begin
  Assert(Obj is TUniStoredProc, Obj.ClassName);
  Result := TUniStoredProc(Obj).StoredProcName;
end;

class procedure TUniDesignUtils.SetStoredProcName(Obj: TCustomDADataSet; const Value: _string);
begin
  Assert(Obj is TUniStoredProc, Obj.ClassName);
  TUniStoredProc(Obj).StoredProcName := Value;
end;

end.
