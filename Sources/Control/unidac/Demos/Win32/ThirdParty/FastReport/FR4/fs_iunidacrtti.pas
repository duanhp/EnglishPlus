
{******************************************}
{                                          }
{             FastScript v1.9              }
{       UniDAC classes and functions       }
{                                          }
{          Created by: Devart              }
{        E-mail: unidac@devart.com         }
{                                          }
{******************************************}

unit fs_iunidacrtti;

interface

{$i fs.inc}

uses
  SysUtils, Classes, fs_iinterpreter, fs_itools, fs_idbrtti, fs_idacrtti, DB,
  Uni;

type
  TfsUniDACRTTI = class(TComponent); // fake component

implementation

type
  TFunctions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetProp(Instance: TObject; ClassType: TClass;
      const PropName: String): Variant;
    procedure SetProp(Instance: TObject; ClassType: TClass;
      const PropName: String; Value: Variant);
  public
    constructor Create(AScript: TfsScript); override;
  end;

{ TFunctions }

constructor TFunctions.Create(AScript: TfsScript);
begin
  inherited Create(AScript);

  with AScript do begin
    with AddClass(TUniConnection, 'TCustomDAConnection') do begin
      AddMethod('procedure Savepoint(const Name: string)', CallMethod);
      AddMethod('procedure RollbackToSavepoint(const Name: string)', CallMethod);
      AddMethod('procedure ReleaseSavepoint(const Name: string)', CallMethod);
      AddMethod('function ExecProc(Name: string; const Params: array of variant): variant', CallMethod);
      AddMethod('function ExecSQLEx(Text: string; const Params: array of variant): variant', CallMethod);
      AddMethod('function ExecProcEx(Name: string; const Params: array of variant): variant', CallMethod);

      AddProperty('ServerVersion', 'string', GetProp);
      AddProperty('ServerVersionFull', 'string', GetProp);
      AddProperty('ClientVersion', 'string', GetProp);
    end;

    AddClass(TUniConnectionOptions, 'TDAConnectionOptions');

    with AddClass(TCustomUniDataSet, 'TCustomDADataSet') do begin
      AddMethod('procedure Lock', CallMethod);
      AddMethod('procedure Unlock', CallMethod);
      AddMethod('function OpenNext: boolean', CallMethod);
      AddMethod('procedure RefreshQuick(const CheckDeleted: boolean)', CallMethod);
      AddMethod('function FindParam(const Value: string): TUniParam', CallMethod);
      AddMethod('function ParamByName(const Value: string): TUniParam', CallMethod);
    end;

    AddClass(TUniParam, 'TDAParam');

    with AddClass(TUniParams, 'TDAParams') do begin
      AddMethod('function FindParam(const Value: string): TUniParam', CallMethod);
      AddMethod('function ParamByName(const Value: string): TUniParam', CallMethod);
    end;

    AddEnum('TLockMode', 'lmNone, lmPessimistic, lmOptimistic');

    AddClass(TUniDataSetOptions, 'TDADataSetOptions');
    AddClass(TUniQuery, 'TCustomUniDataSet');

    with AddClass(TUniTable, 'TCustomUniDataSet') do begin
      AddMethod('procedure PrepareSQL', CallMethod);

      AddProperty('TableName', 'string', GetProp, SetProp);
      AddProperty('OrderFields', 'string', GetProp, SetProp);
    end;

    with AddClass(TUniStoredProc, 'TCustomUniDataSet') do begin
      AddMethod('procedure ExecProc', CallMethod);
      AddMethod('procedure PrepareSQL(IsQuery: boolean = False)', CallMethod);

      AddProperty('StoredProcName', 'string', GetProp, SetProp);
    end;
  end;
end;

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  Result := 0;

  if ClassType = TUniConnection then begin
    if MethodName = 'SAVEPOINT' then
      TUniConnection(Instance).Savepoint(Caller.Params[0])
    else
    if MethodName = 'ROLLBACKTOSAVEPOINT' then
      TUniConnection(Instance).RollbackToSavepoint(Caller.Params[0])
    else
    if MethodName = 'RELEASESAVEPOINT' then
      TUniConnection(Instance).ReleaseSavepoint(Caller.Params[0])
    else
    if MethodName = 'EXECPROC' then
      Result := TUniConnection(Instance).ExecProc(Caller.Params[0], [Caller.Params[1]])
    else
    if MethodName = 'EXECSQLEX' then
      Result := TUniConnection(Instance).ExecSQLEx(Caller.Params[0], [Caller.Params[1]])
    else
    if MethodName = 'EXECPROCEX' then
      Result := TUniConnection(Instance).ExecProcEx(Caller.Params[0], [Caller.Params[1]]);
  end
  else
  if ClassType = TCustomUniDataSet then begin
    if MethodName = 'LOCK' then
      TCustomUniDataSet(Instance).Lock
    else
    if MethodName = 'UNLOCK' then
      TCustomUniDataSet(Instance).Unlock
    else
    if MethodName = 'OPENNEXT' then
      Result := TCustomUniDataSet(Instance).OpenNext
    else
    if MethodName = 'REFRESHQUICK' then
      TCustomUniDataSet(Instance).RefreshQuick(Caller.Params[0])
    else
    if MethodName = 'FINDPARAM' then
      Result := Integer(Pointer(TCustomUniDataSet(Instance).FindParam(Caller.Params[0])))
    else
    if MethodName = 'PARAMBYNAME' then
      Result := Integer(Pointer(TCustomUniDataSet(Instance).ParamByName(Caller.Params[0])));
  end
  else
  if ClassType = TUniParams then begin
    if MethodName = 'FINDPARAM' then
      Result := Integer(Pointer(TUniParams(Instance).FindParam(Caller.Params[0])))
    else
    if MethodName = 'PARAMBYNAME' then
      Result := Integer(Pointer(TUniParams(Instance).ParamByName(Caller.Params[0])));
  end
  else
  if ClassType = TUniTable then begin
    if MethodName = 'PREPARESQL' then
      TUniTable(Instance).PrepareSQL;
  end
  else
  if ClassType = TUniStoredProc then begin
    if MethodName = 'PREPARESQL' then
      TUniStoredProc(Instance).PrepareSQL(Caller.Params[0])
    else
    if MethodName = 'EXECPROC' then
      TUniStoredProc(Instance).ExecProc;
  end;
end;

function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;

  if ClassType = TUniConnection then begin
    if PropName = 'CLIENTVERSION' then
      Result := TUniConnection(Instance).ClientVersion
    else
    if PropName = 'SERVERVERSION' then
      Result := TUniConnection(Instance).ServerVersion
    else
    if PropName = 'SERVERVERSIONFULL' then
      Result := TUniConnection(Instance).ServerVersionFull;
  end
  else
  if ClassType = TUniTable then begin
    if PropName = 'TABLENAME' then
      Result := TUniTable(Instance).TableName
    else
    if PropName = 'ORDERFIELDS' then
      Result := TUniTable(Instance).OrderFields;
  end
  else
  if ClassType = TUniStoredProc then begin
    if PropName = 'STOREDPROCNAME' then
      Result := TUniStoredProc(Instance).StoredProcName;
  end;
end;

procedure TFunctions.SetProp(Instance: TObject; ClassType: TClass;
  const PropName: String; Value: Variant);
begin
  if ClassType = TUniTable then begin
    if PropName = 'TABLENAME' then
      TUniTable(Instance).TableName := Value
    else
    if PropName = 'ORDERFIELDS' then
      TUniTable(Instance).OrderFields := Value;
  end
  else
  if ClassType = TUniStoredProc then begin
    if PropName = 'STOREDPROCNAME' then
      TUniStoredProc(Instance).StoredProcName := Value;
  end;
end;

initialization
  fsRTTIModules.Add(TFunctions);

finalization
  if fsRTTIModules <> nil then
    fsRTTIModules.Remove(TFunctions);

end.

