{******************************************}
{                                          }
{             FastReport v4.0              }
{         UniDAC components RTTI           }
{                                          }

// Created by: Devart
// E-mail: unidac@devart.com

{                                          }
{******************************************}

unit frxUniDACRTTI;

interface

{$I frx.inc}

implementation

uses
  Windows, Classes, fs_iinterpreter, frxUniDACComponents
{$IFDEF Delphi6}
, Variants
{$ENDIF};


type
  TFunctions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

{ TFunctions }

constructor TFunctions.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  with AScript do
  begin
    AddClass(TfrxUniDACDatabase, 'TfrxCustomDatabase');
    AddClass(TfrxUniDACTable, 'TfrxCustomTable');
    with AddClass(TfrxUniDACQuery, 'TfrxCustomQuery') do
      AddMethod('procedure ExecSQL', CallMethod);
  end;
end;

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  Result := 0;
  if ClassType = TfrxUniDACQuery then
  begin
    if MethodName = 'EXECSQL' then
      TfrxUniDACQuery(Instance).Query.Execute;
  end
end;

initialization
  fsRTTIModules.Add(TFunctions);

finalization
  if fsRTTIModules <> nil then
    fsRTTIModules.Remove(TFunctions);

end.
