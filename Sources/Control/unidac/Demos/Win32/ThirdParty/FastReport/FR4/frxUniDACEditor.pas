
{******************************************}
{                                          }
{             FastReport v4.0              }
{     UniDAC components design editors     }
{                                          }

// Created by: Devart
// E-mail: unidac@devart.com

{                                          }
{******************************************}

unit frxUniDACEditor;

interface

{$I frx.inc}

implementation

uses
  Windows, Classes, SysUtils, Forms, Dialogs, frxUniDACComponents, frxCustomDB,
  frxDsgnIntf, frxRes, Uni, UniProvider, frxDACEditor
{$IFDEF Delphi6}
, Variants
{$ENDIF};


type
  TfrxUniDatabaseProperty = class(TfrxComponentProperty)
  public
    function GetValue: String; override;
  end;

  TfrxUniTableNameProperty = class(TfrxTableNameProperty)
  public
    procedure GetValues; override;
  end;

  TfrxProviderNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure SetValue(const Value: String); override;
    procedure GetValues; override;
  end;

{ TfrxUniDatabaseProperty }

function TfrxUniDatabaseProperty.GetValue: String;
var
  db: TfrxUniDACDatabase;
begin
  db := TfrxUniDACDatabase(GetOrdValue);
  if db = nil then begin
    if (UniDACComponents <> nil) and (UniDACComponents.DefaultDatabase <> nil) then
      Result := UniDACComponents.DefaultDatabase.Name
    else
      Result := frxResources.Get('prNotAssigned');
  end
  else
    Result := inherited GetValue;
end;
  
{ TfrxUniTableNameProperty }

procedure TfrxUniTableNameProperty.GetValues;
begin
  inherited;
  with TfrxUniDACTable(Component).Table do
    if Connection <> nil then
      Connection.GetTableNames(Values);
end;

{ TfrxProviderNameProperty }

function TfrxProviderNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList];
end;

procedure TfrxProviderNameProperty.SetValue(const Value: String);
begin
  inherited;
  Designer.UpdateDataTree;
end;

procedure TfrxProviderNameProperty.GetValues;
begin
  inherited;
  UniProviders.GetProviderNames(Values);
end;

initialization
  frxPropertyEditors.Register(TypeInfo(TfrxUniDACDatabase), TfrxUniDACTable, 'Database',
    TfrxUniDatabaseProperty);
  frxPropertyEditors.Register(TypeInfo(TfrxUniDACDatabase), TfrxUniDACQuery, 'Database',
    TfrxUniDatabaseProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxUniDACTable, 'TableName',
    TfrxUniTableNameProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxUniDACDatabase, 'ProviderName',
    TfrxProviderNameProperty);

end.