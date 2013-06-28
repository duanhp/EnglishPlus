
{******************************************}
{                                          }
{             FastReport v4.0              }
{      DAC components design editors       }
{                                          }

// Created by: Devart
// E-mail: support@devart.com

{                                          }
{******************************************}

unit frxDACEditor;

interface

{$I frx.inc}

uses
  Windows, Classes, SysUtils, Forms, Dialogs, frxDACComponents, frxCustomDB,
  frxDsgnIntf, frxRes
{$IFDEF Delphi6}
, Variants
{$ENDIF};


type
  TfrxTableNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure SetValue(const Value: String); override;
  end;

implementation

{ TfrxTableNameProperty }

function TfrxTableNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList];
end;

procedure TfrxTableNameProperty.SetValue(const Value: String);
begin
  inherited;
  Designer.UpdateDataTree;
end;

end.
