
{******************************************}
{                                          }
{             FastReport v4.0              }
{      UniDAC components registration      }
{                                          }

// Created by: Devart
// E-mail: unidac@devart.com

{                                          }
{******************************************}

unit frxUniDACReg;

interface

{$I frx.inc}

procedure Register;

implementation

uses
  Windows, Messages, SysUtils, Classes
{$IFNDEF Delphi6}
, DsgnIntf
{$ELSE}
, DesignIntf, DesignEditors
{$ENDIF}
, frxUniDACComponents;

procedure Register;
begin
  RegisterComponents('FastReport 4.0', [TfrxUniDACComponents]);
end;

end.
