
{******************************************}
{                                          }
{             FastScript v1.9              }
{        UniDAC Registration unit          }
{                                          }
{          Created by: Devart              }
{        E-mail: unidac@devart.com         }
{                                          }
{******************************************}

unit fs_iunidacreg;

{$i fs.inc}

interface


procedure Register;

implementation

uses
  Classes
{$IFNDEF Delphi6}
, DsgnIntf
{$ELSE}
, DesignIntf
{$ENDIF}
, fs_iunidacrtti;

{-----------------------------------------------------------------------}

procedure Register;
begin
  RegisterComponents('FastScript', [TfsUniDACRTTI]);
end;

end.
