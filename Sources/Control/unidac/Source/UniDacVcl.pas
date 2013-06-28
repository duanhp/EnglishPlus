{$IFNDEF CLR}
{$I UniDac.inc}
unit UniDacVcl;
{$ENDIF}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
  SysUtils, Classes, DB, CRParser, DBAccess, DacVcl, UniProvider,
  Uni, UniConnectForm;

{$I UniDacGui.inc}

