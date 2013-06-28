
{$I UniDac.inc}

unit UniDacClx;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
  SysUtils, Classes, DacClx, DBAccess, UniConnectForm, Uni, CRParser, DB;

{$I UniDacGui.inc}
