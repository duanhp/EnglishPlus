
{$J+}

{$IFNDEF CLR}

{$I Dac.inc}

unit DacVcl;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
  Classes, DB, DBAccess, DASQLMonitor, TypInfo, MemData, Forms,
  Controls, StdCtrls, ExtCtrls, Graphics, SysUtils;

{$I DacGui.inc}

