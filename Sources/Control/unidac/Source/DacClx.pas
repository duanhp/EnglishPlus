
{$I Dac.inc}
{$J+}

unit DacClx;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Registry, SysUtils,
{$ENDIF}
  Classes, DB, DBAccess, DASQLMonitor, TypInfo, MemData, QForms, QControls,
  QStdCtrls, QExtCtrls, QGraphics;

{$I DacGui.inc}

