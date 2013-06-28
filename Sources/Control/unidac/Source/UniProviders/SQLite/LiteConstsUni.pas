
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright (c) 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I SQLiteDac.inc}
unit LiteConstsUni;
{$ENDIF}

interface

resourcestring
  SStoredProcNotSupported = 'SQLite does not support stored procedures';
  SClientLibraryDiffers = 'Connection ClientLibrary differs from already active connections';
  SIncorrectParamCount = 'Incorrect parameter count. Expected: %d; Actual: %d';
  SIncorrectConnectionType = 'Incorrect connection type. Expected: %s; Actual: %s';

implementation

end.
