
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright (c) 1998-2011 Devart. All right reserved.
//  InterBase constants
//////////////////////////////////////////////////

{$IFNDEF CLR}
unit IBCConstsUni;
{$ENDIF}

interface

resourcestring

  SNoRows                   = 'SQL statement doesn''t return rows';
  SCursorNotOpened          = 'Cursor must be opened';
  SCantExecuteNonSPStatement = 'Can''t execute non stored proc statement';
  SCantExecuteNonSelectStatement = 'Can''t execute statement that doesn''t return dataset';
  SKeyFieldNotFound         = '%s: Key field ''%s'' not found';
  SBadUpdatingTable         = 'Wrong UpdatingTable value - table %s is unknown';
  SDatabaseNameMissing      = 'Database name missing';
  STransAreDifferent        = 'DataSets use different transactions';
  SNotInCachedUpdate        = 'DataSet is not active or not in CachedUpdates mode';
  SBlobNotAllocatted        = 'BLOB is not allocated';
  SCantReadEmptyBlobID      = 'Invalid BLOB ID';
  SCantSetReadPosition      = 'Can''t set BLOB read position';
  SCannotChangeStreamed     = 'Can''t change Streamed mode with non-cached BLOB';
  SCannotDisableBlobCache   = 'Can''t disable BLOB cache';
  SCannotCompressNotCached  = 'Can''t compress non-cached BLOB';
  SParamIsNotStorable       = 'Parameter cannot be stored';
  SSQLInfoError             = 'SQL information exception';
  SWrongSQLType             = 'Unsupported SQL type';
  SEmptySQL                 = 'SQL statement cannot be empty';
  SDPBConstantNotSupported  = 'Unsupported DPB constant (isc_dpb_%s)';
  SDPBConstantUnknown       = 'Unknown DPB constant (%s)';
  STPBConstantNotSupported  = 'Unsupported TPB constant (isc_tpb_%s)';
  STPBConstantUnknown       = 'Unknown TPB constant (%s)';
  SConnectionClosed         = 'Can''t perform operation on disconnected connection';
  SConnectionOpen           = 'Can''t perform operation on connected connection';
  SConnClosedOnTrStart      = 'Can''t start transaction with closed connection';
  SCantEndSharedTransaction = 'Can''t finish a shared transaction';
  SEventNameTooLong         = 'Event name is too long. Maximum event name length is %d';
  SSendEventNotSupported    = 'Sending events is supported starting with Firebird 2';
  SClientLibraryDiffers     = 'Connection ClientLibrary differs from already active connections';

  SVarIsNotArray            = 'Variant does not contain an array';
  SVarArrayBounds           = 'Variant array bounds error';
  SInvalidTable             = 'Invalid table name';
  SInvalidColumn            = 'Invalid column name';
  SCannotDisableArrayCache  = 'Can''t disable array cache';

  SArrayDimensionError      = 'Arrays dimesions are different';  
  SArrayLowBoundError       = 'Array low bound out of bounds (%d)';
  SArrayHighBoundError      = 'Array high bound out of bounds (%d)';
  SArrayIndexError          = 'Array index out of bounds (%d)';

  SInvalidDimension         = 'Array dimension is not valid';

  SSPBConstantNotSupported  = 'Unsupported SPB constant (isc_spb_%s)';
  SSPBConstantUnknown       = 'Unknown SPB constant (%s)';
  SServerNameMissing        = 'Server name missing';
  SServiceActive            = 'Cann''t perform operation - service is not attached';
  SServiceInActive          = 'Cann''t perform operation - service is attached';
  SQueryParamsError         = 'Query parameters missing or incorrect';
  SStartParamsError         = 'Start parameters missing or incorrect';
  SOutputParsingError       = 'Unexpected Output buffer value';
  SNoVersionInfo            = 'Version infomation for this server is not retreived';
  SUseSpecificProcedures    = 'Generic ServiceStart not applicable: use specific procedures to set configuration params';
  SIB75feature              = 'This is an InterBase 7.5 function. Please upgrade to InterBase 7.5 to use this functonality';
  SBackupFileNotSpecified   = 'Backup file is not specified';

implementation

end.
