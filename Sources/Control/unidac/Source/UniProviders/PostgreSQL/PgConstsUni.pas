
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgConstsUni;

{$ENDIF}

interface

resourcestring
  // Protocol
  sUnexpectedServerResponse       = 'Unexpected server response';
  sUnknownAuthMethod              = 'Unknown auth method';
  sUnsupportedAuthMethod          = 'Unsupported auth method';
  SStoredProcNotFound             = 'Stored procedure "%s"."%s" not found';
  SInvalidFieldType               = 'Invalid field type';
  SInvalidInputSyntax             = 'Invalid input syntax for type %s';
  SInvalidOutParamDataType        = 'DataType of %s output parameter does not correspond to the type returned by query';
  SRowTypeNotSet                  = 'Row type not set';
  SBlobNeedTransaction            = 'Active transaction is required for operations with large objects';
  SRefCursorNeedTransaction       = 'Active transaction is required for operations with REFCURSOR';
  SFetchAllFalseNeedTransaction   = 'Active transaction is required for using FetchAll = False mode';
  SConnectionOpen                 = 'Cannot perform this operation on an open connection';
  SInvalidParams                  = 'Invalid parameters specified';

implementation

end.
