{$IFNDEF CLR}
{$I MyDac.inc}
unit MyConstsUni;
{$ENDIF}

interface

const
  DefaultCommandTimeout = 0; // infinite

const
  KeyPath = '\SOFTWARE\Devart\MyDAC\Editors';
  FontKeyPath = '\SOFTWARE\Devart\SQLDesigner';

resourcestring
  SFieldNameNotDefined    = 'Field name must be defined';
  sMultipleFieldsFound    = 'More than one field found';
  SEmptySQLStatement      = 'No SQL statement provided';
  SKeyFieldNotFound       = '%s: Key field ''%s'' not found';
  SBadStatementType       = 'Bad statement type';
  SBadSQLObjectName       = 'SQL Object name must be provided';
  SNoResultSet            = 'Query must return exactly result set - use Execute';
  SDataDirNotFound        = 'Path to the folder to store data %s not found';
  SBaseDirNotFound        = 'Path to the MySQL basedir %s not found';
  SErrMsgNotFound         = 'File %s not found';
  SEServInitErr           = 'A call to an mysql_server_init failed';
  SConnectionOpen         = 'Cannot perform this operation on an open connection';
  SEmptyTableNames        = 'No TableNames provided';
  SAreYouSureRestore      = 'Are you sure to restore data?';
  SAreYouSureKill         = 'Are you sure to kill process?';
  SOpenNextPreparedSQL    = 'Cannot OpenNext if statement is Prepared';
  SOpenNextVsFetchAll     = 'OpenNext is not compatible with FetchAll = True';
  STimestampFieldRequired = 'Timestamp field required';

  SPrepareNotSupportedC   = 'Prepare/UnPrepare is not supported by this MySQL client version';
  SPrepareNotSupportedS   = 'Prepare/UnPrepare is not supported by this MySQL server version';
  SLockTableVsFetchAll    = 'LockTable is not compatible with FetchAll = False';
  SNoRecordToLock         = 'There is no active record to lock';
  SLockVsFetchAll         = 'Lock is not compatible with FetchAll = False';
  SLockWOSelect           = 'Lock can be used only with queries returning resultset';
  SMustBeInTransaction    = 'Lock cannot be called outside transaction context';
  SDateEncodeError        = 'Invalid argument to date encode';
  SMyBuilderNA            = 'MySQLBuilder is not available';

  SParamNameUnknown       = 'Parameter name is unknown - %s';
  SBadParamValue          = 'Bad parameter value - %s=%s';
  SParamNameMissing       = 'Parameter name missing';
  SParamValueMissing      = 'Parameter value missing';
  SInvalidChar            = 'Invalid character in non-quoted parameter value';

  SInvalidClientVersion   = 'Invalid client version';
  SInvalidUnComprBlobSize = 'Invalid uncompressed blob size';
  SInvalidComprBlobSize   = 'Invalid compressed blob size';
  SInvalidComprBlobHeader = 'Invalid compressed blob header';
  SInvalidComprBlobData   = 'Invalid compressed blob data';
  STableMsgEvent          = '%s %s - %s';

  sMultipleVariablesFound = 'More than one variable found';
  sNoVariablesFound       = 'No variables with the name %s found';

  SWrongTblCount          = 'To execute BackupQuery TableNames must have not more than one table';

  SCannotIntercept        = 'Cannot intercept';
  STwoEmbServer           = 'Cannot run second Embedded server instance for single data folder. Please see details in MyDAC help or MySQL Reference manual';

  SBHCaption              = '-- MyDAC version: %s'#$D#$A +
                            '-- MySQL server version: %s'#$D#$A +
                            '-- MySQL client version: %s'#$D#$A +
                            '-- Script date %s'#$D#$A +
                            '-- ---------------------------------------------------------------------- '#$D#$A +
                            '-- Server: %s'#$D#$A +
                            '-- Database: %s'#$D#$A#$D#$A;

  SBHDatabase             = '-- '#$D#$A +
                            '-- Database %s structure'#$D#$A +
                            '-- '#$D#$A#$D#$A;

  SBHUsers                = '-- '#$D#$A +
                            '-- User permissions'#$D#$A +
                            '-- '#$D#$A#$D#$A;

  SBHStoredProcStruct     = '-- '#$D#$A +
                            '-- StoredProc  %s'#$D#$A +
                            '-- '#$D#$A#$D#$A;


  SBHTableStruct          = '-- '#$D#$A +
                            '-- Table structure for table  %s'#$D#$A +
                            '-- '#$D#$A#$D#$A;

  SBHTableData            = '-- '#$D#$A +
                            '-- Dumping data for table %s'#$D#$A +
                            '-- '#$D#$A#$D#$A;

  SBHTriggerStruct        = '-- '#$D#$A +
                            '-- Trigger  %s'#$D#$A +
                            '-- '#$D#$A#$D#$A;

implementation

end.
