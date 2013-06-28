
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright (c) 1998-2011 Devart. All right reserved.
//  Oracle constants
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Odac.inc}
unit OraConstsUni;
{$ENDIF}

interface

resourcestring
  SSessionNotDefined      = 'Session is not defined';
  SSessionNotConnected    = 'Session is not connected';
  SUnsupportedDataType    = 'Unsupported data type';
  SInvalidBufferSize      = 'For reference BufferSize must be 0';
  SParamIsNotStorable     = 'Param is not storable';
  SCannotChangeBufferSize = 'BufferSize cannot be changed for this type';
  SInvalidIndex           = 'Index of array is not range';
  SInvalidTableType       = 'This type cannot consist in table';
  SInvalidNationalType    = 'Wrong national type';
  SInvalidDmlArrayType    = 'Type is not allowed in DML array';
  SCursorNotOpened        = 'Cursor must be opened';
  SCursorOpened           = 'Cursor has been opened';
  //SUnknownParamType       = 'Unknown type of parameter %s';
  SArrayParam             = '%s parameter must be array with length %d';
  STooManyInputParams     = 'Too many Input params';
  SNotEnoughInputParams   = 'Not enough Input params';
  STooNanyOutputParams    = 'Too many Output params';
  SNotEnoughOutputParams  = 'Not enough Output params';
  SPackageNotExist        = 'Package %s does not exist';
  SNoOCISvcCtx            = 'OCISvcCtx is not defined';
  SObjectNotExist         = 'Object %s does not exist';
  SNoObjectType           = 'Type of object must be defined';
  SObjectNotAllocated     = 'Object is not allocated';
  SObjectIsCached         = 'Object is cached';
  SNeedThreadSafety       = 'Session must be Thread Safety';
  SParamNotFound          = 'Parameter %s not found';
  SReopenNotAllowed       = 'Reopen is not allowed';
  SLobNotAllocatted       = 'Lob is not allocated';
  SSvcCtxNotDefined       = 'OCISvcCtx is not defined';
  SNeedParamType          = 'Lob parameter should be Input or Output';
  SLobNotInited           = 'Lob locator should be inited';
  SCannotDisableLobCache  = 'Cannot disable lob caching. Lob is not empty.';
  SInvalidConnectString   = 'Invalid connect string';
  SHomeNameDiffers        = 'Connection HomeName differs from already active connections';
  SDirectDiffers          = 'Connection Direct property differs from already active connections';
  SOCIUnicodeDiffers      = 'Connection Unicode OCI environment differs from already active connections';

  SCannotChangeParams     = 'Parameters changing is alowed for TOraStoredProc only';
  SNoCorrespondParam      = 'Field corresponding parameter %s is not found';
//  SUpdateFailed           = 'Update Failed. Found %d records';
  SNeedBlobType           = 'Field type should be Memo, Blob or File';
  SNeedLobType            = 'Field type should be CLOB, BLOB or File';
  SNeedFileType           = 'Field type should be CFile or BFile';
  SNeedObjectType         = 'Field type should be Object';
  SNeedTimeStampType      = 'Field type should be TimeStamp';
  SNeedIntervalType       = 'Field type should be Interval';
  SNeedNumberType         = 'Field type should be Number';
  SMissingDataSetField    = 'Missing DataSetField property';
  SColumnNotFound         = 'Column %s not found';

  SUpdatingTableNotDef      = 'UpdatingTable is not defined';
  SCannotWriteExpandedField = 'Cannot write expanded field';
  SCannotReadKeyFields      = 'Cannot read KeyFields. Valid format: [Key1[;Key2<..>]]';

  SInvalidTimeStamp = 'Invalid timestamp.';
  SInvalidInterval = 'Invalid interval.';

  SCannotConvertStrToBool = 'Cannot convert string ''%s'' to boolean.';
  SNoTimers = 'Not enough timers available';
  SBlobNotExpanded = 'Cannot read not expanded blob field ''%s''.';
  SAlerterReceiveFailed = 'Message receive failed. Status: %d';
  SAlerterSendFailed = 'Message send failed';
  SOLiteSchemaNotSupported = 'Schema feature is not supported with Oracle Lite servers';
  SUseUnicodeRequired = 'UseUnicode option must be set to True for Unicode OCI environment';
  SClobMustBeUnicode = 'IsUnicode must be set to True for CLOB in Unicode OCI environment';
  SCannotInitOCI = 'Can''t initialize OCI. Error %d';
  SCheckOCI   = 'OCI is not inizialized';
  SCheckOCI73 = 'Need Oracle 7.3 Call Interface';
  SCheckOCI80 = 'Need Oracle 8 Call Interface';
  SCheckOCI81 = 'Need Oracle 8i Call Interface';
  SCheckOCI90 = 'Need Oracle 9 Call Interface';

  SDirectPathLoadingNotSupportedWithDirect = 'Direct path loading is not supported in Direct mode';

  SProxyPoolNotSupportedWithDirect = 'Proxy pooling is not supported in Direct mode';
  SOCIPoolNotSupportedWithDirect = 'OCI pooling is not supported in Direct mode';
  SOCIPoolNotSupported = 'OCI pooling is supported starting with OCI 9.2';
  SMTSPoolNotSupportedWithDirect = 'MTS pooling is not supported in Direct mode';
  SStmtCacheNotSupportedWithDirect = 'Statement cache is not supported in Direct mode';

  SClientIdentifierFirstCharShouldNotBeColon = 'The first character of the ClientIdentifier should not be '':''';

  SCannotStartTransactionWithoutId = 'Cannot start distributed transaction without transaction name or XID';
  STransactionIdTooLong    = '%s cannot contain more than 64 elements';
  SDetachNotSupportedWithMTS     = 'Detach/Resume operations are not supported with MTS';
  STransactionNotSupportedWithDirect = 'Distributed transactions are not supported in Direct mode';
  SReadOnlyNotSupportedWithMTS = 'Readonly transactions are not supported with MTS';

  SChangeNotifyOperationsCannotBeEmpty = 'Operations for change notification subscription cannot be empty set';
  SChangeNotifyNotSupportedWithDirect = 'Change notification is not supported in Direct mode';
  SChangeNotifyOCIVersion = 'OCI 10.2 or greater is needed for change notification';
  SChangeNotifyServerVersion = 'Change notification is supported starting with Oracle 10.2';

  SQueueNotFound = 'Queue %s.%s not found';
  SQueueTableNotFound = 'Queue table %s.%s not found';
  SUnknownPayloadType = 'Unknown payload type: %s';
  SUnknownSortOrder = 'Unknown sort order: %s';
  SUnknownRecipientsType = 'Unknown recipients type: %s';
  SUnknownMessageGrouping = 'Unknown message grouping: %s';
  SUnknownCompatibility = 'Unknown queue table compatibility: %s';
  SUnknownSecurityValue = 'Unknown queue table security value: %s';
  SUnknownQueueType = 'Unknown queue type: %s';
  SSubscriptionNotSupportedWithDirect = 'AsyncNotification is not supported in Direct mode';
  SSubscriptionOracleVersion = 'Cannot enable AsyncNotification: OCI version doesn''t much Oracle server version';
  SPurgeNotSupported = 'Purging queue table is supported starting with Oracle 10';
  SEnqueueArrayNotSupported = 'EnqueueArray/DequeueArray are supported starting with Oracle 10';
  SPayloadArrayTypeNotFound = 'Collection type for payload type %s.%s not found';
  SMessagesNotEnqueued = 'Some messages was not enqueued';
  SQueueTableNameNotDefined = 'QueueTableName is not defined';
  SQueueNameNotDefined = 'QueueName is not defined';

  SPlSqlTraceNotSet = 'PL/SQL Trace is not set';
  SBadCallParameterPrepare = 'Bad parameter preparation';

implementation

end.
