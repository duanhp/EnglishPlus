
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 2006-2011 Devart. All right reserved.
//  InterBase Call Interface
//////////////////////////////////////////////////

{$R-}

{$IFNDEF CLR}

{$I IbDac.inc}
unit IBCCallUni;
{$ENDIF}

{$O-}

interface

uses
{$IFDEF CLR}
  System.Text,
  System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
{$IFDEF LINUX}
  Types,
{$ENDIF}
  SysUtils, Classes, Math, MemUtils, DAConsts,
  {$IFNDEF UNIDACPRO}IBCConsts{$ELSE}IBCConstsUni{$ENDIF};

const
  ISC_TRUE = 1;
  ISC_FALSE = 0;
  DSQL_close = 1;
  DSQL_drop = 2;
  DSQL_cancel = 4;

  FB_CANCEL_DISABLE = 1;
  FB_CANCEL_ENABLE  = 2;
  FB_CANCEL_RAISE   = 3;
  FB_CANCEL_ABORT   = 4;

var
  CANCEL_OPTION: Word = FB_CANCEL_RAISE;

const
  METADATALENGTH = 68;
  DefaultDBSQLDialect = 3;
  IBDAC_EMPTY_STRING = 'IBDAC_EMPTY_STRING';

const
  SQLDA_VERSION1	             = 1; 
  SQLDA_VERSION2	             = 2; 
  SQL_DIALECT_V5	             = 1; 
  SQL_DIALECT_V6_TRANSITION    = 2;
   
  SQL_DIALECT_V6	             = 3;

  SQL_DIALECT_CURRENT	         = SQL_DIALECT_V6;

  SQL_UNKNOWN          =  0;
  SQL_SELECT           =  1;
  SQL_INSERT           =  2;
  SQL_UPDATE           =  3;
  SQL_DELETE           =  4;
  SQL_DDL              =  5;
  SQL_GET_SEGMENT      =  6;
  SQL_PUT_SEGMENT      =  7;
  SQL_EXEC_PROCEDURE   =  8;
  SQL_START_TRANS      =  9;
  SQL_COMMIT           = 10;
  SQL_ROLLBACK         = 11;
  SQL_SELECT_FOR_UPD   = 12;
  SQL_SET_GENERATOR    = 13;

  SQL_EX_EXECUTE_BLOCK = 100;

type

  Int                  = LongInt;
  UInt                 = DWord;
  Long                 = LongInt;
  ULong                = DWord;
  Short                = SmallInt;
  UShort               = Word;
  Float                = Single;
  UChar                = Byte;
  ISC_LONG             = Long;
  UISC_LONG            = ULong;
  ISC_INT64            = Int64;
  ISC_BOOLEAN          = SmallInt;
  ISC_STATUS           = NativeInt;
  UISC_STATUS          = NativeUInt;
  Void                 = Pointer;

  PPChar               = IntPtr;
  PSmallInt            = IntPtr;
  PInt                 = IntPtr;
  PInteger             = IntPtr;
  PShort               = IntPtr;
  PUShort              = IntPtr;
  PLong                = IntPtr;
  PULong               = IntPtr;
  PFloat               = IntPtr;
  PUChar               = IntPtr;
  PVoid                = IntPtr;
  PDouble              = IntPtr;
  PISC_LONG            = IntPtr;
  PUISC_LONG           = IntPtr;
  PISC_STATUS          = IntPtr;
  PPISC_STATUS         = IntPtr;
  PUISC_STATUS         = IntPtr;

{$IFDEF CLR}
  PStr = TBytes;
{$ELSE}
  PStr = Pointer;
{$ENDIF}

const
{$IFDEF CLR}
  SizeOf_IntPtr = 4;
{$ELSE}
  SizeOf_IntPtr = SizeOf(IntPtr);
{$ENDIF}

type
{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}
  TCTimeStructure = record
    tm_sec : integer;   { Seconds }
    tm_min : integer;   { Minutes }
    tm_hour : integer;  { Hour (0--23) }
    tm_mday : integer;  { Day of month (1--31) }
    tm_mon : integer;   { Month (0--11) }
    tm_year : integer;  { Year (calendar year minus 1900) }
    tm_wday : integer;  { Weekday (0--6) Sunday = 0) }
    tm_yday : integer;  { Day of year (0--365) }
    tm_isdst : integer; { 0 if daylight savings time is not in effect) }
{$IFDEF LINUX}
    tm_gmtoff : integer; {Seconds east of UTC}
    tm_zone: IntPtr; {Timezone abbreviation}
{$ENDIF}
  end;
  PCTimeStructure = IntPtr;

type

{ InterBase Handle Definitions }

  TISC_ATT_HANDLE               = IntPtr;
  PISC_ATT_HANDLE               = IntPtr;
  TISC_BLOB_HANDLE              = IntPtr;
  PISC_BLOB_HANDLE              = IntPtr;
  TISC_DB_HANDLE                = IntPtr;
  PISC_DB_HANDLE                = IntPtr;
  TISC_FORM_HANDLE              = IntPtr;
  PISC_FORM_HANDLE              = IntPtr;
  TISC_REQ_HANDLE               = IntPtr;
  PISC_REQ_HANDLE               = IntPtr;
  TISC_STMT_HANDLE              = IntPtr;
  PISC_STMT_HANDLE              = IntPtr;
  TISC_SVC_HANDLE               = IntPtr;
  PISC_SVC_HANDLE               = IntPtr;
  TISC_TR_HANDLE                = IntPtr;
  PISC_TR_HANDLE                = IntPtr;
  TISC_WIN_HANDLE               = IntPtr;
  PISC_WIN_HANDLE               = IntPtr;
  TISC_CALLBACK                 = procedure(P: IntPtr; Length: SmallInt; Updated: IntPtr); {$IFNDEF CLR} cdecl; {$ENDIF}
  ISC_SVC_HANDLE                = ISC_LONG;

type
  ISC_DATE = Long;
  PISC_DATE = IntPtr;
  ISC_TIME = ULong;
  PISC_TIME = IntPtr;
  PISC_TIMESTAMP = IntPtr;

{ Blob id }
  TISC_QUAD            = Int64;
  PISC_QUAD            = IntPtr;

  PISC_BLOB_DESC       = IntPtr;
  PISC_BLOB_DESC_V2    = IntPtr;

  PISC_ARRAY_BOUND     = IntPtr;
  PISC_ARRAY_DESC      = IntPtr;
  PISC_ARRAY_DESC_V2   = IntPtr;

const
  ARR_DESC_VERSION2 = 2;
  ARR_DESC_CURRENT_VERSION = ARR_DESC_VERSION2;

type

{ Blob stream definitions }

{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}
  TBSTREAM = record
    bstr_blob                   : IntPtr;
    bstr_buffer                 : IntPtr;
    bstr_ptr                    : IntPtr;
    bstr_length                 : Short;
    bstr_cnt                    : Short;
    bstr_mode                   : AnsiChar;
  end;
  PBSTREAM                      = IntPtr;

  PXSQLVAR_V1 = IntPtr;
  PXSQLVAR = IntPtr;
  PXSQLDA_V1 = IntPtr;
  PXSQLDA = IntPtr;

 {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
 {$ENDIF}
  PISC_TEB = IntPtr;
  PISC_TEB_ARRAY = IntPtr;

{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}
  TXSQLVAR_V1 = record
    sqltype                     : Short;     (** datatype of field **)
    sqlscale                    : Short;     (** scale factor **)
    sqlsubtype                  : Short;     (** datatype subtype - BLOBs & text types only **)
    sqllen                      : Short;     (** length of data area **)
    sqldata                     : IntPtr;     (** address of data **)
    sqlind                      : IntPtr;    (** address of indicator variable **)
    sqlname_length              : Short;     (** length of sqlname field **)
    sqlname                     : array[0..31] of AnsiChar; (** name of field, name length + space for NULL **)
    relname_length              : Short;     (** length of relation name **)
    relname                     : array[0..31] of AnsiChar; (** field's relation name + space for NULL **)
    ownname_length              : Short;     (** length of owner name **)
    ownname                     : array[0..31] of AnsiChar; (** relation's owner name + space for NULL **)
    aliasname_length            : Short;     (** length of alias name **)
    aliasname                   : array[0..31] of AnsiChar; (** relation's alias name + space for NULL **)
  end;

const
  SizeOfXSQLVAR_V1 = {$IFNDEF CLR}SizeOf(TXSQLVAR_V1){$ELSE}152{$ENDIF};
  OffsetOf_XSQLVAR_V1_sqltype = 0;
  OffsetOf_XSQLVAR_V1_sqlscale = OffsetOf_XSQLVAR_V1_sqltype + SizeOf(Short); //2
  OffsetOf_XSQLVAR_V1_sqlsubtype = OffsetOf_XSQLVAR_V1_sqlscale + SizeOf(Short); //4
  OffsetOf_XSQLVAR_V1_sqllen = OffsetOf_XSQLVAR_V1_sqlsubtype + SizeOf(Short); //6
  OffsetOf_XSQLVAR_V1_sqldata = OffsetOf_XSQLVAR_V1_sqllen + SizeOf(Short); //8
  OffsetOf_XSQLVAR_V1_sqlind = OffsetOf_XSQLVAR_V1_sqldata + SizeOf_IntPtr; //12
  OffsetOf_XSQLVAR_V1_sqlname_length = OffsetOf_XSQLVAR_V1_sqlind + SizeOf_IntPtr; //16
  OffsetOf_XSQLVAR_V1_sqlname = OffsetOf_XSQLVAR_V1_sqlname_length + SizeOf(Short); //18
  OffsetOf_XSQLVAR_V1_relname_length = OffsetOf_XSQLVAR_V1_sqlname + 32; //50
  OffsetOf_XSQLVAR_V1_relname = OffsetOf_XSQLVAR_V1_relname_length + SizeOf(Short); //52
  OffsetOf_XSQLVAR_V1_ownname_length = OffsetOf_XSQLVAR_V1_relname + 32; //84
  OffsetOf_XSQLVAR_V1_ownname = OffsetOf_XSQLVAR_V1_ownname_length + SizeOf(Short); //86
  OffsetOf_XSQLVAR_V1_aliasname_length = OffsetOf_XSQLVAR_V1_ownname + 32; //118
  OffsetOf_XSQLVAR_V1_aliasname = OffsetOf_XSQLVAR_V1_aliasname_length + SizeOf(Short); //120

type
{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}
  TXSQLDA_V1 = record
    version                     : Short;     (** version of this XSQLDA **)
    sqldaid                     : array[0..7] of AnsiChar; (** XSQLDA name field **)
    sqldabc                     : ISC_LONG;  (** length in bytes of SQLDA **)
    sqln                        : Short;     (** number of fields allocated **)
    sqld                        : Short;     (** actual number of fields **)
    sqlvar                      : array[0..0] of TXSQLVAR_V1; (** first field address **)
  end;

const
  SizeOfXSQLDA_V1 = {$IFNDEF CLR}SizeOf(TXSQLDA_V1){$ELSE}172{$ENDIF};

type
{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}
  TXSQLVAR = record
    sqltype                     : Short;     (** datatype of field **)
    sqlscale                    : Short;     (** scale factor **)
    sqlprecision                : Short;     (** precision : Reserved for future **)
    sqlsubtype                  : Short;     (** datatype subtype - BLOBs & text types only **)
    sqllen                      : Short;     (** length of data area **)
    sqldata                     : IntPtr;     (** address of data **)
    sqlind                      : IntPtr;    (** address of indicator variable **)
    sqlname_length              : Short;     (** length of sqlname field **)
    sqlname                     : array[0..METADATALENGTH - 1] of AnsiChar; (** name of field, name length + space for NULL **)
    relname_length              : Short;     (** length of relation name **)
    relname                     : array[0..METADATALENGTH - 1] of AnsiChar; (** field's relation name + space for NULL **)
    ownname_length              : Short;     (** length of owner name **)
    ownname                     : array[0..METADATALENGTH - 1] of AnsiChar; (** relation's owner name + space for NULL **)
    aliasname_length            : Short;     (** length of alias name **)
    aliasname                   : array[0..METADATALENGTH - 1] of AnsiChar; (** relation's alias name + space for NULL **)
  end;

const
  SizeOfXSQLVAR = {$IFNDEF CLR}SizeOf(TXSQLVAR){$ELSE}300{$ENDIF};
  OffsetOf_XSQLVAR_sqltype = 0;
  OffsetOf_XSQLVAR_sqlscale = OffsetOf_XSQLVAR_sqltype + SizeOf(Short); //2
  OffsetOf_XSQLVAR_sqlprecision = OffsetOf_XSQLVAR_sqlscale + SizeOf(Short); //4
  OffsetOf_XSQLVAR_sqlsubtype = OffsetOf_XSQLVAR_sqlprecision + SizeOf(Short); //6
  OffsetOf_XSQLVAR_sqllen = OffsetOf_XSQLVAR_sqlsubtype + SizeOf(Short); //8
  OffsetOf_XSQLVAR_sqldata = OffsetOf_XSQLVAR_sqllen + SizeOf(Short) + 2; // Should be 10, but Marshal.OffsetOf return 12
  OffsetOf_XSQLVAR_sqlind = OffsetOf_XSQLVAR_sqldata + SizeOf_IntPtr; //16
  OffsetOf_XSQLVAR_sqlname_length = OffsetOf_XSQLVAR_sqlind + SizeOf_IntPtr; //20
  OffsetOf_XSQLVAR_sqlname = OffsetOf_XSQLVAR_sqlname_length + SizeOf(Short); //22
  OffsetOf_XSQLVAR_relname_length = OffsetOf_XSQLVAR_sqlname + METADATALENGTH; //90
  OffsetOf_XSQLVAR_relname = OffsetOf_XSQLVAR_relname_length + SizeOf(Short); //92
  OffsetOf_XSQLVAR_ownname_length = OffsetOf_XSQLVAR_relname + METADATALENGTH; //160
  OffsetOf_XSQLVAR_ownname = OffsetOf_XSQLVAR_ownname_length + SizeOf(Short); //162
  OffsetOf_XSQLVAR_aliasname_length = OffsetOf_XSQLVAR_ownname + METADATALENGTH; //230
  OffsetOf_XSQLVAR_aliasname = OffsetOf_XSQLVAR_aliasname_length + SizeOf(Short); //232

type
{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}
  TXSQLDA = record
    version                     : Short;     (** version of this XSQLDA **)
    sqldaid                     : array[0..7] of AnsiChar; (** XSQLDA name field **)
    sqldabc                     : ISC_LONG;  (** length in bytes of SQLDA **)
    sqln                        : Short;     (** number of fields allocated **)
    sqld                        : Short;     (** actual number of fields **)
    sqlvar                      : array[0..0] of TXSQLVAR; (** first field address **)
  end;

const
  SizeOfXSQLDA = {$IFNDEF CLR}SizeOf(TXSQLDA){$ELSE}320{$ENDIF};
  OffsetOf_XSQLDA_version = 0;
  OffsetOf_XSQLDA_sqldaid = OffsetOf_XSQLDA_version + SizeOf(Short); //2
  OffsetOf_XSQLDA_sqldabc = OffsetOf_XSQLDA_sqldaid + 8 + 2; // Should be 10, but Marshal.OffsetOf return 12
  OffsetOf_XSQLDA_sqln = OffsetOf_XSQLDA_sqldabc + SizeOf(ISC_LONG); //16
  OffsetOf_XSQLDA_sqld = OffsetOf_XSQLDA_sqln + SizeOf(Short); //18
  OffsetOf_XSQLDA_sqlvar = SizeOfXSQLDA - SizeOfXSQLVAR; // Should be 20, but Marshal.OffsetOf return 24 in x64 bit

type
{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}
  TISC_TEB = record
    db_handle      : PISC_DB_HANDLE;
    tpb_length     : Long;
    tpb_address    : IntPtr;
  end;

const
  SizeOfISC_TEB = {$IFNDEF CLR}SizeOf(TISC_TEB){$ELSE}12{$ENDIF};
  OffsetOf_TISC_TEB_tpb_length = SizeOf_IntPtr;
  OffsetOf_TISC_TEB_tpb_address = SizeOfISC_TEB - SizeOf_IntPtr;

type
{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}
  TISC_BLOB_DESC = record
    blob_desc_subtype           : Short;
    blob_desc_charset           : Short;
    blob_desc_segment_size      : Short;
    blob_desc_field_name        : array[0..31] of UChar;
    blob_desc_relation_name     : array[0..31] of UChar;
  end;

const
  SizeOfBLOB_DESC = {$IFNDEF CLR}SizeOf(TISC_BLOB_DESC){$ELSE}70{$ENDIF};

type
{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}
  TISC_ARRAY_BOUND = record
    array_bound_lower  : Short;
    array_bound_upper  : Short;
  end;

const
  SizeOfISC_ARRAY_BOUND = {$IFNDEF CLR}SizeOf(TISC_ARRAY_BOUND){$ELSE}4{$ENDIF};

type
{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}
  TISC_ARRAY_DESC = record
    array_desc_dtype            : UChar;
    array_desc_scale            : AnsiChar;
    array_desc_length           : UShort;
    array_desc_field_name       : array[0..31] of AnsiChar;
    array_desc_relation_name    : array[0..31] of AnsiChar;
    array_desc_dimensions       : Short;
    array_desc_flags            : Short;
    array_desc_bounds           : array[0..15] of TISC_ARRAY_BOUND;
  end;

const
  SizeOfISC_ARRAY_DESC = {$IFNDEF CLR}SizeOf(TISC_ARRAY_DESC){$ELSE}136{$ENDIF};

type
{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}
  TISC_ARRAY_DESC_V2 = record
    array_desc_version          : Short;
    array_desc_dtype            : UChar;
    array_desc_subtype          : UChar;
    array_desc_scale            : Byte;
    array_desc_length           : UShort;
    array_desc_field_name       : array[0..METADATALENGTH - 1] of AnsiChar;
    array_desc_relation_name    : array[0..METADATALENGTH - 1] of AnsiChar;
    array_desc_dimensions       : Short;
    array_desc_flags            : Short;
    array_desc_bounds           : array[0..15] of TISC_ARRAY_BOUND;
  end;

const
  SizeOfISC_ARRAY_DESC_V2 = {$IFNDEF CLR}SizeOf(TISC_ARRAY_DESC_V2){$ELSE}212{$ENDIF};

  sec_uid_spec		                = $01;
  sec_gid_spec		                = $02;
  sec_server_spec		              = $04;
  sec_password_spec	              = $08;
  sec_group_name_spec	            = $10;
  sec_first_name_spec	            = $20;
  sec_middle_name_spec            = $40;
  sec_last_name_spec	            = $80;
  sec_dba_user_name_spec          = $100;
  sec_dba_password_spec           = $200;

  sec_protocol_tcpip              = 1;
  sec_protocol_netbeui            = 2;
  sec_protocol_spx                = 3;
  sec_protocol_local              = 4;

type

{ XML types }

{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}
  Tib_xmlda = record
    xmlda_file_name : IntPtr;
	  xmlda_header_tag : IntPtr;
	  xmlda_database_tag : IntPtr;
	  xmlda_table_tag : IntPtr;
	  xmlda_row_tag : IntPtr;
    xmlda_file_ptr : IntPtr;
    xmlda_temo_buffer : IntPtr;
    xmlda_fetch_stata : ISC_STATUS;
    xmlda_flags : ULong;
    xmlda_more_data : ULong;
    xmlda_temp_size : ULong;
    xmlda_status : Short;
    xmlda_more : Short;
	  xmlda_version : UShort;
    xmlda_array_size : UShort;
    xmlda_reserved :	ULong;
  end;
  PIB_XMLDA = IntPtr;

{ GDS functions definitions }

  Tisc_attach_database = function (status_vector: PISC_STATUS; db_name_length: Short;
                             db_name: IntPtr; db_handle: PISC_DB_HANDLE;
                             parm_buffer_length: Short; parm_buffer: TBytes): ISC_STATUS;
                             {$IFNDEF CLR}
                             {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                             {$ENDIF}



  Tisc_array_gen_sdl = function(status_vector: PISC_STATUS;
                            isc_array_desc: IntPtr;
                            isc_arg3: PShort;
                            isc_arg4: IntPtr;
                            isc_arg5: PShort): ISC_STATUS;
                            {$IFNDEF CLR}
                            {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                            {$ENDIF}

  Tisc_array_get_slice = function (status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
                             trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
                             descriptor: IntPtr; dest_array: IntPtr;
                             slice_length: PISC_LONG): ISC_STATUS;
                             {$IFNDEF CLR}
                             {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                             {$ENDIF}

  Tisc_array_lookup_bounds = function (status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
                                 trans_handle: PISC_TR_HANDLE; table_name, column_name: IntPtr;
                                 descriptor: IntPtr): ISC_STATUS;
                                 {$IFNDEF CLR}
                                 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                 {$ENDIF}

  Tisc_array_lookup_desc = function (status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
                               trans_handle: PISC_TR_HANDLE; table_name, column_name: IntPtr;
                               descriptor: IntPtr): ISC_STATUS;
                               {$IFNDEF CLR}
                               {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                               {$ENDIF}

  Tisc_array_set_desc = function (status_vector: PISC_STATUS; table_name: IntPtr;
                            column_name: IntPtr; sql_dtype, sql_length, sql_dimensions: PShort;
                            descriptor: IntPtr): ISC_STATUS;
                            {$IFNDEF CLR}
                            {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                            {$ENDIF}

  Tisc_array_put_slice = function (status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
                             trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
                             descriptor: IntPtr; source_array: IntPtr;
                             slice_length: PISC_LONG): ISC_STATUS;
                             {$IFNDEF CLR}
                             {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                             {$ENDIF}

  Tisc_blob_default_desc = procedure (descriptor: PISC_BLOB_DESC; table_name: PUChar;
                                column_name: PUChar);
                                {$IFNDEF CLR}
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                {$ENDIF}

  Tisc_blob_default_desc2 = procedure (descriptor: PISC_BLOB_DESC_V2; table_name: PUChar;
                                 column_name: PUChar);
                                 {$IFNDEF CLR}
                                 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                 {$ENDIF}

  Tisc_blob_gen_bpb = function (status_vector: PISC_STATUS; to_descriptor, from_descriptor: PISC_BLOB_DESC;
                          bpb_buffer_length: UShort; bpb_buffer: PUChar;
                          bpb_length: PUShort): ISC_STATUS;
                          {$IFNDEF CLR}
                          {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                          {$ENDIF}

  Tisc_blob_gen_bpb2 = function (status_vector: PISC_STATUS; to_descriptor, from_descriptor: PISC_BLOB_DESC_V2;
                           bpb_buffer_length: UShort; bpb_buffer: PUChar;
                           bpb_length: PUShort): ISC_STATUS;
                           {$IFNDEF CLR}
                           {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                           {$ENDIF}

  Tisc_blob_info = function (status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
                       item_list_buffer_length: Short; item_list_buffer: IntPtr;
                       result_buffer_length: Short; result_buffer: IntPtr): ISC_STATUS;
                       {$IFNDEF CLR}
                       {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                       {$ENDIF}

  Tisc_blob_lookup_desc = function (status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
                              trans_handle: PISC_TR_HANDLE; table_name, column_name: PStr;
                              descriptor: PISC_BLOB_DESC; global: PUChar): ISC_STATUS;
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}

  Tisc_blob_lookup_desc2 = function (status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
                               trans_handle: PISC_TR_HANDLE; table_name, column_name: IntPtr;
                               descriptor: PISC_BLOB_DESC_v2; global: PUChar): ISC_STATUS;
                               {$IFNDEF CLR}
                               {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                               {$ENDIF}

  Tisc_blob_set_desc = function (status_vector: PISC_STATUS; table_name, column_name: IntPtr;
                           subtype, charset, segment_size: Short; descriptor: PISC_BLOB_DESC): ISC_STATUS;
                           {$IFNDEF CLR}
                           {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                           {$ENDIF}

  Tisc_blob_set_desc2 = function (status_vector: PISC_STATUS; table_name, column_name: IntPtr;
                            subtype, charset, segment_size: Short;
                            descriptor: PISC_BLOB_DESC_V2): ISC_STATUS;
                            {$IFNDEF CLR}
                            {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                            {$ENDIF}

  Tisc_cancel_blob = function (status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
                         {$IFNDEF CLR}
                         {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                         {$ENDIF}

  Tisc_cancel_events = function (status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
                           event_id: PISC_LONG): ISC_STATUS;
                           {$IFNDEF CLR}
                           {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                           {$ENDIF}

  Tisc_close_blob = function (status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
                        {$IFNDEF CLR}
                        {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                        {$ENDIF}

  Tisc_commit_retaining = function (status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}

  Tisc_commit_transaction = function (status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
                                {$IFNDEF CLR}
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                {$ENDIF}

  Tisc_create_blob2 = function (status_vector : PISC_STATUS; db_handle : PISC_DB_HANDLE;
                          tran_handle : PISC_TR_HANDLE; blob_handle : PISC_BLOB_HANDLE;
                          blob_id : PISC_QUAD; bpb_length : Short; bpb_address : TBytes): ISC_STATUS;
                          {$IFNDEF CLR}
                          {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                          {$ENDIF}
                              
  Tisc_database_info = function (status_vector : PISC_STATUS; db_handle : PISC_DB_HANDLE;
                           item_list_buffer_length : Short; item_list_buffer : IntPtr;
                           result_buffer_length : Short; result_buffer : IntPtr): ISC_STATUS;
                           {$IFNDEF CLR}
                           {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                           {$ENDIF}

  Tisc_decode_date = procedure (ib_date: PISC_QUAD;var tm_date: TCTimeStructure);
                          {$IFNDEF CLR}
                          {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                          {$ENDIF}

  Tisc_decode_sql_date = procedure (ib_date: PISC_DATE;var tm_date: TCTimeStructure);
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}

  Tisc_decode_sql_time = procedure (ib_time: PISC_TIME;var tm_date: TCTimeStructure);
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}

  Tisc_detach_database = function (status_vector : PISC_STATUS; db_handle : PISC_DB_HANDLE): ISC_STATUS;
                             {$IFNDEF CLR}
                             {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                             {$ENDIF}

  Tisc_drop_database = function (status_vector : PISC_STATUS; db_handle : PISC_DB_HANDLE): ISC_STATUS;
                           {$IFNDEF CLR}
                           {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                           {$ENDIF}

  Tisc_dsql_alloc_statement2 = function (status_vector : PISC_STATUS; db_handle : PISC_DB_HANDLE;
                                   stmt_handle : PISC_STMT_HANDLE): ISC_STATUS;
                                  {$IFNDEF CLR}
                                  {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                  {$ENDIF}

  Tisc_dsql_describe = function (status_vector : PISC_STATUS; stmt_handle : PISC_STMT_HANDLE;
                           dialect : UShort; xsqlda : PXSQLDA): ISC_STATUS;
                           {$IFNDEF CLR}
                           {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                           {$ENDIF}

  Tisc_dsql_describe_bind = function (status_vector : PISC_STATUS; stmt_handle : PISC_STMT_HANDLE;
                                dialect : UShort; xsqlda : PXSQLDA): ISC_STATUS;
                                {$IFNDEF CLR}
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                {$ENDIF}

  Tisc_dsql_execute = function (status_vector : PISC_STATUS; tran_handle : PISC_TR_HANDLE;
                          stmt_handle : PISC_STMT_HANDLE; dialect : UShort;
                          xsqlda : PXSQLDA): ISC_STATUS;
                          {$IFNDEF CLR}
                          {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                          {$ENDIF}

  Tisc_dsql_execute2 = function (status_vector : PISC_STATUS; tran_handle : PISC_TR_HANDLE;
                           stmt_handle : PISC_STMT_HANDLE; dialect : UShort;
                           in_xsqlda, out_xsqlda : PXSQLDA): ISC_STATUS;
                           {$IFNDEF CLR}
                           {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                           {$ENDIF}

  Tisc_dsql_execute_immediate = function (status_vector : PISC_STATUS; db_handle : PISC_DB_HANDLE;
                                    tran_handle : PISC_TR_HANDLE; length : UShort;
                                    statement : IntPtr; dialect : UShort;
                                    xsqlda : PXSQLDA): ISC_STATUS;
                                   {$IFNDEF CLR}
                                   {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                   {$ENDIF}

  Tisc_dsql_fetch = function (status_vector : PISC_STATUS; stmt_handle : PISC_STMT_HANDLE;
                        dialect : UShort; xsqlda : PXSQLDA): ISC_STATUS;
                        {$IFNDEF CLR}
                        {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                        {$ENDIF}

  Tisc_dsql_free_statement = function (status_vector : PISC_STATUS; stmt_handle : PISC_STMT_HANDLE;
                                 options : UShort): ISC_STATUS;
                                 {$IFNDEF CLR}
                                 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                 {$ENDIF}

  Tfb_cancel_operation = function (status_vector : PISC_STATUS; stmt_handle : PISC_STMT_HANDLE;
                             options : UShort): ISC_STATUS;
                             {$IFNDEF CLR}
                             {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                             {$ENDIF}

  Tisc_dsql_prepare = function (status_vector : PISC_STATUS; tran_handle : PISC_TR_HANDLE;
                          stmt_handle : PISC_STMT_HANDLE; length : UShort;
                          statement : PStr; dialect : UShort;
                          xsqlda : PXSQLDA): ISC_STATUS;
                          {$IFNDEF CLR}
                          {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                          {$ENDIF}

  Tisc_dsql_set_cursor_name = function (status_vector : PISC_STATUS; stmt_handle : PISC_STMT_HANDLE;
                                  cursor_name : IntPtr; _type : UShort): ISC_STATUS;
                                  {$IFNDEF CLR}
                                  {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                  {$ENDIF}

  Tisc_dsql_sql_info = function (status_vector : PISC_STATUS; stmt_handle : PISC_STMT_HANDLE;
                           item_length : Short; items : IntPtr; buffer_length : Short;
                           buffer : IntPtr): ISC_STATUS;
                           {$IFNDEF CLR}
                           {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                           {$ENDIF}

  Tisc_encode_date = procedure (var tm_date : TCTimeStructure; ib_date : PISC_QUAD);
                          {$IFNDEF CLR}
                          {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                          {$ENDIF}

  Tisc_encode_sql_date = procedure (var tm_date: TCTimeStructure; ib_date : PISC_DATE);
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}

  Tisc_encode_sql_time = procedure (var tm_date : TCTimeStructure; ib_time : PISC_TIME);
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}

  Tisc_seek_blob = function (status_vector : PISC_STATUS; blob_handle : PISC_BLOB_HANDLE;
                       mode: Short;  //blb_seek_from_begining = 0, blb_seek_relative = 1, blb_seek_from_tail = 2
                       offset: ISC_LONG; actual_offset: PISC_LONG): ISC_STATUS;
                       {$IFNDEF CLR}
                       {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                       {$ENDIF}

  Tisc_interprete = function (buffer : IntPtr; status_vector : PPISC_STATUS): ISC_STATUS;
                        {$IFNDEF CLR}
                        {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                        {$ENDIF}

  Tisc_open_blob2 = function (status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
                        tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE;
                        blob_id: PISC_QUAD; bpb_length: Short; bpb_buffer: TBytes): ISC_STATUS;
                        {$IFNDEF CLR}
                        {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                        {$ENDIF}

  Tisc_release_savepoint = function (status_vector : PISC_STATUS; tran_handle : PISC_TR_HANDLE;
                               tran_name : IntPtr) : ISC_STATUS;
                               {$IFNDEF CLR}
                               {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                               {$ENDIF}

  Tisc_rollback_retaining = function (status_vector : PISC_STATUS; tran_handle : PISC_TR_HANDLE): ISC_STATUS;
                                {$IFNDEF CLR}
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                {$ENDIF}

  Tisc_rollback_savepoint = function (status_vector : PISC_STATUS; tran_handle : PISC_TR_HANDLE;
                                tran_name : IntPtr; Option : UShort) : ISC_STATUS;
                                {$IFNDEF CLR}
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                {$ENDIF}

  Tisc_rollback_transaction = function (status_vector : PISC_STATUS; tran_handle : PISC_TR_HANDLE): ISC_STATUS;
                                  {$IFNDEF CLR}
                                  {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                  {$ENDIF}

  Tisc_start_multiple = function (status_vector : PISC_STATUS; tran_handle : PISC_TR_HANDLE;
                            db_handle_count : Short; teb_vector_address : PISC_TEB): ISC_STATUS;
                            {$IFNDEF CLR}
                            {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                            {$ENDIF}

  Tisc_start_savepoint = function (status_vector : PISC_STATUS; tran_handle : PISC_TR_HANDLE;
                             tran_name : IntPtr): ISC_STATUS;
                             {$IFNDEF CLR}
                             {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                             {$ENDIF}

  Tisc_sqlcode = function (status_vector : PISC_STATUS): ISC_LONG;
                     {$IFNDEF CLR}
                     {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                     {$ENDIF}

  Tisc_sql_interprete = procedure (sqlcode : Short; buffer : IntPtr;
                             buffer_length : Short);
                             {$IFNDEF CLR}
                             {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                             {$ENDIF}

{ Client information functions }

  Tisc_get_client_version = procedure (buffer : IntPtr);
                                 {$IFNDEF CLR}
                                 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                 {$ENDIF}

  Tisc_get_client_major_version = function : Integer;
                                      {$IFNDEF CLR}
                                      {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                      {$ENDIF}

  Tisc_get_client_minor_version = function : Integer;
                                      {$IFNDEF CLR}
                                      {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                      {$ENDIF}
                              
  Tisc_transaction_info = function (status_vector           : PISC_STATUS;
                              tran_handle             : PISC_TR_HANDLE;
                              item_list_buffer_length : Short;
                              item_list_buffer        : IntPtr;
                              result_buffer_length    : Short;
                              result_buffer           : IntPtr): ISC_STATUS;
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}

  Tisc_dsql_xml_fetch = function (status: PISC_STATUS;
                            stmt: PISC_STMT_HANDLE;
                            da_version: USHORT;
                            sqlda: PXSQLDA;
                            var ib_xmlda: TIB_XMLDA) : Integer;
                           {$IFNDEF CLR}
                           {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                           {$ENDIF}

  Tisc_dsql_xml_fetch_all = function (status: PISC_STATUS;
                                stmt: PISC_STMT_HANDLE;
                                da_version: USHORT;
                                sqlda: PXSQLDA;
                                var ib_xmlda: TIB_XMLDA) : Integer;
                                {$IFNDEF CLR}
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                {$ENDIF}

  Tisc_dsql_xml_buffer_fetch = function (status : PISC_STATUS;
                                   stmt: PISC_STMT_HANDLE;
		                               buffer : IntPtr; buffer_size : Integer;
                                   da_version: USHORT;
                                   sqlda: PXSQLDA;
                                   var ib_xmlda: TIB_XMLDA) : Integer;
                                   {$IFNDEF CLR}
                                   {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                   {$ENDIF}

{ Other Dynamic sql functions }

  Tisc_dsql_execute2_m = function (status_vector    : PISC_STATUS;
                                   tran_handle      : PISC_TR_HANDLE;
                                   statement_handle : PISC_STMT_HANDLE;
                                   isc_arg4         : UShort;
                                   isc_arg5         : IntPtr;
                                   isc_arg6         : UShort;
                                   isc_arg7         : UShort;
                                   isc_arg8         : IntPtr;
                                   isc_arg9         : UShort;
                                   isc_arg10        : IntPtr;
                                   isc_arg11        : UShort;
                                   isc_arg12        : UShort;
                                   isc_arg13        : IntPtr): ISC_STATUS;
                                   {$IFNDEF CLR}
                                   {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                   {$ENDIF}

  Tisc_dsql_exec_immed3_m = function  (status_vector : PISC_STATUS;
                                       db_handle     : PISC_DB_HANDLE;
                                       tran_handle   : PISC_TR_HANDLE;
                                       isc_arg4      : UShort;
                                       isc_arg5      : IntPtr;
                                       isc_arg6      : UShort;
                                       isc_arg7      : UShort;
                                       isc_arg8      : IntPtr;
                                       isc_arg9      : UShort;
                                       isc_arg10     : UShort;
                                       isc_arg11     : IntPtr;
                                       isc_arg12     : UShort;
                                       isc_arg13     : IntPtr;
                                       isc_arg14     : UShort;
                                       isc_arg15     : UShort;
                                       isc_arg16     : IntPtr): ISC_STATUS;
                                       {$IFNDEF CLR}
                                       {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                       {$ENDIF}
                                       
  Tisc_dsql_fetch_m = function    (status_vector    : PISC_STATUS;
                                   statement_handle : PISC_STMT_HANDLE;
                                   isc_arg3         : UShort;
                                   isc_arg4         : IntPtr;
                                   isc_arg5         : UShort;
                                   isc_arg6         : UShort;
                                   isc_arg7         : IntPtr): ISC_STATUS;
                                   {$IFNDEF CLR}
                                   {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                   {$ENDIF}

  Tisc_dsql_insert_m = function   (status_vector    : PISC_STATUS;
                                   statement_handle : PISC_STMT_HANDLE;
                                   isc_arg3         : UShort;
                                   isc_arg4         : IntPtr;
                                   isc_arg5         : UShort;
                                   isc_arg6         : UShort;
                                   isc_arg7         : IntPtr): ISC_STATUS;
                                   {$IFNDEF CLR}
                                   {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                   {$ENDIF}

  Tisc_dsql_prepare_m = function  (status_vector             : PISC_STATUS;
                                   tran_handle               : PISC_TR_HANDLE;
                                   statement_handle          : PISC_STMT_HANDLE;
                                   isc_arg4                  : UShort;
                                   isc_arg5                  : IntPtr;
                                   isc_arg6                  : UShort;
                                   isc_arg7                  : UShort;
                                   isc_arg8                  : IntPtr;
                                   isc_arg9                  : UShort;
                                   isc_arg10                 : IntPtr): ISC_STATUS;
                                   {$IFNDEF CLR}
                                   {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                   {$ENDIF}
                                   
  Tisc_dsql_batch_execute_immed = function (status_vector: IntPtr;
                                    db_handle: PISC_DB_HANDLE;
                                    tr_handle: PISC_TR_HANDLE;
                                    dialect: integer;
                                    number_of_sql: longword;
                                    sql: IntPtr;
                                    rows_affected: IntPtr): ISC_STATUS;
                                    {$IFNDEF CLR}
                                    {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                    {$ENDIF}

{ Other OSRI functions }

  Tisc_prepare_transaction = function (status_vector : PISC_STATUS;
                                       tran_handle   : PISC_TR_HANDLE): ISC_STATUS;
                                       {$IFNDEF CLR}
                                       {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                       {$ENDIF}

{ Stream BLOB functions }

  Tisc_prepare_transaction2 = function (status_vector : PISC_STATUS; tran_handle : PISC_TR_HANDLE;
                                        msg_length : Short; msg : IntPtr): ISC_STATUS;
                                        {$IFNDEF CLR}
                                        {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                        {$ENDIF}

  Tisc_put_segment = function (status_vector : PISC_STATUS; blob_handle : PISC_BLOB_HANDLE;
                               seg_buffer_len : UShort; seg_buffer : IntPtr): ISC_STATUS;
                               {$IFNDEF CLR}
                               {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                               {$ENDIF}

  Tisc_get_segment = function (status_vector : PISC_STATUS; blob_handle : PISC_BLOB_HANDLE;
                               actual_seg_length : PUShort; seg_buffer_length : UShort;
                               seg_buffer : IntPtr): ISC_STATUS;
                               {$IFNDEF CLR}
                               {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                               {$ENDIF}

  Tisc_que_events = function (status_vector : PISC_STATUS; db_handle : PISC_DB_HANDLE;
                              event_id : PISC_LONG; length : Short; event_buffer : IntPtr;
                              event_function : IntPtr; event_function_arg : IntPtr): Integer;
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}
                              
  Tisc_event_block = function (var event_buffer: IntPtr; var result_buffer: IntPtr; id_count: UShort;
                               event_1, event_2, event_3, event_4, event_5, event_6, event_7, event_8, event_9,
                               event_10, event_11, event_12, event_13, event_14, event_15: IntPtr): ISC_LONG;
                               {$IFNDEF CLR}
                                 cdecl;
                               {$ENDIF}

  Tisc_free = function (Buffer: IntPtr): ISC_LONG;
                       {$IFNDEF CLR}
                       {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                       {$ENDIF}

  Tisc_event_counts = procedure (status_vector : PISC_STATUS; buffer_length  : Short;
                                 event_buffer : IntPtr; result_buffer : IntPtr);
                                 {$IFNDEF CLR}
                                 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                 {$ENDIF}

{ Other Blob functions }

  TBLOB_open = function (blob_handle: PISC_BLOB_HANDLE; isc_arg2: IntPtr; isc_arg3: int): PBSTREAM;
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}


  TBLOB_put = function (put_char: AnsiChar; blob_stream: PBSTREAM): integer;
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}

  TBLOB_get = function (blob_stream: PBSTREAM): integer;
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}

  TBLOB_close = function (blob_stream: PBSTREAM): integer;
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}

  TBLOB_display = function (blob_ID: PISC_QUAD;
                           db_handle: TISC_DB_HANDLE;
                           tran_handle: TISC_TR_HANDLE;
                           isc_arg4: IntPtr): integer;
                           {$IFNDEF CLR}
                           {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                           {$ENDIF}

  TBLOB_dump = function (blob_ID: PISC_QUAD;
                         db_handle: PISC_DB_HANDLE;
                         tran_handle: PISC_TR_HANDLE;
                         isc_arg4: IntPtr): integer;
                         {$IFNDEF CLR}
                         {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                         {$ENDIF}

  TBLOB_edit = function (blob_ID: PISC_QUAD;
                         db_handle: PISC_DB_HANDLE;
                         tran_handle: PISC_TR_HANDLE;
                         isc_arg4: IntPtr): integer;
                         {$IFNDEF CLR}
                         {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                         {$ENDIF}

  TBLOB_load = function (blob_ID: PISC_QUAD;
                         db_handle: PISC_DB_HANDLE;
                         tran_handle: PISC_TR_HANDLE;
                         isc_arg4: IntPtr): integer;
                         {$IFNDEF CLR}
                         {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                         {$ENDIF}

  TBLOB_text_dump = function (blob_ID: PISC_QUAD;
                              db_handle: PISC_DB_HANDLE;
                              tran_handle: PISC_TR_HANDLE;
                              isc_arg4: IntPtr): integer;
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}

  TBLOB_text_load = function (blob_ID: PISC_QUAD;
                              db_handle: TISC_DB_HANDLE;
                              tran_handle: TISC_TR_HANDLE;
                              isc_arg4: IntPtr): integer;
                              {$IFNDEF CLR}
                              {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                              {$ENDIF}

  TBopen = function (blob_ID: PISC_QUAD;
                     db_handle: PISC_DB_HANDLE;
                     tran_handle: PISC_TR_HANDLE;
                     open_mode: IntPtr): PBSTREAM;
                     {$IFNDEF CLR}
                     {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                     {$ENDIF}

  TBopen2 = function (blob_ID: PISC_QUAD;
                      db_handle: TISC_DB_HANDLE;
                      tran_handle: TISC_TR_HANDLE;
                      isc_arg4: IntPtr;
                      isc_arg5: UShort): PBSTREAM;
                      {$IFNDEF CLR}
                      {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                      {$ENDIF}

  Tisc_service_attach = function (status_vector: PISC_STATUS;
                                  isc_arg2: UShort;
                                  isc_arg3: IntPtr;
                                  service_handle: PISC_SVC_HANDLE;
                                  isc_arg5: UShort;
                                  isc_arg6: TBytes): ISC_STATUS;
                                  {$IFNDEF CLR}
                                  {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                  {$ENDIF}

  Tisc_service_detach = function (status_vector: PISC_STATUS;
                                  service_handle: PISC_SVC_HANDLE): ISC_STATUS;
                                  {$IFNDEF CLR}
                                  {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                  {$ENDIF}

  Tisc_service_query = function (status_vector: PISC_STATUS;
                                 service_handle: PISC_SVC_HANDLE;
                                 recv_handle: PISC_SVC_HANDLE;
                                 isc_arg4: UShort;
                                 isc_arg5: TBytes;
                                 isc_arg6: UShort;
                                 isc_arg7: TBytes;
                                 isc_arg8: UShort;
                                 isc_arg9: IntPtr): ISC_STATUS;
                                 {$IFNDEF CLR}
                                 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                 {$ENDIF}

  Tisc_service_start = function (status_vector: PISC_STATUS;
                                 service_handle: PISC_SVC_HANDLE;
                                 recv_handle: PISC_SVC_HANDLE;
                                 isc_arg4: UShort;
                                 isc_arg5: TBytes): ISC_STATUS;
                                 {$IFNDEF CLR}
                                 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
                                 {$ENDIF}

{ Actions to pass to the blob filter (ctl_source) }
const
  isc_blob_filter_open           =          0;
  isc_blob_filter_get_segment    =          1;
  isc_blob_filter_close          =          2;
  isc_blob_filter_create         =          3;
  isc_blob_filter_put_segment    =          4;
  isc_blob_filter_alloc          =          5;
  isc_blob_filter_free           =          6;
  isc_blob_filter_seek           =          7;

{ Blr definitions }

  blr_text                       =         14;
  blr_text2                      =         15;
  blr_short                      =          7;
  blr_long                       =          8;
  blr_quad                       =          9;
  blr_float                      =         10;
  blr_double                     =         27;
  blr_d_float                    =         11;
  blr_timestamp                  =         35;
  blr_varying                    =         37;
  blr_varying2                   =         38;
  blr_blob                       =        261;
  blr_cstring                    =         40;
  blr_cstring2                   =         41;
  blr_blob_id                    =         45;
  blr_sql_date                   =         12;
  blr_sql_time                   =         13;
  blr_int64                      =         16;
  blr_boolean_dtype              =         17;
  blr_date                       =         blr_timestamp;

  blr_inner                      =          0;
  blr_left                       =          1;
  blr_right                      =          2;
  blr_full                       =          3;

  blr_gds_code                   =          0;
  blr_sql_code                   =          1;
  blr_exception                  =          2;
  blr_trigger_code               =          3;
  blr_default_code               =          4;

  blr_version4                   =          4;
  blr_version5                   =          5;
  blr_eoc                        =         76;
  blr_end                        =         -1;

  blr_assignment                 =          1;
  blr_begin                      =          2;
  blr_dcl_variable               =          3;
  blr_message                    =          4;
  blr_erase                      =          5;
  blr_fetch                      =          6;
  blr_for                        =          7;
  blr_if                         =          8;
  blr_loop                       =          9;
  blr_modify                     =         10;
  blr_handler                    =         11;
  blr_receive                    =         12;
  blr_select                     =         13;
  blr_send                       =         14;
  blr_store                      =         15;
  blr_label                      =         17;
  blr_leave                      =         18;
  blr_store2                     =         19;
  blr_post                       =         20;

  blr_literal                    =         21;
  blr_dbkey                      =         22;
  blr_field                      =         23;
  blr_fid                        =         24;
  blr_parameter                  =         25;
  blr_variable                   =         26;
  blr_average                    =         27;
  blr_count                      =         28;
  blr_maximum                    =         29;
  blr_minimum                    =         30;
  blr_total                      =         31;
  blr_add                        =         34;
  blr_subtract                   =         35;
  blr_multiply                   =         36;
  blr_divide                     =         37;
  blr_negate                     =         38;
  blr_concatenate                =         39;
  blr_substring                  =         40;
  blr_parameter2                 =         41;
  blr_from                       =         42;
  blr_via                        =         43;
  blr_user_name                  =         44;
  blr_null                       =         45;

  blr_eql                        =         47;
  blr_neq                        =         48;
  blr_gtr                        =         49;
  blr_geq                        =         50;
  blr_lss                        =         51;
  blr_leq                        =         52;
  blr_containing                 =         53;
  blr_matching                   =         54;
  blr_starting                   =         55;
  blr_between                    =         56;
  blr_or                         =         57;
  blr_and                        =         58;
  blr_not                        =         59;
  blr_any                        =         60;
  blr_missing                    =         61;
  blr_unique                     =         62;
  blr_like                       =         63;

  blr_stream                     =         65;
  blr_set_index                  =         66;
  blr_rse                        =         67;
  blr_first                      =         68;
  blr_project                    =         69;
  blr_sort                       =         70;
  blr_boolean                    =         71;
  blr_ascending                  =         72;
  blr_descending                 =         73;
  blr_relation                   =         74;
  blr_rid                        =         75;
  blr_union                      =         76;
  blr_map                        =         77;
  blr_group_by                   =         78;
  blr_aggregate                  =         79;
  blr_join_type                  =         80;
  blr_rows                       =         81;

{ sub parameters for blr_rows }

  blr_ties                       =          0;
  blr_percent                    =          1;

  blr_agg_count                  =         83;
  blr_agg_max                    =         84;
  blr_agg_min                    =         85;
  blr_agg_total                  =         86;
  blr_agg_average                =         87;
  blr_parameter3                 =         88;
  blr_run_count                  =        118;
  blr_run_max                    =         89;
  blr_run_min                    =         90;
  blr_run_total                  =         91;
  blr_run_average                =         92;
  blr_agg_count2                 =         93;
  blr_agg_count_distinct         =         94;
  blr_agg_total_distinct         =         95;
  blr_agg_average_distinct       =         96;

  blr_function                   =        100;
  blr_gen_id                     =        101;
  blr_prot_mask                  =        102;
  blr_upcase                     =        103;
  blr_lock_state                 =        104;
  blr_value_if                   =        105;
  blr_matching2                  =        106;
  blr_index                      =        107;
  blr_ansi_like                  =        108;
  blr_bookmark                   =        109;
  blr_crack                      =        110;
  blr_force_crack                =        111;
  blr_seek                       =        112;
  blr_find                       =        113;

  blr_continue                   =          0;
  blr_forward                    =          1;
  blr_backward                   =          2;
  blr_bof_forward                =          3;
  blr_eof_backward               =          4;

  blr_lock_relation              =        114;
  blr_lock_record                =        115;
  blr_set_bookmark               =        116;
  blr_get_bookmark               =        117;
  blr_rs_stream                  =        119;
  blr_exec_proc                  =        120;
  blr_begin_range                =        121;
  blr_end_range                  =        122;
  blr_delete_range               =        123;
  blr_procedure                  =        124;
  blr_pid                        =        125;
  blr_exec_pid                   =        126;
  blr_singular                   =        127;
  blr_abort                      =        128;
  blr_block                      =        129;
  blr_error_handler              =        130;
  blr_cast                       =        131;
  blr_release_lock               =        132;
  blr_release_locks              =        133;
  blr_start_savepoint            =        134;
  blr_end_savepoint              =        135;
  blr_find_dbkey                 =        136;
  blr_range_relation             =        137;
  blr_delete_ranges              =        138;

  blr_plan                       =        139;
  blr_merge                      =        140;
  blr_join                       =        141;
  blr_sequential                 =        142;
  blr_navigational               =        143;
  blr_indices                    =        144;
  blr_retrieve                   =        145;

  blr_relation2                  =        146;
  blr_rid2                       =        147;
  blr_reset_stream               =        148;
  blr_release_bookmark           =        149;
  blr_set_generator              =        150;
  blr_ansi_any                   =        151;
  blr_exists                     =        152;
  blr_cardinality                =        153;

  blr_record_version             =        154;
  blr_stall                      =        155;
  blr_seek_no_warn               =        156;
  blr_find_dbkey_version         =        157;
  blr_ansi_all                   =        158;

  blr_extract                    =        159;

{ sub parameters for blr_extract }

  blr_extract_year               = 0;
  blr_extract_month              = 1;
  blr_extract_day	               = 2;
  blr_extract_hour               = 3;
  blr_extract_minute             = 4;
  blr_extract_second             = 5;
  blr_extract_weekday            = 6;
  blr_extract_yearday            = 7;

  blr_current_date               = 160;
  blr_current_timestamp          = 161;
  blr_current_time               = 162;

{ These verbs were added in 6.0        }
{ primarily to support 64-bit integers }

  blr_add2	                = 163;
  blr_subtract2	            = 164;
  blr_multiply2             = 165;
  blr_divide2	              = 166;
  blr_agg_total2            = 167;
  blr_agg_total_distinct2   = 168;
  blr_agg_average2          = 169;
  blr_agg_average_distinct2 = 170;
  blr_average2		          = 171;
  blr_gen_id2		            = 172;
  blr_set_generator2        = 173;

{ These verbs were added in 7.0 for BOOLEAN dtype supprt }

  blr_boolean_true  = 174;
  blr_boolean_false = 175;

{ These verbs were added in 7.1 for SQL savepoint support }

  blr_start_savepoint2      = 176;
  blr_release_savepoint     = 177;
  blr_rollback_savepoint    = 178;

{ Database parameter block stuff }

  isc_dpb_version1               =          1;
  isc_dpb_cdd_pathname           =          1;
  isc_dpb_allocation             =          2;
  isc_dpb_journal                =          3;
  isc_dpb_page_size              =          4;
  isc_dpb_num_buffers            =          5;
  isc_dpb_buffer_length          =          6;
  isc_dpb_debug                  =          7;
  isc_dpb_garbage_collect        =          8;
  isc_dpb_verify                 =          9;
  isc_dpb_sweep                  =         10;
  isc_dpb_enable_journal         =         11;
  isc_dpb_disable_journal        =         12;
  isc_dpb_dbkey_scope            =         13;
  isc_dpb_number_of_users        =         14;
  isc_dpb_trace                  =         15;
  isc_dpb_no_garbage_collect     =         16;
  isc_dpb_damaged                =         17;
  isc_dpb_license                =         18;
  isc_dpb_sys_user_name          =         19;
  isc_dpb_encrypt_key            =         20;
  isc_dpb_activate_shadow        =         21;
  isc_dpb_sweep_interval         =         22;
  isc_dpb_delete_shadow          =         23;
  isc_dpb_force_write            =         24;
  isc_dpb_begin_log              =         25;
  isc_dpb_quit_log               =         26;
  isc_dpb_no_reserve             =         27;
  isc_dpb_user_name              =         28;
  isc_dpb_password               =         29;
  isc_dpb_password_enc           =         30;
  isc_dpb_sys_user_name_enc      =         31;
  isc_dpb_interp                 =         32;
  isc_dpb_online_dump            =         33;
  isc_dpb_old_file_size          =         34;
  isc_dpb_old_num_files          =         35;
  isc_dpb_old_file               =         36;
  isc_dpb_old_start_page         =         37;
  isc_dpb_old_start_seqno        =         38;
  isc_dpb_old_start_file         =         39;
  isc_dpb_drop_walfile           =         40;
  isc_dpb_old_dump_id            =         41;
  isc_dpb_wal_backup_dir         =         42;
  isc_dpb_wal_chkptlen           =         43;
  isc_dpb_wal_numbufs            =         44;
  isc_dpb_wal_bufsize            =         45;
  isc_dpb_wal_grp_cmt_wait       =         46;
  isc_dpb_lc_messages            =         47;
  isc_dpb_lc_ctype               =         48;
  isc_dpb_cache_manager          =         49;
  isc_dpb_shutdown               =         50;
  isc_dpb_online                 =         51;
  isc_dpb_shutdown_delay         =         52;
  isc_dpb_reserved               =         53;
  isc_dpb_overwrite              =         54;
  isc_dpb_sec_attach             =         55;
  isc_dpb_disable_wal            =         56;
  isc_dpb_connect_timeout        =         57;
  isc_dpb_dummy_packet_interval  =         58;
  isc_dpb_gbak_attach            =         59;
  isc_dpb_sql_role_name          =         60;
  isc_dpb_set_page_buffers       =         61;
  isc_dpb_working_directory      =         62;
  isc_dpb_SQL_dialect            =         63;
  isc_dpb_set_db_readonly        =         64;
  isc_dpb_set_db_SQL_dialect     =         65;
  isc_dpb_gfix_attach		         =         66;
  isc_dpb_gstat_attach      		 =         67;
//  isc_dpb_last_dpb_constant      =         isc_dpb_gstat_attach;
  isc_dpb_gbak_ods_version       =         68;
  isc_dpb_gbak_ods_minor_version =         69;
  isc_dpb_set_group_commit	     =         70;
  isc_dpb_gbak_validate          =         71;
  isc_dpb_client_interbase_var	 =         72;
  isc_dpb_admin_option           =         73;
//  isc_dpb_flush_interval         =         74;
  isc_dpb_process_name           =         74;
  isc_dpb_instance_name	  	     =         75;
  isc_dpb_last_dpb_constant      =         isc_dpb_instance_name;

{ isc_dpb_verify specific flags }

  isc_dpb_pages                  =          1;
  isc_dpb_records                =          2;
  isc_dpb_indices                =          4;
  isc_dpb_transactions           =          8;
  isc_dpb_no_update              =         16;
  isc_dpb_repair                 =         32;
  isc_dpb_ignore                 =         64;

{ isc_dpb_shutdown specific flags }

  isc_dpb_shut_cache             =          1;
  isc_dpb_shut_attachment        =          2;
  isc_dpb_shut_transaction       =          4;
  isc_dpb_shut_force             =          8;

{ Bit assignments in RDB$SYSTEM_FLAG }

  RDB_system                     =          1;
  RDB_id_assigned                =          2;

{ Transaction parameter block stuff }

  isc_tpb_version1               =          1;
  isc_tpb_version3               =          3;
  isc_tpb_consistency            =          1;
  isc_tpb_concurrency            =          2;
  isc_tpb_shared                 =          3;
  isc_tpb_protected              =          4;
  isc_tpb_exclusive              =          5;
  isc_tpb_wait                   =          6;
  isc_tpb_nowait                 =          7;
  isc_tpb_read                   =          8;
  isc_tpb_write                  =          9;
  isc_tpb_lock_read              =         10;
  isc_tpb_lock_write             =         11;
  isc_tpb_verb_time              =         12;
  isc_tpb_commit_time            =         13;
  isc_tpb_ignore_limbo           =         14;
  isc_tpb_read_committed         =         15;
  isc_tpb_autocommit             =         16;
  isc_tpb_rec_version            =         17;
  isc_tpb_no_rec_version         =         18;
  isc_tpb_restart_requests       =         19;
  isc_tpb_no_auto_undo           =         20;
  isc_tpb_no_savepoint           =         21;
  isc_tpb_lock_timeout           =         21; //FB 2 Specific feature
  unique_isc_tpb_lock_timeout    =         22;
  isc_tpb_last_tpb_constant      =         unique_isc_tpb_lock_timeout;

{ Blob Parameter Block }

  isc_bpb_version1               =          1;
  isc_bpb_source_type            =          1;
  isc_bpb_target_type            =          2;
  isc_bpb_type                   =          3;
  isc_bpb_source_interp          =          4;
  isc_bpb_target_interp          =          5;
  isc_bpb_filter_parameter       =          6;
  isc_bpb_last_bpb_constant      =          isc_bpb_filter_parameter;

  isc_bpb_type_segmented         =          0;
  isc_bpb_type_stream            =          1;

{ Service parameter block stuff }

  isc_spb_user_name              =          1;
  isc_spb_sys_user_name          =          2;
  isc_spb_sys_user_name_enc      =          3;
  isc_spb_password               =          4;
  isc_spb_password_enc           =          5;
  isc_spb_command_line           =          6;
  isc_spb_dbname                 =          7;
  isc_spb_verbose                =          8;
  isc_spb_options                =          9;
  isc_spb_connect_timeout        =          10;
  isc_spb_dummy_packet_interval  =          11;
  isc_spb_sql_role_name          =          12;
  isc_spb_instance_name          =          13;
  isc_spb_last_spb_constant      =          isc_spb_instance_name;

  isc_spb_version1                                = 1;
  isc_spb_current_version                         = 2;
  isc_spb_version		                              = isc_spb_current_version;
  isc_spb_user_name_mapped_to_server              = isc_dpb_user_name;
  isc_spb_sys_user_name_mapped_to_server          = isc_dpb_sys_user_name;
  isc_spb_sys_user_name_enc_mapped_to_server      = isc_dpb_sys_user_name_enc;
  isc_spb_password_mapped_to_server               = isc_dpb_password;
  isc_spb_password_enc_mapped_to_server           = isc_dpb_password_enc;
  isc_spb_command_line_mapped_to_server           = 105;
  isc_spb_dbname_mapped_to_server                 = 106;
  isc_spb_verbose_mapped_to_server                = 107;
  isc_spb_options_mapped_to_server                = 108;
  isc_spb_user_dbname_mapped_tp_server            = 109;
  isc_spb_connect_timeout_mapped_to_server        = isc_dpb_connect_timeout;
  isc_spb_dummy_packet_interval_mapped_to_server  = isc_dpb_dummy_packet_interval;
  isc_spb_sql_role_name_mapped_to_server          = isc_dpb_sql_role_name;
  isc_spb_instance_name_mapped_to_server          = isc_dpb_instance_name;

{ Information call declarations }

{ Common, structural codes }

  isc_info_end                   =          1;
  isc_info_truncated             =          2;
  isc_info_error                 =          3;
  isc_info_data_not_ready	       =          4;
  isc_info_flag_end		           =          127;

{ Database information items }

  isc_info_db_id                 =          4;
  isc_info_reads                 =          5;
  isc_info_writes                =          6;
  isc_info_fetches               =          7;
  isc_info_marks                 =          8;
  isc_info_implementation        =         11;
  isc_info_version               =         12;
  isc_info_base_level            =         13;
  isc_info_svr_maj_ver           =         isc_info_base_level;
  isc_info_page_size             =         14;
  isc_info_num_buffers           =         15;
  isc_info_limbo                 =         16;
  isc_info_current_memory        =         17;
  isc_info_max_memory            =         18;
  isc_info_window_turns          =         19;
  isc_info_license               =         20;
  isc_info_allocation            =         21;
  isc_info_attachment_id         =         22;
  isc_info_read_seq_count        =         23;
  isc_info_read_idx_count        =         24;
  isc_info_insert_count          =         25;
  isc_info_update_count          =         26;
  isc_info_delete_count          =         27;
  isc_info_backout_count         =         28;
  isc_info_purge_count           =         29;
  isc_info_expunge_count         =         30;
  isc_info_sweep_interval        =         31;
  isc_info_ods_version           =         32;
  isc_info_ods_minor_version     =         33;
  isc_info_no_reserve            =         34;
  isc_info_logfile               =         35;
  isc_info_cur_logfile_name      =         36;
  isc_info_cur_log_part_offset   =         37;
  isc_info_num_wal_buffers       =         38;
  isc_info_wal_buffer_size       =         39;
  isc_info_wal_ckpt_length       =         40;
  isc_info_wal_cur_ckpt_interval =         41;
  isc_info_wal_prv_ckpt_fname    =         42;
  isc_info_wal_prv_ckpt_poffset  =         43;
  isc_info_wal_recv_ckpt_fname   =         44;
  isc_info_wal_recv_ckpt_poffset =         45;
  isc_info_wal_grpc_wait_usecs   =         47;
  isc_info_wal_num_io            =         48;
  isc_info_wal_avg_io_size       =         49;
  isc_info_wal_num_commits       =         50;
  isc_info_wal_avg_grpc_size     =         51;
  isc_info_forced_writes         =         52;
  isc_info_user_names            =         53;
  isc_info_page_errors           =         54;
  isc_info_record_errors         =         55;
  isc_info_bpage_errors          =         56;
  isc_info_dpage_errors          =         57;
  isc_info_ipage_errors          =         58;
  isc_info_ppage_errors          =         59;
  isc_info_tpage_errors          =         60;
  isc_info_set_page_buffers      =         61;
  isc_info_db_SQL_dialect        =         62;
  isc_info_db_read_only          =         63;
  isc_info_db_size_in_pages      =         64;
  isc_info_db_reads              =         65;
  isc_info_db_writes             =         66;
  isc_info_db_fetches            =         67;
  isc_info_db_marks              =         68;
  isc_info_db_group_commit       =         69;
  isc_info_att_charset           =         70;
  isc_info_svr_min_ver           =         71;
  isc_info_ib_env_var            =         72;
  isc_info_server_tcp_port       =         73;
  isc_info_db_preallocate        =         74;
  isc_info_db_encrypted          =         75;
  isc_info_db_eua_active         =         76;

  frb_info_att_charset           =         101;
  frb_info_db_class              =         102;
  frb_info_firebird_version      =         103;
  frb_info_oldest_transaction    =         104;
  frb_info_oldest_active         =         105;
  frb_info_oldest_snapshot       =         106;
  frb_info_next_transaction      =         107;
  frb_info_db_provider           =         108;
  frb_info_active_transactions   =         109;


{ Database information return values }

  isc_info_db_impl_rdb_vms       =          1;
  isc_info_db_impl_rdb_eln       =          2;
  isc_info_db_impl_rdb_eln_dev   =          3;
  isc_info_db_impl_rdb_vms_y     =          4;
  isc_info_db_impl_rdb_eln_y     =          5;
  isc_info_db_impl_jri           =          6;
  isc_info_db_impl_jsv           =          7;
  isc_info_db_impl_isc_a         =         25;
  isc_info_db_impl_isc_u         =         26;
  isc_info_db_impl_isc_v         =         27;
  isc_info_db_impl_isc_s         =         28;
  isc_info_db_impl_isc_apl_68K   =         25;
  isc_info_db_impl_isc_vax_ultr  =         26;
  isc_info_db_impl_isc_vms       =         27;
  isc_info_db_impl_isc_sun_68k   =         28;
  isc_info_db_impl_isc_os2       =         29;
  isc_info_db_impl_isc_sun4      =         30;
  isc_info_db_impl_isc_hp_ux     =         31;
  isc_info_db_impl_isc_sun_386i  =         32;
  isc_info_db_impl_isc_vms_orcl  =         33;
  isc_info_db_impl_isc_mac_aux   =         34;
  isc_info_db_impl_isc_rt_aix    =         35;
  isc_info_db_impl_isc_mips_ult  =         36;
  isc_info_db_impl_isc_xenix     =         37;
  isc_info_db_impl_isc_dg        =         38;
  isc_info_db_impl_isc_hp_mpexl  =         39;
  isc_info_db_impl_isc_hp_ux68K  =         40;
  isc_info_db_impl_isc_sgi       =         41;
  isc_info_db_impl_isc_sco_unix  =         42;
  isc_info_db_impl_isc_cray      =         43;
  isc_info_db_impl_isc_imp       =         44;
  isc_info_db_impl_isc_delta     =         45;
  isc_info_db_impl_isc_next      =         46;
  isc_info_db_impl_isc_dos       =         47;
  isc_info_db_impl_isc_winnt     =         48;
  isc_info_db_impl_isc_epson     =         49;

  isc_info_db_class_access       =          1;
  isc_info_db_class_y_valve      =          2;
  isc_info_db_class_rem_int      =          3;
  isc_info_db_class_rem_srvr     =          4;
  isc_info_db_class_pipe_int     =          7;
  isc_info_db_class_pipe_srvr    =          8;
  isc_info_db_class_sam_int      =          9;
  isc_info_db_class_sam_srvr     =         10;
  isc_info_db_class_gateway      =         11;
  isc_info_db_class_cache        =         12;

{ Request information items }

  isc_info_number_messages       =          4;
  isc_info_max_message           =          5;
  isc_info_max_send              =          6;
  isc_info_max_receive           =          7;
  isc_info_state                 =          8;
  isc_info_message_number        =          9;
  isc_info_message_size          =         10;
  isc_info_request_cost          =         11;
  isc_info_access_path           =         12;
  isc_info_req_select_count      =         13;
  isc_info_req_insert_count      =         14;
  isc_info_req_update_count      =         15;
  isc_info_req_delete_count      =         16;


{ Access path items }

  isc_info_rsb_end               =          0;
  isc_info_rsb_begin             =          1;
  isc_info_rsb_type              =          2;
  isc_info_rsb_relation          =          3;
  isc_info_rsb_plan              =          4;

{ Rsb types }

  isc_info_rsb_unknown           =          1;
  isc_info_rsb_indexed           =          2;
  isc_info_rsb_navigate          =          3;
  isc_info_rsb_sequential        =          4;
  isc_info_rsb_cross             =          5;
  isc_info_rsb_sort              =          6;
  isc_info_rsb_first             =          7;
  isc_info_rsb_boolean           =          8;
  isc_info_rsb_union             =          9;
  isc_info_rsb_aggregate         =         10;
  isc_info_rsb_merge             =         11;
  isc_info_rsb_ext_sequential    =         12;
  isc_info_rsb_ext_indexed       =         13;
  isc_info_rsb_ext_dbkey         =         14;
  isc_info_rsb_left_cross        =         15;
  isc_info_rsb_select            =         16;
  isc_info_rsb_sql_join          =         17;
  isc_info_rsb_simulate          =         18;
  isc_info_rsb_sim_cross         =         19;
  isc_info_rsb_once              =         20;
  isc_info_rsb_procedure         =         21;

{ Bitmap expressions }

  isc_info_rsb_and               =          1;
  isc_info_rsb_or                =          2;
  isc_info_rsb_dbkey             =          3;
  isc_info_rsb_index             =          4;

  isc_info_req_active            =          2;
  isc_info_req_inactive          =          3;
  isc_info_req_send              =          4;
  isc_info_req_receive           =          5;
  isc_info_req_select            =          6;
  isc_info_req_sql_stall         =          7;

{ Blob information items }

  isc_info_blob_num_segments     =          4;
  isc_info_blob_max_segment      =          5;
  isc_info_blob_total_length     =          6;
  isc_info_blob_type             =          7;

{ Transaction information items }

  isc_info_tra_id                =          4;

{ Service action items }

  //isc_action_min                  = 1;

  isc_action_svc_backup           = 1; 
  isc_action_svc_restore          = 2; 
  isc_action_svc_repair           = 3; 
  isc_action_svc_add_user         = 4; 
  isc_action_svc_delete_user      = 5; 
  isc_action_svc_modify_user      = 6; 
  isc_action_svc_display_user     = 7; 
  isc_action_svc_properties       = 8; 
  isc_action_svc_add_license      = 9; 
  isc_action_svc_remove_license   = 10;
  isc_action_svc_db_stats	        = 11;
  isc_action_svc_get_ib_log       = 12;
  isc_action_svc_add_db_alias     = 13;
  isc_action_svc_delete_db_alias  = 14;
  isc_action_svc_display_db_alias = 15;

  isc_action_svc_nbak             = 20;
  isc_action_svc_nrest            = 21;
  isc_action_svc_trace_start      = 22;
  isc_action_svc_trace_stop       = 23;
  isc_action_svc_trace_suspend    = 24;
  isc_action_svc_trace_resume     = 25;
  isc_action_svc_trace_list       = 26;
  isc_action_svc_set_mapping      = 27;
  isc_action_svc_drop_mapping     = 28;

  //isc_action_max                  = 16;

{ Service information items }

  isc_info_svc_svr_db_info        = 50;
  isc_info_svc_get_license        = 51;
  isc_info_svc_get_license_mask   = 52;
  isc_info_svc_get_config         = 53;
  isc_info_svc_version            = 54;
  isc_info_svc_server_version     = 55;
  isc_info_svc_implementation     = 56;
  isc_info_svc_capabilities       = 57;
  isc_info_svc_user_dbpath        = 58;
  isc_info_svc_get_env	          = 59;
  isc_info_svc_get_env_lock       = 60;
  isc_info_svc_get_env_msg        = 61;
  isc_info_svc_line               = 62;
  isc_info_svc_to_eof             = 63;
  isc_info_svc_timeout            = 64;
  isc_info_svc_get_licensed_users = 65;
  isc_info_svc_limbo_trans	      = 66;
  isc_info_svc_running		        = 67;
  isc_info_svc_get_users      	  = 68;
  isc_info_svc_get_db_alias	      = 69;

{ Parameters for isc_action_{add|delete|modify)_user }

  isc_spb_sec_userid              = 5;
  isc_spb_sec_groupid             = 6;
  isc_spb_sec_username            = 7;
  isc_spb_sec_password            = 8;
  isc_spb_sec_groupname           = 9;
  isc_spb_sec_firstname           = 10;
  isc_spb_sec_middlename          = 11;
  isc_spb_sec_lastname            = 12;

{ Parameters for isc_action_{add|delete|display)_db_alias }

  isc_spb_sec_db_alias_name       = 20;
  isc_spb_sec_db_alias_dbpath     = 21;


{ Parameters for isc_action_svc_(add|remove)_license, }
{ isc_info_svc_get_license                            }

  isc_spb_lic_key                 = 5;
  isc_spb_lic_id                  = 6;
  isc_spb_lic_desc                = 7;

{ Parameters for isc_action_svc_backup }

  isc_spb_bkp_file                = 5;
  isc_spb_bkp_factor              = 6;
  isc_spb_bkp_length              = 7;
  isc_spb_bkp_ignore_checksums    = $01;
  isc_spb_bkp_ignore_limbo        = $02;
  isc_spb_bkp_metadata_only       = $04;
  isc_spb_bkp_no_garbage_collect  = $08;
  isc_spb_bkp_old_descriptions    = $10;
  isc_spb_bkp_non_transportable   = $20;
  isc_spb_bkp_convert             = $40;
  isc_spb_bkp_expand              = $80;

{ Parameters for isc_action_svc_properties }

  isc_spb_prp_page_buffers	        = 5;
  isc_spb_prp_sweep_interval	      = 6;
  isc_spb_prp_shutdown_db	          =	7;
  isc_spb_prp_deny_new_attachments  = 9;
  isc_spb_prp_deny_new_transactions = 10;
  isc_spb_prp_reserve_space	        = 11;
  isc_spb_prp_write_mode	          =	12;
  isc_spb_prp_access_mode	          =	13;
  isc_spb_prp_set_sql_dialect	      = 14;
  isc_spb_prp_activate		          = $0100;
  isc_spb_prp_db_online		          = $0200;

{ Parameters for isc_spb_prp_reserve_space }

  isc_spb_prp_res_use_full	      = 35;
  isc_spb_prp_res		              =	36;

{ Parameters for isc_spb_prp_write_mode  }

  isc_spb_prp_wm_async		= 37;
  isc_spb_prp_wm_sync		  = 38;

{ Parameters for isc_spb_prp_access_mode }

  isc_spb_prp_am_readonly	 = 39;
  isc_spb_prp_am_readwrite = 40;

{ Parameters for isc_action_svc_repair }

  isc_spb_rpr_commit_trans	       = 15;
  isc_spb_rpr_rollback_trans	     = 34;
  isc_spb_rpr_recover_two_phase	   = 17;
  isc_spb_tra_id                   = 18;
  isc_spb_single_tra_id		         = 19;
  isc_spb_multi_tra_id		         = 20;
  isc_spb_tra_state		             = 21;
  isc_spb_tra_state_limbo	         = 22;
  isc_spb_tra_state_commit	       = 23;
  isc_spb_tra_state_rollback	     = 24;
  isc_spb_tra_state_unknown	       = 25;
  isc_spb_tra_host_site		         = 26;
  isc_spb_tra_remote_site	         = 27;
  isc_spb_tra_db_path		           = 28;
  isc_spb_tra_advise		           = 29;
  isc_spb_tra_advise_commit	       = 30;
  isc_spb_tra_advise_rollback	     = 31;
  isc_spb_tra_advise_unknown	     = 33;
  isc_spb_rpr_validate_db	         = $01;
  isc_spb_rpr_sweep_db		         = $02;
  isc_spb_rpr_mend_db		           = $04;
  isc_spb_rpr_list_limbo_trans	   = $08;
  isc_spb_rpr_check_db		         = $10;
  isc_spb_rpr_ignore_checksum	     = $20;
  isc_spb_rpr_kill_shadows	       = $40;
  isc_spb_rpr_full		             = $80;

{ Parameters for isc_action_svc_restore }

  isc_spb_res_buffers		       = 9;
  isc_spb_res_page_size		     = 10;
  isc_spb_res_length		       = 11;
  isc_spb_res_access_mode	     = 12;
  isc_spb_res_deactivate_idx	 = $0100;
  isc_spb_res_no_shadow		     = $0200;
  isc_spb_res_no_validity	     = $0400;
  isc_spb_res_one_at_a_time	   = $0800;
  isc_spb_res_replace		       = $1000;
  isc_spb_res_create		       = $2000;
  isc_spb_res_use_all_space	   = $4000;
  isc_spb_res_validate   	   	 = $8000;
  
{ Parameters for isc_spb_res_access_mode }

  isc_spb_res_am_readonly		= isc_spb_prp_am_readonly;
  isc_spb_res_am_readwrite	= isc_spb_prp_am_readwrite;

{ Parameters for isc_info_svc_svr_db_info }

  isc_spb_num_att               = 5;
  isc_spb_num_db                = 6;

{ Parameters for isc_info_svc_db_stats }

  isc_spb_sts_data_pages	    = $01;
  isc_spb_sts_db_log		      = $02;
  isc_spb_sts_hdr_pages		    = $04;
  isc_spb_sts_idx_pages		    = $08;
  isc_spb_sts_sys_relations	  = $10;
  isc_spb_sts_record_versions	= $20;
  isc_spb_sts_table	          = $40;

{ Parameters for isc_action_svc_nbak }

  isc_spb_nbk_level           = 5;
  isc_spb_nbk_file            = 6;
  isc_spb_nbk_no_triggers     = 1;

{ Parameters for isc_action_svc_trace_* }

  isc_spb_trc_id              = 1;
  isc_spb_trc_name            = 2;
  isc_spb_trc_cfg             = 3;

{ SQL information items }

  isc_info_sql_select            =  4;
  isc_info_sql_bind              =  5;
  isc_info_sql_num_variables     =  6;
  isc_info_sql_describe_vars     =  7;
  isc_info_sql_describe_end      =  8;
  isc_info_sql_sqlda_seq         =  9;
  isc_info_sql_message_seq       = 10;
  isc_info_sql_type              = 11;
  isc_info_sql_sub_type          = 12;
  isc_info_sql_scale             = 13;
  isc_info_sql_length            = 14;
  isc_info_sql_null_ind          = 15;
  isc_info_sql_field             = 16;
  isc_info_sql_relation          = 17;
  isc_info_sql_owner             = 18;
  isc_info_sql_alias             = 19;
  isc_info_sql_sqlda_start       = 20;
  isc_info_sql_stmt_type         = 21;
  isc_info_sql_get_plan          = 22;
  isc_info_sql_records           = 23;
  isc_info_sql_batch_fetch       = 24;
  isc_info_sql_precision         = 25;

{ SQL information return values }

  isc_info_sql_stmt_select           =  1;
  isc_info_sql_stmt_insert           =  2;
  isc_info_sql_stmt_update           =  3;
  isc_info_sql_stmt_delete           =  4;
  isc_info_sql_stmt_ddl              =  5;
  isc_info_sql_stmt_get_segment      =  6;
  isc_info_sql_stmt_put_segment      =  7;
  isc_info_sql_stmt_exec_procedure   =  8;
  isc_info_sql_stmt_start_trans      =  9;
  isc_info_sql_stmt_commit           = 10;
  isc_info_sql_stmt_rollback         = 11;
  isc_info_sql_stmt_select_for_upd   = 12;
  isc_info_sql_stmt_set_generator    = 13;

{ Server configuration key values }

  ISCCFG_LOCKMEM_KEY             =   0;
  ISCCFG_LOCKSEM_KEY             =   1;
  ISCCFG_LOCKSIG_KEY             =   2;
  ISCCFG_EVNTMEM_KEY             =   3;
  ISCCFG_DBCACHE_KEY             =   4;
  ISCCFG_PRIORITY_KEY            =   5;
  ISCCFG_IPCMAP_KEY              =   6;
  ISCCFG_MEMMIN_KEY              =   7;
  ISCCFG_MEMMAX_KEY              =   8;
  ISCCFG_LOCKORDER_KEY           =   9;
  ISCCFG_ANYLOCKMEM_KEY          =  10;
  ISCCFG_ANYLOCKSEM_KEY          =  11;
  ISCCFG_ANYLOCKSIG_KEY          =  12;
  ISCCFG_ANYEVNTMEM_KEY          =  13;
  ISCCFG_LOCKHASH_KEY            =  14;
  ISCCFG_DEADLOCK_KEY            =  15;
  ISCCFG_LOCKSPIN_KEY            =  16;
  ISCCFG_CONN_TIMEOUT_KEY        =  17;
  ISCCFG_DUMMY_INTRVL_KEY        =  18;
  ISCCFG_TRACE_POOLS_KEY         =  19;
  ISCCFG_REMOTE_BUFFER_KEY	     =  20;
  ISCCFG_CPU_AFFINITY_KEY	       =  21;
  ISCCFG_SWEEP_QUANTUM_KEY	     =  22;
  ISCCFG_USER_QUANTUM_KEY	       =  23;
  ISCCFG_SLEEP_TIME_KEY	         =  24;
  ISCCFG_MAX_THREADS_KEY	       =  25;
  ISCCFG_ADMIN_DB_KEY	           =  26;
  ISCCFG_USE_SANCTUARY_KEY    	 =  27;
  ISCCFG_ENABLE_HT_KEY	         =  28;
  ISCCFG_USE_ROUTER_KEY	         =  29;
  ISCCFG_SORTMEM_BUFFER_SIZE_KEY =  30;
  ISCCFG_SQL_CMP_RECURSION_KEY	 =  31;
  ISCCFG_SOL_BOUND_THREADS_KEY	 =  32;
  ISCCFG_SOL_SYNC_SCOPE_KEY	     =  33;


{ Error codes }

  isc_facility                   =         20;
  isc_err_base                   =  335544320;
  isc_err_factor                 =          1;
  isc_arg_end                    =          0;
  isc_arg_gds                    =          1;
  isc_arg_string                 =          2;
  isc_arg_cstring                =          3;
  isc_arg_number                 =          4;
  isc_arg_interpreted            =          5;
  isc_arg_vms                    =          6;
  isc_arg_unix                   =          7;
  isc_arg_domain                 =          8;
  isc_arg_dos                    =          9;
  isc_arg_mpexl                  =         10;
  isc_arg_mpexl_ipc              =         11;
  isc_arg_next_mach              =         15;
  isc_arg_netware                =         16;
  isc_arg_win32                  =         17;
  isc_arg_warning                =         18;

{ Dynamic Data Definition Language operators }

{ Version number }

  isc_dyn_version_1              =          1;
  isc_dyn_eoc                    =         -1;

{ Operations (may be nested) }

  isc_dyn_begin                  =          2;
  isc_dyn_end                    =          3;
  isc_dyn_if                     =          4;
  isc_dyn_def_database           =          5;
  isc_dyn_def_global_fld         =          6;
  isc_dyn_def_local_fld          =          7;
  isc_dyn_def_idx                =          8;
  isc_dyn_def_rel                =          9;
  isc_dyn_def_sql_fld            =         10;
  isc_dyn_def_view               =         12;
  isc_dyn_def_trigger            =         15;
  isc_dyn_def_security_class     =        120;
  isc_dyn_def_dimension          =        140;
  isc_dyn_def_generator          =         24;
  isc_dyn_def_function           =         25;
  isc_dyn_def_filter             =         26;
  isc_dyn_def_function_arg       =         27;
  isc_dyn_def_shadow             =         34;
  isc_dyn_def_trigger_msg        =         17;
  isc_dyn_def_file               =         36;
  isc_dyn_mod_database           =         39;
  isc_dyn_mod_rel                =         11;
  isc_dyn_mod_global_fld         =         13;
  isc_dyn_mod_idx                =        102;
  isc_dyn_mod_local_fld          =         14;
  isc_dyn_mod_sql_fld            =        216;
  isc_dyn_mod_view               =         16;
  isc_dyn_mod_security_class     =        122;
  isc_dyn_mod_trigger            =        113;
  isc_dyn_mod_trigger_msg        =         28;
  isc_dyn_delete_database        =         18;
  isc_dyn_delete_rel             =         19;
  isc_dyn_delete_global_fld      =         20;
  isc_dyn_delete_local_fld       =         21;
  isc_dyn_delete_idx             =         22;
  isc_dyn_delete_security_class  =        123;
  isc_dyn_delete_dimensions      =        143;
  isc_dyn_delete_trigger         =         23;
  isc_dyn_delete_trigger_msg     =         29;
  isc_dyn_delete_filter          =         32;
  isc_dyn_delete_function        =         33;
  isc_dyn_delete_generator       =        217;
  isc_dyn_delete_shadow          =         35;
  isc_dyn_grant                  =         30;
  isc_dyn_revoke                 =         31;
  isc_dyn_def_primary_key        =         37;
  isc_dyn_def_foreign_key        =         38;
  isc_dyn_def_unique             =         40;
  isc_dyn_def_procedure          =        164;
  isc_dyn_delete_procedure       =        165;
  isc_dyn_def_parameter          =        135;
  isc_dyn_delete_parameter       =        136;
  isc_dyn_mod_procedure          =        175;
  isc_dyn_def_log_file           =        176;
  isc_dyn_def_cache_file         =        180;
  isc_dyn_def_exception          =        181;
  isc_dyn_mod_exception          =        182;
  isc_dyn_del_exception          =        183;
  isc_dyn_drop_log               =        194;
  isc_dyn_drop_cache             =        195;
  isc_dyn_def_default_log        =        202;

{ View specific stuff }

  isc_dyn_view_blr               =         43;
  isc_dyn_view_source            =         44;
  isc_dyn_view_relation          =         45;
  isc_dyn_view_context           =         46;
  isc_dyn_view_context_name      =         47;

{ Generic attributes }

  isc_dyn_rel_name               =         50;
  isc_dyn_fld_name               =         51;
  isc_dyn_new_fld_name           =        215;
  isc_dyn_idx_name               =         52;
  isc_dyn_description            =         53;
  isc_dyn_security_class         =         54;
  isc_dyn_system_flag            =         55;
  isc_dyn_update_flag            =         56;
  isc_dyn_prc_name               =        166;
  isc_dyn_prm_name               =        137;
  isc_dyn_sql_object             =        196;
  isc_dyn_fld_character_set_name =        174;
  isc_dyn_restrict_or_cascade    =        220;

{ Relation specific attributes }

  isc_dyn_rel_dbkey_length       =         61;
  isc_dyn_rel_store_trig         =         62;
  isc_dyn_rel_modify_trig        =         63;
  isc_dyn_rel_erase_trig         =         64;
  isc_dyn_rel_store_trig_source  =         65;
  isc_dyn_rel_modify_trig_source =         66;
  isc_dyn_rel_erase_trig_source  =         67;
  isc_dyn_rel_ext_file           =         68;
  isc_dyn_rel_sql_protection     =         69;
  isc_dyn_rel_constraint         =        162;
  isc_dyn_delete_rel_constraint  =        163;
  isc_dyn_rel_sql_scope          =        218;
  isc_dyn_rel_sql_on_commit      =        219;

{ Global field specific attributes }

  isc_dyn_fld_type               =         70;
  isc_dyn_fld_length             =         71;
  isc_dyn_fld_scale              =         72;
  isc_dyn_fld_sub_type           =         73;
  isc_dyn_fld_segment_length     =         74;
  isc_dyn_fld_query_header       =         75;
  isc_dyn_fld_edit_string        =         76;
  isc_dyn_fld_validation_blr     =         77;
  isc_dyn_fld_validation_source  =         78;
  isc_dyn_fld_computed_blr       =         79;
  isc_dyn_fld_computed_source    =         80;
  isc_dyn_fld_missing_value      =         81;
  isc_dyn_fld_default_value      =         82;
  isc_dyn_fld_query_name         =         83;
  isc_dyn_fld_dimensions         =         84;
  isc_dyn_fld_not_null           =         85;
  isc_dyn_fld_precision          =         86;
  isc_dyn_fld_char_length        =        172;
  isc_dyn_fld_collation          =        173;
  isc_dyn_fld_default_source     =        193;
  isc_dyn_del_default            =        197;
  isc_dyn_del_validation         =        198;
  isc_dyn_single_validation      =        199;
  isc_dyn_fld_character_set      =        203;

{ Local field specific attributes }

  isc_dyn_fld_source             =         90;
  isc_dyn_fld_base_fld           =         91;
  isc_dyn_fld_position           =         92;
  isc_dyn_fld_update_flag        =         93;

{ Index specific attributes }

  isc_dyn_idx_unique             =        100;
  isc_dyn_idx_inactive           =        101;
  isc_dyn_idx_type               =        103;
  isc_dyn_idx_foreign_key        =        104;
  isc_dyn_idx_ref_column         =        105;
  isc_dyn_idx_statistic          =        204;

{ Trigger specific attributes }

  isc_dyn_trg_type               =        110;
  isc_dyn_trg_blr                =        111;
  isc_dyn_trg_source             =        112;
  isc_dyn_trg_name               =        114;
  isc_dyn_trg_sequence           =        115;
  isc_dyn_trg_inactive           =        116;
  isc_dyn_trg_msg_number         =        117;
  isc_dyn_trg_msg                =        118;

{ Security Class specific attributes }

  isc_dyn_scl_acl                =        121;
  isc_dyn_grant_user             =        130;
  isc_dyn_grant_proc             =        186;
  isc_dyn_grant_trig             =        187;
  isc_dyn_grant_view             =        188;
  isc_dyn_grant_options          =        132;
  isc_dyn_grant_user_group       =        205;

{ Dimension specific information }

  isc_dyn_dim_lower              =        141;
  isc_dyn_dim_upper              =        142;

{ File specific attributes }

  isc_dyn_file_name              =        125;
  isc_dyn_file_start             =        126;
  isc_dyn_file_length            =        127;
  isc_dyn_shadow_number          =        128;
  isc_dyn_shadow_man_auto        =        129;
  isc_dyn_shadow_conditional     =        130;

{ Log file specific attributes }

  isc_dyn_log_file_sequence      =        177;
  isc_dyn_log_file_partitions    =        178;
  isc_dyn_log_file_serial        =        179;
  isc_dyn_log_file_overflow      =        200;
  isc_dyn_log_file_raw           =        201;

{ Log specific attributes }

  isc_dyn_log_group_commit_wait  =        189;
  isc_dyn_log_buffer_size        =        190;
  isc_dyn_log_check_point_length =        191;
  isc_dyn_log_num_of_buffers     =        192;

{ Function specific attributes }

  isc_dyn_function_name          =        145;
  isc_dyn_function_type          =        146;
  isc_dyn_func_module_name       =        147;
  isc_dyn_func_entry_point       =        148;
  isc_dyn_func_return_argument   =        149;
  isc_dyn_func_arg_position      =        150;
  isc_dyn_func_mechanism         =        151;
  isc_dyn_filter_in_subtype      =        152;
  isc_dyn_filter_out_subtype     =        153;

  isc_dyn_description2           =        154;
  isc_dyn_fld_computed_source2   =        155;
  isc_dyn_fld_edit_string2       =        156;
  isc_dyn_fld_query_header2      =        157;
  isc_dyn_fld_validation_source2 =        158;
  isc_dyn_trg_msg2               =        159;
  isc_dyn_trg_source2            =        160;
  isc_dyn_view_source2           =        161;
  isc_dyn_xcp_msg2               =        184;

{ Generator specific attributes }

  isc_dyn_generator_name         =         95;
  isc_dyn_generator_id           =         96;

{ Procedure specific attributes }

  isc_dyn_prc_inputs             =        167;
  isc_dyn_prc_outputs            =        168;
  isc_dyn_prc_source             =        169;
  isc_dyn_prc_blr                =        170;
  isc_dyn_prc_source2            =        171;

{ Parameter specific attributes }

  isc_dyn_prm_number             =        138;
  isc_dyn_prm_type               =        139;

{ Relation specific attributes }

  isc_dyn_xcp_msg                =        185;

{ Cascading referential integrity values }

  isc_dyn_foreign_key_update     =        205;
  isc_dyn_foreign_key_delete     =        206;
  isc_dyn_foreign_key_cascade    =        207;
  isc_dyn_foreign_key_default    =        208;
  isc_dyn_foreign_key_null       =        209;
  isc_dyn_foreign_key_none       =        210;

{ SQL role values }

  isc_dyn_def_sql_role           =        211;
  isc_dyn_sql_role_name          =        212;
  isc_dyn_grant_admin_options    =        213;
  isc_dyn_del_sql_role           =        214;

{ ADMIN OPTION values }

  isc_dyn_add_admin              =        221;
  isc_dyn_drop_admin             =        222;
  isc_dyn_admin_active           =        223;
  isc_dyn_admin_inactive         =        224;

{ Last $dyn value assigned }

  isc_dyn_last_dyn_value         =        224;

{ Array slice description language (SDL) }

  isc_sdl_version1               =          1;
  isc_sdl_eoc                    =         -1;
  isc_sdl_relation               =          2;
  isc_sdl_rid                    =          3;
  isc_sdl_field                  =          4;
  isc_sdl_fid                    =          5;
  isc_sdl_struct                 =          6;
  isc_sdl_variable               =          7;
  isc_sdl_scalar                 =          8;
  isc_sdl_tiny_integer           =          9;
  isc_sdl_short_integer          =         10;
  isc_sdl_long_integer           =         11;
  isc_sdl_literal                =         12;
  isc_sdl_add                    =         13;
  isc_sdl_subtract               =         14;
  isc_sdl_multiply               =         15;
  isc_sdl_divide                 =         16;
  isc_sdl_negate                 =         17;
  isc_sdl_eql                    =         18;
  isc_sdl_neq                    =         19;
  isc_sdl_gtr                    =         20;
  isc_sdl_geq                    =         21;
  isc_sdl_lss                    =         22;
  isc_sdl_leq                    =         23;
  isc_sdl_and                    =         24;
  isc_sdl_or                     =         25;
  isc_sdl_not                    =         26;
  isc_sdl_while                  =         27;
  isc_sdl_assignment             =         28;
  isc_sdl_label                  =         29;
  isc_sdl_leave                  =         30;
  isc_sdl_begin                  =         31;
  isc_sdl_end                    =         32;
  isc_sdl_do3                    =         33;
  isc_sdl_do2                    =         34;
  isc_sdl_do1                    =         35;
  isc_sdl_element                =         36;

{ International text interpretation values }

  isc_interp_eng_ascii           =          0;
  isc_interp_jpn_sjis            =          5;
  isc_interp_jpn_euc             =          6;

{ Scroll direction for isc_dsql_fetch2 }

  isc_fetch_next                 =          0;
  isc_fetch_prior                =          1;
  isc_fetch_first                =          2;
  isc_fetch_last                 =          3;
  isc_fetch_absolute             =          4;
  isc_fetch_relative             =          5;

{ SQL definitions }

  SQL_VARYING                    =        448;
  SQL_TEXT                       =        452;
  SQL_DOUBLE                     =        480;
  SQL_FLOAT                      =        482;
  SQL_LONG                       =        496;
  SQL_SHORT                      =        500;
  SQL_TIMESTAMP                  =        510;
  SQL_BLOB                       =        520;
  SQL_D_FLOAT                    =        530;
  SQL_ARRAY                      =        540;
  SQL_QUAD                       =        550;
  SQL_TYPE_TIME                  =        560;
  SQL_TYPE_DATE                  =        570;
  SQL_INT64                      =        580;
  SQL_DATE                       =        SQL_TIMESTAMP;
  SQL_BOOLEAN                    =        590;

{CHARSET ID}
  CH_NONE                        =        0;
  CH_OCTETS                      =        1;
  CH_ASCII                       =        2;
  CH_UNICODE_FSS                 =        3; 
  CH_UTF8                        =        4; 

{ Blob Subtypes }

{ types less than zero are reserved for customer use }

  isc_blob_untyped               =          0;

{ internal subtypes }

  isc_blob_text                  =          1;
  isc_blob_blr                   =          2;
  isc_blob_acl                   =          3;
  isc_blob_ranges                =          4;
  isc_blob_summary               =          5;
  isc_blob_format                =          6;
  isc_blob_tra                   =          7;
  isc_blob_extfile               =          8;

{ the range 20-30 is reserved for dBASE and Paradox types }

  isc_blob_formatted_memo        =         20;
  isc_blob_paradox_ole           =         21;
  isc_blob_graphic               =         22;
  isc_blob_dbase_ole             =         23;
  isc_blob_typed_binary          =         24;

{ XML }

  XMLDA_ATTRIBUTE_FLAG       = $01;
  XMLDA_DISPLAY_NULL_FLAG    = $02;
  XMLDA_NO_HEADER_FLAG       = $04;

  MAXCHARSET_LENGTH          = 32; 
  SHORT_LEN                  =  7; 
  LONG_LEN                   = 12; 
  INT64_LEN                  = 21; 
  QUAD_LEN                   = 19;
  FLOAT_LEN                  = 14; 
  DOUBLE_LEN                 = 23; 
  DATE_LEN                   = 11; 
  DATETIME_LEN               = 25; 
  TIME_ONLY_LEN              = 13; 
  DATE_ONLY_LEN              = 11;
  UNKNOWN_LEN                = 20; 

  ERR_NOT_ENOUGH_MEMORY      = -1;
  ERR_BUFFERSIZE_NOT_ENOUGH  = -2;

type
{$IFDEF CLR}
  TSPBBuffer = IntPtr;
{$ELSE}
  TSPBBuffer = PAnsiChar;
{$ENDIF}

  TStatusVector = IntPtr;


const
{$IFDEF MSWINDOWS}
  GDSDLLName = 'gds32.dll';
  FBClientDLLName = 'fbclient.dll';
  IBXMLDLLName = 'ibxml.dll';
{$ENDIF}
{$IFDEF LINUX}
  GDSDLLName = 'libgds.so.0';
  IBXMLDLLName = 'ibxml.so.0';
{$ENDIF}

type
  TGDS = class
  protected
  {$IFDEF MSWINDOWS}
    hIBCLib: HMODULE;
    hXMLLib: HMODULE;
  {$ENDIF}
  {$IFDEF LINUX}
    hIBCCrypt: IntPtr;
    hIBCLib: IntPtr;
    hXMLLib: IntPtr;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    hBusy: THandle;
  {$ENDIF}
    FAlerterFatalError: boolean;

    procedure LoadGDS;
    procedure FreeGDS;
  {$IFNDEF CLR}
    function GetProc(const Name: string): IntPtr;
    function GetXMLProc(const Name: string): IntPtr;
  {$ENDIF}
    procedure InitFunctions;

  public
    GDSDLL: string;
    IBXMLDLL: string;
    EnsureThreadSafety: boolean;
    VersionSt: string;
    Version: Currency;

    isc_attach_database : Tisc_attach_database;
    isc_array_gen_sdl : Tisc_array_gen_sdl;
    isc_array_get_slice : Tisc_array_get_slice;
    isc_array_lookup_bounds : Tisc_array_lookup_bounds;
    isc_array_lookup_desc : Tisc_array_lookup_desc;
    isc_array_set_desc : Tisc_array_set_desc;
    isc_array_put_slice : Tisc_array_put_slice;
    isc_blob_default_desc : Tisc_blob_default_desc;
    isc_blob_default_desc2 : Tisc_blob_default_desc2;
    isc_blob_gen_bpb : Tisc_blob_gen_bpb;
    isc_blob_gen_bpb2 : Tisc_blob_gen_bpb2;
    isc_blob_info : Tisc_blob_info;
    isc_blob_lookup_desc : Tisc_blob_lookup_desc;
    isc_blob_lookup_desc2 : Tisc_blob_lookup_desc2;
    isc_blob_set_desc : Tisc_blob_set_desc;
    isc_blob_set_desc2 : Tisc_blob_set_desc2;
    isc_cancel_blob : Tisc_cancel_blob;
    isc_cancel_events : Tisc_cancel_events;
    isc_close_blob : Tisc_close_blob;
    isc_commit_retaining : Tisc_commit_retaining;
    isc_commit_transaction : Tisc_commit_transaction;
    isc_create_blob2 : Tisc_create_blob2;
    isc_database_info : Tisc_database_info;
    isc_decode_date : Tisc_decode_date;
    isc_decode_sql_date : Tisc_decode_sql_date;
    isc_decode_sql_time : Tisc_decode_sql_time;
    isc_detach_database : Tisc_detach_database;
    isc_drop_database : Tisc_drop_database;
    isc_dsql_alloc_statement2 : Tisc_dsql_alloc_statement2;
    isc_dsql_describe : Tisc_dsql_describe;
    isc_dsql_describe_bind : Tisc_dsql_describe_bind;
    isc_dsql_execute : Tisc_dsql_execute;
    isc_dsql_execute2 : Tisc_dsql_execute2;
    isc_dsql_execute_immediate : Tisc_dsql_execute_immediate;
    isc_dsql_fetch : Tisc_dsql_fetch;
    isc_dsql_free_statement : Tisc_dsql_free_statement;
    isc_dsql_prepare : Tisc_dsql_prepare;
    isc_dsql_set_cursor_name : Tisc_dsql_set_cursor_name;
    isc_dsql_sql_info : Tisc_dsql_sql_info;
    isc_encode_date : Tisc_encode_date;
    isc_encode_sql_date : Tisc_encode_sql_date;
    isc_encode_sql_time : Tisc_encode_sql_time;
    isc_seek_blob : Tisc_seek_blob;
    isc_interprete : Tisc_interprete;
    isc_open_blob2 : Tisc_open_blob2;
    isc_release_savepoint : Tisc_release_savepoint;
    isc_rollback_retaining : Tisc_rollback_retaining;
    isc_rollback_savepoint : Tisc_rollback_savepoint;
    isc_rollback_transaction : Tisc_rollback_transaction;
    isc_prepare_transaction : Tisc_prepare_transaction;
    isc_prepare_transaction2: Tisc_prepare_transaction2;
    isc_transaction_info: Tisc_transaction_info;
    isc_put_segment: Tisc_put_segment;
    isc_get_segment: Tisc_get_segment;
    isc_que_events: Tisc_que_events;
    isc_event_counts: Tisc_event_counts;
    isc_event_block : Tisc_event_block;
    isc_free: Tisc_free;
    isc_start_multiple : Tisc_start_multiple;
    isc_start_savepoint : Tisc_start_savepoint;
    isc_sqlcode : Tisc_sqlcode;
    isc_sql_interprete : Tisc_sql_interprete;
    isc_get_client_version : Tisc_get_client_version;
    isc_get_client_major_version : Tisc_get_client_major_version;
    isc_get_client_minor_version : Tisc_get_client_minor_version;
    isc_dsql_execute2_m: Tisc_dsql_execute2_m;
    isc_dsql_exec_immed3_m: Tisc_dsql_exec_immed3_m;
    isc_dsql_fetch_m: Tisc_dsql_fetch_m;
    isc_dsql_insert_m: Tisc_dsql_insert_m;
    isc_dsql_prepare_m: Tisc_dsql_prepare_m;
    isc_dsql_batch_execute_immed: Tisc_dsql_batch_execute_immed;

    isc_dsql_xml_fetch : Tisc_dsql_xml_fetch;
    isc_dsql_xml_fetch_all: Tisc_dsql_xml_fetch_all;
    isc_dsql_xml_buffer_fetch: Tisc_dsql_xml_buffer_fetch;

    isc_service_attach: Tisc_service_attach;
    isc_service_detach: Tisc_service_detach;
    isc_service_query: Tisc_service_query;
    isc_service_start: Tisc_service_start;

    fb_cancel_operation : Tfb_cancel_operation;

    BLOB_open : TBLOB_open;
    BLOB_put : TBLOB_put;
    BLOB_get : TBLOB_get;
    BLOB_close : TBLOB_close;
    BLOB_display : TBLOB_display;
    BLOB_dump : TBLOB_dump;
    BLOB_edit : TBLOB_edit;
    BLOB_load : TBLOB_load;
    BLOB_text_dump : TBLOB_text_dump;
    BLOB_text_load : TBLOB_text_load;
    Bopen : TBopen;
    Bopen2 : TBopen2;

    constructor Create(const LibFile: string);
    destructor Destroy; override;

    procedure Busy;
    procedure Release;

    procedure GetIBError(ErrorCode: integer; UseUnicode: boolean; var ErrorNumber: integer;
      var StatusVector: TStatusVector; var ErrorMsg: _string; var SQLErrorMsg: _string);

    function getb(p: PBSTREAM): AnsiChar;
    function putb(x: AnsiChar; p: PBSTREAM): Int;
    function putbx(x: AnsiChar; p: PBSTREAM): Int;

    property AlerterFatalError: boolean read FAlerterFatalError write FAlerterFatalError;
  end;

  TGDSList = class(TThreadList)
  public
    destructor Destroy; override;
    procedure Clear;
    function FindGDS(const LibFile: string): TGDS;
    function GetGDS(const LibFile: string): TGDS;
  end;

var
  GDSList: TGDSList;
  ThreadSafetyClientLibrary: boolean = False;


{Unicode support routines}

function Utf8ToWs(Source: IntPtr; SourceBytes: integer;
  Dest: IntPtr; MaxDestBytes{w/wo #0}: integer;
  AddNull: boolean = True): integer{bytes w/wo #0};
function Utf8ToAnsi(Source: IntPtr; SrcLen: integer; Dest: IntPtr; DestLen: integer): integer;
function WsToUtf8(Source: IntPtr; Dest: IntPtr; DestLen: integer): integer;
function AnsiToUtf8(Source: IntPtr; Dest: IntPtr; DestLen: integer): integer;

function StringToPtrGDS(const S: _string; UseUnicode: boolean): PStr;
procedure FreeStringGDS(P: PStr; UseUnicode: boolean);
function PtrToStringGDS(P: IntPtr; UseUnicode: boolean): _string; overload;
function PtrToStringGDS(P: IntPtr; Size: integer; UseUnicode: boolean): _string; overload;

const
  DACProductName = 'IBDAC';

implementation
uses
{$IFDEF CLR}
  {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF},
{$ENDIF}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  {$IFNDEF UNIDACPRO}IBCError{$ELSE}IBCErrorUni{$ENDIF};




{ TGDSList }

destructor TGDSList.Destroy;
begin
  Clear;

  inherited;
end;

procedure TGDSList.Clear;
var
  List: TList;
  i: integer;
begin
  List := LockList;
  try
    for i := 0 to List.Count - 1 do
      TGDS(List[i]).Free;

    List.Clear;
  finally
    UnlockList;
  end;
end;

function TGDSList.FindGDS(const LibFile: string): TGDS;
var
  List: TList;
  i: integer;
  GDS: TGDS;
  NormFile: string;
begin
  List := LockList;
  try
    NormFile := Trim(LibFile);
    for i := 0 to List.Count - 1 do begin
      GDS := TGDS(List[i]);
    {$IFNDEF CLR}
      if (NormFile = '') or AnsiSameText(GDS.GDSDLL, NormFile) then
    {$ENDIF}
      begin
        Result := GDS;
        exit;
      end;
    end;
  finally
    UnlockList;
  end;
  Result := nil;
end;

function TGDSList.GetGDS(const LibFile: string): TGDS;
var
  List: TList;
begin
  List := LockList;
  try
    Result := FindGDS(LibFile);
    if Result = nil then begin
      Result := TGDS.Create(LibFile);
      List.Add(Result);
    end;
  finally
    UnlockList;
  end;
end;

{ TGDS }

(*{$IFDEF MSWINDOWS}
function GetFileVersion(FileName: string): string;
var
  VersionData: TBytes;
  VersionSt: IntPtr;
  Len: DWORD;
  Handle: DWORD;
  CharSetC: cardinal;
  CharSet: string;
begin
  Result := '';
  //GDSFileVersion := 0;

  Len := GetFileVersionInfoSize(PChar(FileName), Handle);
  SetLength(VersionData, Len);
  if GetFileVersionInfo(PChar(FileName), Handle, Len, VersionData) then begin
    if VerQueryValueA(VersionData, PAnsiChar('\VarFileInfo\Translation'), VersionSt, Len) then begin
    {$IFDEF CLR}
      CharSetC := Marshal.ReadInt32(VersionSt);
    {$ELSE}
      Move(VersionSt^, CharSetC, 4);
    {$ENDIF}
      CharSet := IntToHex(CharSetC, 8);
      CharSet := Copy(CharSet, 5, 4) + Copy(CharSet, 1, 4);
    end
    else
      CharSet := '040904B0';

    if VerQueryValueA(VersionData, PAnsiChar(AnsiString('\StringFileInfo\' + CharSet + '\FileVersion')), VersionSt, Len) then
      Result := string(Marshal.PtrToStringAnsi(VersionSt));
  end;
end;
{$ENDIF}*)

constructor TGDS.Create(const LibFile: string);
begin
  inherited Create;

{$IFDEF MSWINDOWS}
  hBusy := CreateMutex(nil, False, '');
{$ENDIF}

  GDSDLL := Trim(LibFile);
  LoadGDS;
end;

destructor TGDS.Destroy;
begin
  FreeGDS;

{$IFDEF MSWINDOWS}
  CloseHandle(hBusy);
{$ENDIF}

  inherited;
end;

procedure TGDS.Busy;
begin
{$IFDEF MSWINDOWS}
  if EnsureThreadSafety or ThreadSafetyClientLibrary then
    WaitForSingleObject(hBusy, INFINITE);
{$ENDIF}
end;

procedure TGDS.Release;
begin
{$IFDEF MSWINDOWS}
  if EnsureThreadSafety or ThreadSafetyClientLibrary then
    ReleaseMutex(hBusy);
{$ENDIF}
end;

procedure TGDS.LoadGDS;
var
  NeedLoadGDS: boolean;
begin
  NeedLoadGDS := True;
{$IFDEF CLR}
  GDSDLL := GDSDLLName;
  IBXMLDLL := IBXMLDLLName;
{$ELSE}
{$IFDEF MSWINDOWS}
  if GDSDLL = '' then begin
    GDSDLL := GDSDLLName;
    hIBCLib := LoadLibraryEx(GDSDLLName, 0, LOAD_WITH_ALTERED_SEARCH_PATH);
    if hIBCLib = 0 then begin
      hIBCLib := LoadLibraryEx(FBClientDLLName, 0, LOAD_WITH_ALTERED_SEARCH_PATH);
      if hIBCLib <> 0 then
        GDSDLL := FBClientDLLName;
    end;
    NeedLoadGDS := False;
  end;
{$ENDIF}

{$IFDEF LINUX}
  if GDSDLL = '' then
    GDSDLL := GDSDLLName;
{$ENDIF}

  if IBXMLDLL = '' then begin
    IBXMLDLL := IBXMLDLLName;
    IBXMLDLL := ExtractFilePath(GDSDLL) + IBXMLDLL;
  end;
{$ENDIF}

  //GDSFileVersionSt := '1'; //GetFileVersion(GDSDLL);
  //GDSFileVersion := 0; //VersionStrToWord(GDSFileVersionSt);

  //if GDSFileVersionSt = '' then
  //  RaiseError;

{$IFDEF LINUX}
  hIBCCrypt := dlopen('libcrypt.so', {RTLD_GLOBAL}RTLD_LAZY);
  if hIBCCrypt = nil then
    raise Exception.Create('Cannot load DLL: libcrypt.so');
{$ENDIF}

  if NeedLoadGDS then
{$IFDEF MSWINDOWS}
    hIBCLib := LoadLibraryEx(PChar(GDSDLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
{$ENDIF}
{$IFDEF LINUX}
    hIBCLib := dlopen(PChar(GDSDLL), {RTLD_GLOBAL}RTLD_LAZY);
{$ENDIF}

{$IFDEF MSWINDOWS}
  if hIBCLib = 0 then
{$ENDIF}
{$IFDEF LINUX}
  if hIBCLib = nil then
{$ENDIF}
    raise Exception.Create('Cannot load client DLL: ' + GDSDLL);

{$IFDEF MSWINDOWS}
  if Pos('GDS32', GDSDLL) = 0 then
    hXMLLib := 0
  else
    hXMLLib := LoadLibrary(PChar(IBXMLDLL));
{$ENDIF}
{$IFDEF LINUX}
  if Pos('libgds.so.0', GDSDLL) <= 0 then
    hXMLLib := nil
  else
    hXMLLib := dlopen(PChar(IBXMLDLL), {RTLD_GLOBAL}RTLD_LAZY);
{$ENDIF}

  InitFunctions;
  EnsureThreadSafety := (Version < 2.5) or (Version >= 6);
end;

procedure TGDS.FreeGDS;
begin
{$IFDEF MSWINDOWS}
  if hIBCLib <> 0 then begin
    FreeLibrary(hIBCLib);  // Returns False on Win95 in DLL
    hIBCLib := 0;
  end;
  if hXMLLib <> 0 then begin
    FreeLibrary(hXMLLib);  // Returns False on Win95 in DLL
    hXMLLib := 0;
  end;
{$ENDIF}
{$IFDEF LINUX}
 if hIBCLib <> nil then begin
    // dlclose(hIBCLib); // BUG: SIGSEGV on close application
    hIBCLib := nil;
 end;
 if hIBCCrypt <> nil then
   hIBCCrypt := nil;
 if hXMLLib <> nil then
   hXMLLib := nil;
{$ENDIF}

  VersionSt := '';
  Version := 0;
end;

{$IFNDEF CLR}
function NotLink: integer;
begin
  RaiseError('GDS function is not linked');
  Result := 0;
end;

function TGDS.GetProc(const Name: string): IntPtr;
begin
{$IFDEF MSWINDOWS}
  Result := GetProcAddress(hIBCLib, PChar(Name));
{$ENDIF}
{$IFDEF LINUX}
  Result := dlsym(hIBCLib, PChar(Name));
{$ENDIF}
  if Result = nil then
    Result := @NotLink;
end;

function TGDS.GetXMLProc(const Name: string): IntPtr;
begin
{$IFDEF MSWINDOWS}
  Result := GetProcAddress(hXMLLib, PChar(Name));
{$ENDIF}
{$IFDEF LINUX}
  Result := dlsym(hXMLLib, PChar(Name));
{$ENDIF}
  if Result = nil then
    Result := @NotLink;
end;
{$ENDIF}

procedure TGDS.InitFunctions;
{$IFNDEF CLR}
var
  s: AnsiString;
  p1, p2: Integer;
{$ENDIF}
begin
{$IFDEF CLR}
  BLOB_open := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.BLOB_open;
  BLOB_put := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.BLOB_put;
  BLOB_get := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.BLOB_get;
  BLOB_close := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.BLOB_close;
  BLOB_display := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.BLOB_display;
  BLOB_dump := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.BLOB_dump;
  BLOB_edit := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.BLOB_edit;
  BLOB_load := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.BLOB_load;
  BLOB_text_dump := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.BLOB_text_dump;
  BLOB_text_load := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.BLOB_text_load;
  Bopen := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.Bopen;
  Bopen2 := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.Bopen2;

  isc_sqlcode := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_sqlcode;
  isc_sql_interprete := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_sql_interprete;
  isc_interprete := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_interprete;
  isc_blob_info := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_blob_info;
  isc_open_blob2 := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_open_blob2;
  isc_close_blob := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_close_blob;
  isc_get_segment := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_get_segment;
  isc_seek_blob := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_seek_blob;
  isc_put_segment := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_put_segment;
  isc_create_blob2 := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_create_blob2;
  isc_cancel_blob := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_cancel_blob;
  isc_array_get_slice := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_array_get_slice;
  isc_array_lookup_bounds := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_array_lookup_bounds;
  isc_array_lookup_desc := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_array_lookup_desc;
  isc_array_set_desc := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_array_set_desc;
  isc_array_put_slice := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_array_put_slice;
  isc_blob_default_desc := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_blob_default_desc;
  isc_blob_gen_bpb := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_blob_gen_bpb;
  isc_blob_lookup_desc := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_blob_lookup_desc;
  isc_blob_set_desc := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_blob_set_desc;
  isc_decode_date := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_decode_date;
  isc_encode_date := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_encode_date;
  isc_dsql_free_statement := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_free_statement;
  isc_dsql_execute2 := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_execute2;
  isc_dsql_execute := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_execute;
  isc_dsql_set_cursor_name := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_set_cursor_name;
  isc_dsql_fetch := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_fetch;
  isc_dsql_sql_info := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_sql_info;
  isc_dsql_alloc_statement2 := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_alloc_statement2;
  isc_dsql_prepare := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_prepare;
  isc_dsql_describe_bind := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_describe_bind;
  isc_dsql_describe := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_describe;
  isc_dsql_execute_immediate := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_execute_immediate;
  isc_drop_database := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_drop_database;
  isc_detach_database := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_detach_database;
  isc_attach_database := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_attach_database;
  isc_database_info := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_database_info;
  isc_start_multiple := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_start_multiple;
  isc_commit_transaction := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_commit_transaction;
  isc_commit_retaining := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_commit_retaining;
  isc_rollback_transaction := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_rollback_transaction;
  isc_cancel_events := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_cancel_events;
  isc_que_events := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_que_events;
  isc_event_counts := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_event_counts;
  isc_event_block := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_event_block;
  isc_free := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_free;
  isc_prepare_transaction := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_prepare_transaction;
  isc_prepare_transaction2 := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_prepare_transaction2;
  isc_transaction_info := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_transaction_info;
  isc_dsql_execute2_m := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_execute2_m;
  isc_dsql_exec_immed3_m := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_exec_immed3_m;
  isc_dsql_fetch_m := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_fetch_m;
  isc_dsql_insert_m := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_insert_m;
  isc_dsql_prepare_m := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_prepare_m;
  isc_array_gen_sdl := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_array_gen_sdl;

  if GetProcAddress(hIBCLib, 'fb_cancel_operation') <> nil then begin
    fb_cancel_operation := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.fb_cancel_operation;
    CANCEL_OPTION := FB_CANCEL_RAISE;
  end
  else begin
    fb_cancel_operation := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_free_statement;
    CANCEL_OPTION := DSQL_cancel;
  end;

  if GetProcAddress(hIBCLib, 'isc_rollback_retaining') <> nil then begin
    Version := 6;
    VersionSt := '6';
    isc_rollback_retaining := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_rollback_retaining;
    isc_decode_sql_date := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_decode_sql_date;
    isc_decode_sql_time := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_decode_sql_time;
    isc_encode_sql_date := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_encode_sql_date;
    isc_encode_sql_time := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_encode_sql_time;
  end
  else begin
    Version := 5;
    VersionSt := '5';
  end;
  if GetProcAddress(hIBCLib, 'isc_get_client_version') <> nil then begin
    isc_get_client_version := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_get_client_version;
    isc_get_client_major_version := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_get_client_major_version;
    isc_get_client_minor_version := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_get_client_minor_version;
    Version := isc_get_client_major_version + (isc_get_client_minor_version / 10);
    VersionSt := IntToStr(isc_get_client_major_version)+'.'+IntToStr(isc_get_client_minor_version);
  end;
  if Version >= 7 then begin
    isc_array_gen_sdl := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_array_gen_sdl2;
    isc_array_get_slice := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_array_get_slice2;
    isc_array_lookup_bounds := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_array_lookup_bounds2;
    isc_array_lookup_desc := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_array_lookup_desc2;
    isc_array_set_desc := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_array_set_desc2;
    isc_array_put_slice := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_array_put_slice2;
    isc_blob_default_desc2 := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_blob_default_desc2;
    isc_blob_gen_bpb2 := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_blob_gen_bpb2;
    isc_blob_lookup_desc2 := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_blob_lookup_desc2;
    isc_blob_set_desc2 := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_blob_set_desc2;
    if Version >= 7.1 then begin
      isc_release_savepoint := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_release_savepoint;
      isc_rollback_savepoint := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_rollback_savepoint;
      isc_start_savepoint := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_start_savepoint;
    end
  end;
  if Version >= 9 then begin
    isc_dsql_batch_execute_immed := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_batch_execute_immed;
  end;

  isc_service_attach := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_service_attach;
  isc_service_detach := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_service_detach;
  isc_service_query := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_service_query;
  isc_service_start := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_service_start;

  if (hXMLLib <> 0) then begin
    isc_dsql_xml_fetch := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_xml_fetch;
    isc_dsql_xml_fetch_all := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_xml_fetch_all;
    isc_dsql_xml_buffer_fetch := {$IFNDEF UNIDACPRO}IBCCallCLR{$ELSE}IBCCallCLRUni{$ENDIF}.isc_dsql_xml_buffer_fetch;
  end;

{$ELSE}
  BLOB_open := GetProc('BLOB_open');
  BLOB_put := GetProc('BLOB_put');
  BLOB_get := GetProc('BLOB_get');
  BLOB_close := GetProc('BLOB_close');
  BLOB_display := GetProc('BLOB_display');
  BLOB_dump := GetProc('BLOB_dump');
  BLOB_edit := GetProc('BLOB_edit');
  BLOB_load := GetProc('BLOB_load');
  BLOB_text_dump := GetProc('BLOB_text_dump');
  BLOB_text_load := GetProc('BLOB_text_load');
  Bopen := GetProc('Bopen');
  Bopen2 := GetProc('Bopen2');
  isc_sqlcode := GetProc('isc_sqlcode');
  isc_sql_interprete := GetProc('isc_sql_interprete');
  isc_interprete := GetProc('isc_interprete');
  isc_blob_info := GetProc('isc_blob_info');
  isc_open_blob2 := GetProc('isc_open_blob2');
  isc_close_blob := GetProc('isc_close_blob');
  isc_seek_blob := GetProc('isc_seek_blob');
  isc_create_blob2 := GetProc('isc_create_blob2');
  isc_cancel_blob := GetProc('isc_cancel_blob');
  isc_get_segment := GetProc('isc_get_segment');
  isc_put_segment := GetProc('isc_put_segment');

  isc_array_gen_sdl := GetProc('isc_array_gen_sdl');
  isc_array_get_slice := GetProc('isc_array_get_slice');
  isc_array_lookup_bounds := GetProc('isc_array_lookup_bounds');
  isc_array_lookup_desc := GetProc('isc_array_lookup_desc');
  isc_array_set_desc := GetProc('isc_array_set_desc');
  isc_array_put_slice := GetProc('isc_array_put_slice');
  isc_blob_default_desc := GetProc('isc_blob_default_desc');
  isc_blob_gen_bpb := GetProc('isc_blob_gen_bpb');
  isc_blob_lookup_desc := GetProc('isc_blob_lookup_desc');
  isc_blob_set_desc := GetProc('isc_blob_set_desc');
  isc_decode_date := GetProc('isc_decode_date');
  isc_encode_date := GetProc('isc_encode_date');
  isc_dsql_free_statement := GetProc('isc_dsql_free_statement');
  isc_dsql_execute2 := GetProc('isc_dsql_execute2');
  isc_dsql_execute := GetProc('isc_dsql_execute');
  isc_dsql_set_cursor_name := GetProc('isc_dsql_set_cursor_name');
  isc_dsql_fetch := GetProc('isc_dsql_fetch');
  isc_dsql_sql_info := GetProc('isc_dsql_sql_info');
  isc_dsql_alloc_statement2 := GetProc('isc_dsql_alloc_statement2');
  isc_dsql_prepare := GetProc('isc_dsql_prepare');
  isc_dsql_describe_bind := GetProc('isc_dsql_describe_bind');
  isc_dsql_describe := GetProc('isc_dsql_describe');
  isc_dsql_execute_immediate := GetProc('isc_dsql_execute_immediate');
  isc_drop_database := GetProc('isc_drop_database');
  isc_detach_database := GetProc('isc_detach_database');
  isc_attach_database := GetProc('isc_attach_database');
  isc_database_info := GetProc('isc_database_info');
  isc_start_multiple := GetProc('isc_start_multiple');
  isc_commit_transaction := GetProc('isc_commit_transaction');
  isc_commit_retaining := GetProc('isc_commit_retaining');
  isc_rollback_transaction := GetProc('isc_rollback_transaction');
  isc_prepare_transaction := GetProc('isc_prepare_transaction');
  isc_prepare_transaction2 := GetProc('isc_prepare_transaction2');
  isc_transaction_info := GetProc('isc_transaction_info');
  isc_que_events := GetProc('isc_que_events');
  isc_event_counts := GetProc('isc_event_counts');
  isc_cancel_events := GetProc('isc_cancel_events');
  isc_event_block := GetProc('isc_event_block');
  isc_free := GetProc('isc_free');
  isc_dsql_execute2_m := GetProc('isc_dsql_execute2_m');
  isc_dsql_exec_immed3_m := GetProc('isc_dsql_exec_immed3_m');
  isc_dsql_fetch_m := GetProc('isc_dsql_fetch_m');
  isc_dsql_insert_m := GetProc('isc_dsql_insert_m');
  isc_dsql_prepare_m := GetProc('isc_dsql_prepare_m');

  fb_cancel_operation := GetProc('fb_cancel_operation');
  if @fb_cancel_operation = @NotLink then begin
    fb_cancel_operation := GetProc('isc_dsql_free_statement');
    CANCEL_OPTION := DSQL_cancel;
  end
  else
    CANCEL_OPTION := FB_CANCEL_RAISE;

  isc_rollback_retaining := GetProc('isc_rollback_retaining');
  if @isc_rollback_retaining <> @NotLink then begin
    Version := 6;
    VersionSt := '6';
    isc_decode_sql_date := GetProc('isc_decode_sql_date');
    isc_decode_sql_time := GetProc('isc_decode_sql_time');
    isc_encode_sql_date := GetProc('isc_encode_sql_date');
    isc_encode_sql_time := GetProc('isc_encode_sql_time');
  end
  else begin
    Version := 5;
    VersionSt := '5';
    isc_decode_sql_date := @NotLink;
    isc_decode_sql_time := @NotLink;
    isc_encode_sql_date := @NotLink;
    isc_encode_sql_time := @NotLink;
  end;

  isc_get_client_version := GetProc('isc_get_client_version');
  if @isc_get_client_version <> @NotLink then begin
    isc_get_client_major_version := GetProc('isc_get_client_major_version');
    isc_get_client_minor_version := GetProc('isc_get_client_minor_version');

    SetLength(s, 64);
    FillChar(PAnsiChar(s), Length(s), 0);
    isc_get_client_version(PAnsiChar(s));
    s := AnsiString(LowerCase(string(s)));
    p1 := Pos('firebird', string(s));
    if p1 > 0 then begin
      p1 := p1 + Length('firebird');
      if s[p1] = ' ' then
        Inc(p1);
      p2 := p1;
      while (p2 <= Length(s)) and (s[p2] <> ' ') and (s[p2] <> #0) do
        Inc(p2);

      VersionSt := Copy(string(s), p1, p2 - p1);
      Version := StrToFloat(StringReplace(VersionSt, '.', DecimalSeparator, []));
    end
    else begin
      VersionSt := IntToStr(isc_get_client_major_version) + '.' + IntToStr(isc_get_client_minor_version);
      Version := isc_get_client_major_version + (isc_get_client_minor_version / 10);
    end;
  end
  else begin
    isc_get_client_major_version := @NotLink;
    isc_get_client_minor_version := @NotLink;
  end;

  if Version >= 7 then begin
    isc_array_gen_sdl := GetProc('isc_array_gen_sdl2');
    isc_array_get_slice := GetProc('isc_array_get_slice2');
    isc_array_lookup_bounds := GetProc('isc_array_lookup_bounds2');
    isc_array_lookup_desc := GetProc('isc_array_lookup_desc2');
    isc_array_set_desc := GetProc('isc_array_set_desc2');
    isc_array_put_slice := GetProc('isc_array_put_slice2');
    isc_blob_default_desc2 := GetProc('isc_blob_default_desc2');
    isc_blob_gen_bpb2 := GetProc('isc_blob_gen_bpb2');
    isc_blob_lookup_desc2 := GetProc('isc_blob_lookup_desc2');
    isc_blob_set_desc2 := GetProc('isc_blob_set_desc2');
    if Version >= 7.1 then begin
      isc_release_savepoint := GetProc('isc_release_savepoint');
      isc_rollback_savepoint := GetProc('isc_rollback_savepoint');
      isc_start_savepoint := GetProc('isc_start_savepoint');
    end
    else begin
      isc_release_savepoint := @NotLink;
      isc_rollback_savepoint := @NotLink;
      isc_start_savepoint := @NotLink;
    end;
  end
  else begin
    isc_blob_default_desc2 := @NotLink;
    isc_blob_gen_bpb2 := @NotLink;
    isc_blob_lookup_desc2 := @NotLink;
    isc_blob_set_desc2 := @NotLink;
  end;
  if Version >= 9 then begin
    isc_dsql_batch_execute_immed := GetProc('isc_dsql_batch_execute_immed');
  end
  else begin
    isc_dsql_batch_execute_immed := @NotLink;
  end;

  isc_service_attach := GetProc('isc_service_attach');
  isc_service_detach := GetProc('isc_service_detach');
  isc_service_query := GetProc('isc_service_query');
  isc_service_start := GetProc('isc_service_start');

{$IFDEF MSWINDOWS}
  if (hXMLLib <> 0) then begin
{$ENDIF}
{$IFDEF LINUX}
  if (hXMLLib <> nil) then begin
{$ENDIF}
    isc_dsql_xml_fetch := GetXMLProc('isc_dsql_xml_fetch');
    isc_dsql_xml_fetch_all := GetXMLProc('isc_dsql_xml_fetch_all');
    isc_dsql_xml_buffer_fetch := GetXMLProc('isc_dsql_xml_buffer_fetch');
  end
  else begin
    isc_dsql_xml_fetch := @NotLink;
    isc_dsql_xml_fetch_all := @NotLink;
    isc_dsql_xml_buffer_fetch := @NotLink;
  end;
{$ENDIF}
end;

procedure TGDS.GetIBError(ErrorCode: integer; UseUnicode: boolean; var ErrorNumber: integer;
  var StatusVector: TStatusVector; var ErrorMsg: _string; var SQLErrorMsg: _string);
const
  MsgLen = 1024 * 4;
var
  MsgBuf: IntPtr;
  PStatusVector: IntPtr;
begin
  MsgBuf := Marshal.AllocHGlobal(MsgLen);
  PStatusVector := Marshal.AllocHGlobal(SizeOf(IntPtr));
  try
    ErrorNumber := Marshal.ReadInt32(StatusVector, 1 * SizeOf(NativeInt));
    Marshal.WriteIntPtr(PStatusVector, StatusVector);
    Marshal.WriteByte(MsgBuf, 0);

    Busy;
    try
      isc_sql_interprete(ErrorCode, MsgBuf, MsgLen);
      SQLErrorMsg := PtrToStringGDS(MsgBuf, False{UseUnicode});
      ErrorMsg := '';
      Marshal.WriteByte(MsgBuf, 0);
      while isc_interprete(MsgBuf, PStatusVector) > 0 do
        ErrorMsg := ErrorMsg + LineSeparator + PtrToStringGDS(MsgBuf, False{UseUnicode});
    finally
      Release;
    end;
  finally
    Marshal.FreeHGlobal(MsgBuf);
    Marshal.FreeHGlobal(PStatusVector);
  end;
end;

function TGDS.getb(p: PBSTREAM): AnsiChar;
begin
{$IFDEF CLR}
  Result := ' ';
{$ELSE}
  with TBSTREAM(p^) do begin
    Dec(bstr_cnt);
    if (bstr_cnt >= 0) then begin
      Result := AnsiChar(Int(bstr_ptr^) and Int(0377));
      Inc(PAnsiChar(bstr_ptr)^);
    end else
      Result := AnsiChar(BLOB_get(p));
 end;
{$ENDIF}
end;

function TGDS.putb(x: AnsiChar; p: PBSTREAM): Int;
begin
{$IFDEF CLR}
  Result := -1;
{$ELSE}
  with TBSTREAM(p^) do begin
    Dec(bstr_cnt);
    if (x = #13) or (bstr_cnt = 0) then
      Result := BLOB_put(x, p)
    else begin
      PAnsiChar(bstr_ptr)^ := x;
      Result := UInt(x);
      Inc(PAnsiChar(bstr_ptr)^);
    end;
 end;
{$ENDIF}
end;

function TGDS.putbx(x: AnsiChar; p: PBSTREAM): Int;
begin
{$IFDEF CLR}
  Result := -1;
{$ELSE}
  with TBSTREAM(p^) do begin
    Dec(bstr_cnt);
    if (bstr_cnt = 0) then
      Result := BLOB_put(x, p)
    else begin
      PAnsiChar(bstr_ptr)^ := AnsiChar(x);
      Inc(PAnsiChar(bstr_ptr)^);
      Result := UInt(x);
    end;
  end;
{$ENDIF}
end;

{Unicode support routines}

{$IFDEF CLR}
[DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'MultiByteToWideChar')]
function MultiByteToWideChar(CodePage: UINT; dwFlags: DWORD;
  lpMultiByteStr: IntPtr; cchMultiByte: Integer;
  lpWideCharStr: IntPtr; cchWideChar: Integer): Integer; external;
{$ENDIF}

// Convert Utf8 buffer to WideString buffer with or without null terminator.
{$IFNDEF LINUX}
// should work starting with Windows 98
function Utf8ToWs(Source: IntPtr; SourceBytes: integer;
  Dest: IntPtr; MaxDestBytes{w/wo #0}: integer;
  AddNull: boolean = True): integer{bytes w/wo #0};
var
  MaxDestChars: integer;
  Buf, TempBuf: IntPtr;
begin
  Assert(Source <> nil, 'Utf8ToWs: Source is nil');
  Assert(Dest <> nil, 'Utf8ToWs: Destination is nil');

  MaxDestChars := MaxDestBytes div 2;

  Result := MultibyteToWideChar(CP_UTF8, 0, Source, SourceBytes, nil, 0);

  if Result > MaxDestChars then begin
    TempBuf := Marshal.AllocHGlobal(Result * 2);
    Buf := TempBuf;
  end
  else begin
    TempBuf := nil;
    Buf := Dest;
  end;

  Result := MultibyteToWideChar(CP_UTF8, 0, Source, SourceBytes,
    Buf, Result);

  if Buf = TempBuf then begin
    Result := MaxDestBytes;
    CopyBuffer(TempBuf, Dest, Result);
    Marshal.FreeHGlobal(TempBuf);
  end
  else
    Result := Result * 2;

  if AddNull and (MaxDestBytes > 0) then begin
    if Result >= MaxDestBytes then
      Result := MaxDestBytes - SizeOf(WideChar);

    Marshal.WriteInt16(Dest, Result, 0);

    Inc(Result, SizeOf(WideChar));
  end;
end;
{$ELSE}
// Old code (does not support Unicode surrogate pairs)
// Nearly copied from System.Utf8ToUnicode
function Utf8ToWs(Source: IntPtr; SourceBytes: integer;
  Dest: IntPtr; MaxDestBytes{w/wo #0}: integer;
  AddNull: boolean = True): integer{bytes w/wo #0};
var
  i: integer;
  c: Byte;
  wc: Cardinal;
begin
  Assert(Source <> nil, 'Utf8ToWs: Source is nil');
  Assert(Dest <> nil, 'Utf8ToWs: Destination is nil');
  Result := 0;
  i := 0;
  while i < SourceBytes do
  begin
    wc := Cardinal(PAnsiChar(Source)[i]);
    if wc = 0 then   //zero terminator
      break;
    Inc(i);
    if (wc and $80) <> 0 then
    begin
      Assert(i < SourceBytes, 'Utf8ToWs: Incomplete multibyte char');
      wc := wc and $3F;
      if (wc and $20) <> 0 then
      begin
        c := Byte(PAnsiChar(Source)[i]);
        Inc(i);
        Assert((c and $C0) = $80, 'Utf8ToWs: Malformed trail byte or out of range char');
        Assert(i < SourceBytes, 'Utf8ToWs: Incomplete multibyte char');
        wc := (wc shl 6) or (c and $3F);
      end;
      c := Byte(PAnsiChar(Source)[i]);
      Inc(i);
      Assert((c and $C0) = $80, 'Utf8ToWs: Malformed trail byte');
      wc := (wc shl 6) or (c and $3F);
    end;

    if not (Result + 1 < MaxDestBytes) then
      Break;
    PWord(PtrOffset(Dest, Result))^ := Word(wc);
    Inc(Result, SizeOf(WideChar));
  end;

  if AddNull and (MaxDestBytes > 0) then begin
    if Result >= MaxDestBytes then
      Result := MaxDestBytes - SizeOf(WideChar);
    PWord(PtrOffset(Dest, Result))^ := 0;
    Inc(Result, SizeOf(WideChar));
  end;
end;
{$ENDIF}

function Utf8ToAnsi(Source: IntPtr; SrcLen: integer; Dest: IntPtr; DestLen: integer): integer;
var
  DestBuf: IntPtr;
  AnsiStr: AnsiString;
  WideStr: WideString;
begin
  DestBuf := Marshal.AllocHGlobal(DestLen * 2);
  try
    Result := Utf8ToWs(Source, SrcLen, DestBuf, (DestLen - 1) * 2, False);

    WideStr := Marshal.PtrToStringUni(DestBuf, Result div 2);
    AnsiStr := AnsiString(WideStr);
    Result := Length(AnsiStr);
    if Result > DestLen - 1 then
      Result := DestLen - 1;

    Marshal.Copy(TBytes(AnsiStr), 0, Dest, Result);
    Marshal.WriteByte(Dest, Result, 0);
    Inc(Result);
  finally
    Marshal.FreeHGlobal(DestBuf);
  end;
end;

function WsToUtf8(Source: IntPtr; Dest: IntPtr; DestLen: integer): integer;
var
 utfBuff: TBytes;
begin
  SetLength(utfBuff, 0);
  if Source = nil then
    Result := 0
  else begin
    utfBuff := Encoding.UTF8.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(Marshal.PtrToStringUni(Source));
    Result := min(DestLen, Length(utfBuff));
    Marshal.Copy(utfBuff, 0, Dest, Result);
  end;
end;

function AnsiToUtf8(Source: IntPtr; Dest: IntPtr; DestLen: integer): integer;
var
 ansiBuff: TBytes;
begin
  SetLength(ansiBuff, 0);
  if Source = nil then
    Result := 0
  else begin
    ansiBuff := Encoding.UTF8.GetBytes(Marshal.PtrToStringAnsi(Source));
    Result := Length(ansiBuff);
    Marshal.Copy(ansiBuff, 0, Dest, Result);
  end;
end;

function StringToPtrGDS(const S: _string; UseUnicode: boolean): PStr;
{$IFNDEF CLR}
var
  Size: integer;
  Str: AnsiString;
{$ENDIF}
begin
  if UseUnicode then begin
  {$IFNDEF CLR}
    Str := UTF8Encode(S);
    Size := Length(Str) + 1;
    GetMem(Result, Size);
    Move(PAnsiChar(Str)^, Result^, Size);
  {$ELSE}
    Result := Encoding.UTF8.GetBytes(S);
  {$ENDIF}
  end
  else begin
  {$IFNDEF CLR}
  {$IFNDEF IS_UNICODE}
    Result := PAnsiChar(S);
  {$ELSE}
    Str := AnsiString(S);
    Size := Length(Str) + 1;
    GetMem(Result, Size);
    Move(PAnsiChar(Str)^, Result^, Size);
  {$ENDIF}
  {$ELSE}
    Result := Encoding.Default.GetBytes(S);
  {$ENDIF}
  end;
end;

procedure FreeStringGDS(P: PStr; UseUnicode: boolean);
begin
{$IFNDEF CLR}
{$IFNDEF IS_UNICODE}
  if UseUnicode then
{$ENDIF}
    FreeMem(P);
{$ENDIF}
end;

function PtrToStringGDS(P: IntPtr; Size: integer; UseUnicode: boolean): _string;
var
{$IFNDEF CLR}
  Str: AnsiString;
{$ELSE}
  Bytes: TBytes;
{$ENDIF}
begin
  if UseUnicode then begin
  {$IFNDEF CLR}
    Str := Marshal.PtrToStringAnsi(P, Size);
    Result := Utf8Decode(Str);
  {$ELSE}
    SetLength(Bytes, Size);
    Marshal.Copy(P, Bytes, 0, Size);
    Result := Encoding.UTF8.GetString(Bytes);
  {$ENDIF}
  end
  else
    Result := _string(Marshal.PtrToStringAnsi(P, Size));
end;

{$IFDEF CLR}
function PtrToBytes(P: IntPtr; MaxSize: integer = MaxInt): TBytes;
var
  i, Size: integer;
begin
  i := 0;
  while i < MaxSize do begin
    if Marshal.ReadByte(P, i) = 0 then
      break
    else
      Inc(i);
  end;
  Size := i;
  SetLength(Result, Size);
  Marshal.Copy(P, Result, 0, Size);
end;
{$ENDIF}

function PtrToStringGDS(P: IntPtr; UseUnicode: boolean): _string;
{$IFDEF CLR}
var
  Bytes: TBytes;
{$ENDIF}
begin
  if UseUnicode then begin
  {$IFNDEF CLR}
    Result := Utf8Decode(PAnsiChar(P));
  {$ELSE}
    Bytes := PtrToBytes(P);
    Result := Encoding.UTF8.GetString(Bytes);
  {$ENDIF}
  end
  else
    Result := _string(Marshal.PtrToStringAnsi(P));
end;

initialization

  GDSList := TGDSList.Create;

finalization

  GDSList.Free;

end.

