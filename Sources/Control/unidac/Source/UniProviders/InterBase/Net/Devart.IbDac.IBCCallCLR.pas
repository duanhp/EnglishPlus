{$IFNDEF UNIDACPRO}
{$I ..\IbDac.inc}
unit Devart.IbDac.IBCCallCLR;
{$ENDIF}

interface

uses
  System.Runtime.InteropServices, System.Text,
  {$IFNDEF UNIDACPRO}IBCCall{$ELSE}IBCCallUni{$ENDIF};

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_attach_database(status_vector: PISC_STATUS; db_name_length: Short;
  db_name: IntPtr; db_handle: PISC_DB_HANDLE; parm_buffer_length: Short;
  parm_buffer: TBytes): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_array_gen_sdl(status_vector: PISC_STATUS;
  isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort; isc_arg4: IntPtr;
  isc_arg5: PShort): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_array_gen_sdl2(status_vector: PISC_STATUS; isc_array_desc: PISC_ARRAY_DESC_V2;
  isc_arg3: PShort; isc_arg4: IntPtr; isc_arg5: PShort): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_array_get_slice(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
  dest_array: IntPtr; slice_length: PISC_LONG): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_array_get_slice2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC_V2;
  dest_array: IntPtr; slice_length: PISC_LONG): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_array_lookup_bounds(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
  table_name, column_name: IntPtr; descriptor: PISC_ARRAY_DESC): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_array_lookup_bounds2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
  table_name, column_name: IntPtr; descriptor: PISC_ARRAY_DESC_V2): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_array_lookup_desc(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
  table_name, column_name: IntPtr; descriptor: PISC_ARRAY_DESC): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_array_lookup_desc2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: IntPtr;
  descriptor: PISC_ARRAY_DESC_V2): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_array_set_desc(status_vector: PISC_STATUS; table_name: IntPtr;
  column_name: IntPtr; sql_dtype, sql_length, sql_dimensions: PShort;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_array_set_desc2(status_vector: PISC_STATUS; table_name: IntPtr;
  column_name: IntPtr; sql_dtype, sql_length, sql_dimensions: PShort;
  descriptor: PISC_ARRAY_DESC_V2): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_array_put_slice(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
  source_array: IntPtr; slice_length: PISC_LONG): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_array_put_slice2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC_V2;
  source_array: IntPtr; slice_length: PISC_LONG): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
procedure isc_blob_default_desc(descriptor: PISC_BLOB_DESC;
  table_name: PUChar;  column_name: PUChar); external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
procedure isc_blob_default_desc2(descriptor: PISC_BLOB_DESC_V2;
  table_name: PUChar; column_name: PUChar); external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_blob_gen_bpb(status_vector: PISC_STATUS; to_descriptor, from_descriptor: PISC_BLOB_DESC;
  bpb_buffer_length: UShort; bpb_buffer: PUChar; bpb_length: PUShort): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_blob_gen_bpb2(status_vector: PISC_STATUS; to_descriptor, from_descriptor: PISC_BLOB_DESC_V2;
  bpb_buffer_length: UShort; bpb_buffer: PUChar; bpb_length: PUShort): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_blob_info(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  item_list_buffer_length: Short; item_list_buffer: IntPtr;
  result_buffer_length: Short; result_buffer: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_blob_lookup_desc(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: PStr;
  descriptor: PISC_BLOB_DESC; global: PUChar): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_blob_lookup_desc2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: IntPtr;
  descriptor: PISC_BLOB_DESC_v2; global: PUChar): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_blob_set_desc(status_vector: PISC_STATUS; table_name, column_name: IntPtr;
  subtype, charset, segment_size: Short; descriptor: PISC_BLOB_DESC): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_blob_set_desc2(status_vector: PISC_STATUS; table_name, column_name: IntPtr;
  subtype, charset, segment_size: Short; descriptor: PISC_BLOB_DESC_V2): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_cancel_blob(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_cancel_events(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  event_id: PISC_LONG): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_close_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;  external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_commit_retaining(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_commit_transaction(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_create_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD;
  bpb_length: Short; bpb_address: TBytes): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_database_info(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: IntPtr; result_buffer_length: Short;
  result_buffer: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
procedure isc_decode_date(ib_date: PISC_QUAD;
  var tm_date: TCTimeStructure); external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
procedure isc_decode_sql_date(ib_date: PISC_DATE;
  var tm_date: TCTimeStructure); external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
procedure isc_decode_sql_time(ib_time: PISC_TIME;
  var tm_date: TCTimeStructure); external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_detach_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_drop_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_alloc_statement2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_describe(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_describe_bind(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_execute(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_execute2(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
  dialect: UShort; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_execute_immediate(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: UShort;
  statement: IntPtr; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_fetch(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_free_statement(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  options: UShort): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_prepare(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  stmt_handle: PISC_STMT_HANDLE; length: UShort; statement: PStr;
  dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_set_cursor_name(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  cursor_name: IntPtr; _type: UShort): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_sql_info(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  item_length: Short; items: IntPtr; buffer_length: Short; buffer: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
procedure isc_encode_date(var tm_date: TCTimeStructure; ib_date: PISC_QUAD); external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
procedure isc_encode_sql_date(var tm_date: TCTimeStructure; ib_date: PISC_DATE); external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
procedure isc_encode_sql_time(var tm_date: TCTimeStructure; ib_time: PISC_TIME); external;

[DllImport(GDSDLLName, CallingConvention = CallingConvention.cdecl, CharSet = CharSet.Auto, SetLastError = True)]
function isc_event_block(var event_buffer: IntPtr; var result_buffer: IntPtr; id_count: UShort;
  event_1, event_2, event_3, event_4, event_5, event_6, event_7, event_8, event_9,
  event_10, event_11, event_12, event_13, event_14, event_15: IntPtr): ISC_LONG; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_free(buffer: IntPtr): ISC_LONG; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
procedure isc_event_counts(status_vector: PISC_STATUS; buffer_length: Short;
  event_buffer: IntPtr; result_buffer: IntPtr); external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_get_segment(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  actual_seg_length: PUShort; seg_buffer_length: UShort; seg_buffer: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_interprete(buffer: IntPtr; status_vector: PPISC_STATUS): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_open_blob2(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE;
  blob_id: PISC_QUAD; bpb_length: Short; bpb_buffer: TBytes): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_prepare_transaction2(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  msg_length: Short; msg: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_put_segment(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  seg_buffer_len: UShort; seg_buffer: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_que_events(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  event_id: PISC_LONG; length: Short; event_buffer: IntPtr;
  event_function: IntPtr; event_function_arg: IntPtr): Integer; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_release_savepoint(status_vector: PISC_STATUS; tran_handle:  PISC_TR_HANDLE;
  tran_name: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_rollback_retaining(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_rollback_savepoint(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  tran_name: IntPtr; Option: UShort): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_rollback_transaction(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_start_multiple(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  db_handle_count: Short; teb_vector_address: PISC_TEB): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_start_savepoint(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  tran_name: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_sqlcode(status_vector: PISC_STATUS): ISC_LONG; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
procedure isc_sql_interprete(sqlcode: Short; buffer: IntPtr; buffer_length: Short); external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_transaction_info(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  item_list_buffer_length: Short; item_list_buffer: IntPtr;
  result_buffer_length: Short; result_buffer: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_prepare_transaction(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_seek_blob(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  isc_arg3: Short; isc_arg4: ISC_LONG; isc_arg5: PISC_LONG): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_execute2_m(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  statement_handle: PISC_STMT_HANDLE; isc_arg4: UShort; isc_arg5: IntPtr;
  isc_arg6: UShort; isc_arg7: UShort; isc_arg8: IntPtr; isc_arg9: UShort;
  isc_arg10: IntPtr; isc_arg11: UShort; isc_arg12: UShort; isc_arg13: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_exec_immed3_m(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  tran_handle: PISC_TR_HANDLE; isc_arg4: UShort; isc_arg5: IntPtr;
  isc_arg6: UShort; isc_arg7: UShort; isc_arg8: IntPtr; isc_arg9: UShort;
  isc_arg10: UShort; isc_arg11: IntPtr; isc_arg12: UShort; isc_arg13: IntPtr;
  isc_arg14: UShort; isc_arg15: UShort; isc_arg16: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_fetch_m(status_vector: PISC_STATUS; statement_handle: PISC_STMT_HANDLE;
  isc_arg3: UShort; isc_arg4: IntPtr; isc_arg5: UShort; isc_arg6: UShort;
  isc_arg7: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_insert_m(status_vector: PISC_STATUS; statement_handle: PISC_STMT_HANDLE;
  isc_arg3: UShort; isc_arg4: IntPtr; isc_arg5: UShort;
  isc_arg6: UShort; isc_arg7: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_prepare_m(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; statement_handle: PISC_STMT_HANDLE;
  isc_arg4: UShort; isc_arg5: IntPtr; isc_arg6: UShort; isc_arg7: UShort;
  isc_arg8: IntPtr; isc_arg9: UShort; isc_arg10: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_batch_execute_immed(status_vector: IntPtr; db_handle: PISC_DB_HANDLE;
  tr_handle: PISC_TR_HANDLE; dialect: integer; number_of_sql: longword;
  sql: IntPtr; rows_affected: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function BLOB_open(blob_handle: TISC_BLOB_HANDLE;
  isc_arg2: IntPtr; isc_arg3: int): PBSTREAM; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function BLOB_put(isc_arg1: AnsiChar; isc_arg2: PBSTREAM): Int; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function BLOB_close(isc_arg1: PBSTREAM): Int; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function BLOB_get(isc_arg1: PBSTREAM): Int; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function BLOB_display(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE;
  tran_handle: TISC_TR_HANDLE; isc_arg4: IntPtr): Int; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function BLOB_dump(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE;
  tran_handle: TISC_TR_HANDLE; isc_arg4: IntPtr): Int; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function BLOB_edit(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE;
  tran_handle: TISC_TR_HANDLE; isc_arg4: IntPtr): Int; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function BLOB_load(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE;
  tran_handle: TISC_TR_HANDLE; isc_arg4: IntPtr): Int; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function BLOB_text_dump(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE;
  tran_handle: TISC_TR_HANDLE; isc_arg4: IntPtr): Int; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function BLOB_text_load(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE;
  tran_handle: TISC_TR_HANDLE; isc_arg4: IntPtr): Int; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function Bopen(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE;
  tran_handle: TISC_TR_HANDLE; isc_arg4: IntPtr): PBSTREAM; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function Bopen2(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE;
  tran_handle: TISC_TR_HANDLE; isc_arg4: IntPtr; isc_arg5: UShort): PBSTREAM; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
procedure isc_get_client_version(buffer: IntPtr); external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_get_client_major_version: Integer; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_get_client_minor_version: Integer; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_xml_fetch(status: PISC_STATUS; stmt: PISC_STMT_HANDLE;
  da_version: USHORT; sqlda: PXSQLDA; var ib_xmlda: TIB_XMLDA): Integer; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_xml_fetch_all(status: PISC_STATUS; stmt: PISC_STMT_HANDLE;
  da_version: USHORT; sqlda: PXSQLDA; var ib_xmlda: TIB_XMLDA): Integer; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_dsql_xml_buffer_fetch(status: PISC_STATUS; stmt: PISC_STMT_HANDLE;
  buffer: IntPtr; buffer_size: Integer; da_version: USHORT;
  sqlda: PXSQLDA; var ib_xmlda: TIB_XMLDA) : Integer; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_service_attach(status_vector: PISC_STATUS; isc_arg2: UShort;
  isc_arg3: IntPtr; service_handle: PISC_SVC_HANDLE; isc_arg5: UShort;
  isc_arg6: TBytes): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_service_detach(status_vector: PISC_STATUS;
  service_handle: PISC_SVC_HANDLE): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_service_query(status_vector: PISC_STATUS;
  service_handle: PISC_SVC_HANDLE; recv_handle: PISC_SVC_HANDLE; isc_arg4: UShort;
  isc_arg5: TBytes; isc_arg6: UShort; isc_arg7: TBytes; isc_arg8: UShort;
  isc_arg9: IntPtr): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function isc_service_start(status_vector: PISC_STATUS;
  service_handle: PISC_SVC_HANDLE; recv_handle: PISC_SVC_HANDLE;
  isc_arg4: UShort; isc_arg5: TBytes): ISC_STATUS; external;

[DllImport(GDSDLLName, CharSet = CharSet.Auto, SetLastError = True)]
function fb_cancel_operation(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  options: UShort): ISC_STATUS; external;

implementation
end.
