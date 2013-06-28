
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  Oracle Call Interface
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Odac.inc}
unit OraCallUni;
{$ENDIF}

{$O-}

interface

uses
{$IFDEF CLR}
  System.IO, System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
{$IFNDEF LINUX}
  Windows, Registry,
{$ENDIF}
{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  MTSCall,
{$ENDIF}
{$ENDIF}
  DAConsts, SysUtils, MemUtils;



const
{ external data types }
  SQLT_UNK            = 0;        // UNKNOWN
  SQLT_CHR            = 1;        // (ORANET TYPE) character string
  SQLT_NUM            = 2;        // (ORANET TYPE) oracle numeric
  SQLT_INT            = 3;        // (ORANET TYPE) integer
  SQLT_FLT            = 4;        // (ORANET TYPE) Floating point number
  SQLT_STR            = 5;        // zero terminated string
  SQLT_VNU            = 6;        // NUM with preceding length byte
  SQLT_PDN            = 7;        // (ORANET TYPE) Packed Decimal Numeric
  SQLT_LNG            = 8;        // long
  SQLT_VCS            = 9;        // Variable character string
  SQLT_NON            = 10;       // Null/empty PCC Descriptor entry
  SQLT_RID            = 11;       // rowid
  SQLT_DAT            = 12;       // date in oracle format
  SQLT_VBI            = 15;       // binary in VCS format
  SQLT_BFLOAT         = 21;       // Native Binary float
  SQLT_BDOUBLE        = 22;       // Native binary double
  SQLT_BIN            = 23;       // binary data(DTYBIN)
  SQLT_LBI            = 24;       // long binary
  SQLT_UND            = 25;       // UNDEFINED
  SQLT_UIN            = 68;       // unsigned integer
  SQLT_SLS            = 91;       // Display sign leading separate
  SQLT_LVC            = 94;       // Longer longs (char)
  SQLT_LVB            = 95;       // Longer long binary
  SQLT_AFC            = 96;       // Ansi fixed char
  SQLT_AVC            = 97;       // Ansi Var char
  SQLT_IBFLOAT        = 100;      // binary float canonical
  SQLT_IBDOUBLE       = 101;      // binary double canonical
  SQLT_CUR            = 102;      // cursor  type
  SQLT_RDD            = 104;      // rowid descriptor
  SQLT_LAB            = 105;      // label type
  SQLT_OSL            = 106;      // oslabel type
  SQLT_NTY            = 108;      // named object type
  SQLT_REF            = 110;      // ref type
  SQLT_CLOB           = 112;      // character lob
  SQLT_BLOB           = 113;      // binary lob
  SQLT_BFILEE         = 114;      // binary file lob
  SQLT_CFILEE         = 115;      // character file lob
  SQLT_RSET           = 116;      // result set type
  SQLT_NCO            = 122;      // named collection type (varray or nested table) table
  SQLT_VARRAY         = 123;      // ---- // ----                                   varray
  SQLT_VST            = 155;      // OCIString type
  SQLT_ODT            = 156;      // OCIDate type
  SQLT_FILE           = SQLT_BFILEE; // binary file lob
  SQLT_CFILE          = SQLT_CFILEE;
  SQLT_BFILE          = SQLT_BFILEE;
  SQLT_DATE           = 184;      // ANSI Date
  SQLT_TIME           = 185;      // TIME
  SQLT_TIME_TZ        = 186;      // TIME WITH TIME ZONE
  SQLT_TIMESTAMP      = 187;      // TIMESTAMP
  SQLT_TIMESTAMP_TZ   = 188;      // TIMESTAMP WITH TIME ZONE
  SQLT_INTERVAL_YM    = 189;      // INTERVAL YEAR TO MONTH
  SQLT_INTERVAL_DS    = 190;      // INTERVAL DAY TO SECOND
  SQLT_TIMESTAMP_LTZ  = 232;      // TIMESTAMP WITH LOCAL TZ
  SQLT_REC            = 250;      // PLSQL record
  SQLT_TAB            = 251;      // PLSQL indexed table
  SQLT_BOL            = 252;      // PLSQL boolean

type

  //TChangeNotify* type declarations moved here because of the cbuilder bug

  TChangeNotifyOperation = (cnoInsert, cnoUpdate, cnoDelete, cnoAllRows, cnoAlter, cnoDrop);
  {$NODEFINE TChangeNotifyOperation}

  TChangeNotifyDMLOperation = cnoInsert..cnoDelete;
  {$NODEFINE TChangeNotifyDMLOperation}

  TChangeNotifyOperations = set of TChangeNotifyOperation;
  {$NODEFINE TChangeNotifyOperations}
  
  TChangeNotifyDMLOperations = set of TChangeNotifyDMLOperation;
  {$NODEFINE TChangeNotifyDMLOperations}
  
  {$IFNDEF UNIDACPRO}
  (*$HPPEMIT 'namespace Oracall {'*)
  {$ELSE}
  (*$HPPEMIT 'namespace Oracalluni {'*)
  {$ENDIF}
  (*$HPPEMIT '  #pragma option push -b-'*)
  (*$HPPEMIT '  enum TChangeNotifyOperation { cnoInsert, cnoUpdate, cnoDelete, cnoAllRows, cnoAlter, cnoDrop };'*)
  (*$HPPEMIT '  typedef Set<TChangeNotifyOperation, cnoInsert, cnoDrop> TChangeNotifyOperations;'*)
  (*$HPPEMIT '  typedef TChangeNotifyOperation TChangeNotifyDMLOperation;'*)
  (*$HPPEMIT '  typedef Set<TChangeNotifyDMLOperation, cnoInsert, cnoDelete> TChangeNotifyDMLOperations;'*)
  (*$HPPEMIT '  #pragma option pop'*)
  (*$HPPEMIT '}'*)//namespace Oraclasses

  eword = integer;
  uword = cardinal;
  sword = integer;

  eb1   = shortint;
  ub1   = byte;
  sb1   = shortint;
{$IFDEF CLR}
  pub1  = IntPtr;
  psb1  = IntPtr;
  ppub1 = IntPtr;
  pppub1 = IntPtr;

  pub2  = IntPtr;
  psb2  = IntPtr;
  ppub2 = IntPtr;

  psb4  = IntPtr;
  pub4  = IntPtr;
  ppub4 = IntPtr;

  pub8  = IntPtr;
  ppub8 = IntPtr;

  pbool = IntPtr;

  PAnsiChar = string;
{$ELSE}
  pub1  = ^ub1;
  psb1  = ^sb1;
  ppub1 = ^pub1;
  pppub1 = ^ppub1;

  pub2  = ^ub2;
  psb2  = ^sb2;
  ppub2 = ^pub2;

  psb4  = ^sb4;
  pub4  = ^ub4;
  ppub4 = ^pub4;

  pub8  = ^ub8;
  ppub8 = ^pub8;

  PPointer = ^IntPtr;
  pbool = ^tbool;
{$ENDIF}

  eb2   = smallint;
  ub2   = word;
  sb2   = smallint;

  eb4   = integer;
  ub4   = cardinal;
  sb4   = integer;

  ub8   = int64;

  tbool = integer;

// use size_t data type for Integer values that is depend on platform
// in most cases size_t should be used for OUT or VAR parameters
// for IN parameters size_t data type can be replaced by Integer
{$IFNDEF CLR}
  size_t = NativeInt;
{$ELSE}
// for .Net it will be integer -> TODO
  size_t = Integer;
{$ENDIF}

{$IFDEF VER5}
  PPChar   = ^PChar;
{$ENDIF}

// ORACLE 7.3 specific

const
  CDA_SIZE = 64;
  HDA_SIZE = 256;

// internal/external datatype codes

  UNKNOWN_TYPE  = 0;
  VARCHAR2_TYPE = 1;
  NUMBER_TYPE   = 2;
  INTEGER_TYPE  = 3;
  FLOAT_TYPE    = 4;
  STRING_TYPE   = 5;
  LONG_TYPE     = 8;
  ROWID_TYPE    = 11;
  DATE_TYPE     = 12;
  RAW_TYPE      = 23;
  LONGRAW_TYPE  = 24;
  CHAR_TYPE     = 96;
  CHARZ_TYPE    = 97;
  CURSOR_TYPE   = 102;


// ORACLE error codes

  OCI_VAR_NOT_IN_LIST     = -303;  //1007;
  OCI_NO_DATA_FOUND       = 4;     //1403;
  OCI_NULL_VALUE_RETURNED = 1405;
  OCI_BLOCKED             = -3123;
  OCI_CONNECTION_BUSY     = -3127;
// Session error
  OCI_SESSION_KILLED      = 28;
  OCI_NOT_LOGGEDON        = 1012;
  OCI_EOF_COMMUNICATION   = 3113;
  OCI_NOT_CONNECTED       = 3114;
  OCI_NO_INTERFACE        = 3121;

  OCI_STILL_IS_PIECE      = -3130;
  OCI_STILL_IS_PIECE1     = -3129;
  OCI_BREAKED             = -1013;

// SQL function codes

  SQL_UNKNOWN = 0;
  SQL_SET_ROLE = 2;
  SQL_INSERT  = 3;
  SQL_SELECT  = 4;
  SQL_UPDATE  = 5;
  SQL_DELETE  = 9;
  SQL_EXPLAIN = 27;
  SQL_GRANT = 28;
  SQL_REVOKE = 29;
  SQL_SET_TRANSACTION = 33;
  SQL_PLSQL   = 34;
  SQL_LOCK = 35;
  SQL_RENAME = 37;
  SQL_COMMENT = 38;
  SQL_AUDIT = 39;
  SQL_NOAUDIT = 40;
  SQL_COMMIT  = 54;
  SQL_ROLLBACK = 55;
  SQL_SAVEPOINT = 56;
  SQL_CALL_METHOD = 170;

  FC_OOPEN = 14;

{ OCI Environment Modes for opinit call }

  OCI_EV_DEF = 0;                    // default single-threaded environment
  OCI_EV_TSF = 1;                    // thread-safe environment

{ OCI Logon Modes for olog call }

  OCI_LM_DEF = 0;                                   // default login
  OCI_LM_NBL = 1;                                   // non-blocking logon

{ Piece Definitions }

  OCI_ONE_PIECE   = 0;                 // there or this is the only piece
  OCI_FIRST_PIECE = 1;                 // the first of many pieces
  OCI_NEXT_PIECE  = 2;                 // the next of many pieces
  OCI_LAST_PIECE  = 3;                 // the last piece of this column

{ for parse }

  OCI_PARSE_NODEFER = 0;
  OCI_PARSE_DEFER = 1;
  OCI_LANG_V6 = 0;
  OCI_LANG_NORM = 1;
  OCI_LANG_V7 = 2;

{ CHAR/NCHAR/VARCHAR2/NVARCHAR2/CLOB/NCLOB char set "form" information }
  SQLCS_IMPLICIT  = 1; //* for CHAR, VARCHAR2, CLOB w/o a specified set */
  SQLCS_NCHAR     = 2; //* for NCHAR, NCHAR VARYING, NCLOB */
  SQLCS_EXPLICIT  = 3; //* for CHAR, etc, with "CHARACTER SET ..." syntax */
  SQLCS_FLEXIBLE  = 4; //* for PL/SQL "flexible" parameters */
  SQLCS_LIT_NULL  = 5; //* for typecheck of NULL and empty_clob() lits */

type
{$IFDEF CLR}
  PTRD = record
  private
    Ptr: IntPtr;
    function GetUB1Property(Index: Integer): ub1;
    procedure SetUB1Property(Index: Integer; Value: ub1);
    function GetUB2Property(Index: Integer): ub2;
    procedure SetUB2Property(Index: Integer; Value: ub2);
    function GetUB4Property(Index: Integer): ub4;
    procedure SetUB4Property(Index: Integer; Value: ub4);

  public
    property rcs4 : ub4 index 0 read GetUB4Property write SetUB4Property;
    property rcs5 : ub2 index 4 read GetUB2Property write SetUB2Property;
    property rcs6 : ub1 index 6 read GetUB1Property write SetUB1Property;

    class operator Implicit(AValue: IntPtr): PTRD;
    class operator Implicit(AValue: PTRD): IntPtr;
  end;
{$ENDIF}
  TRD = packed record
    rcs4 : ub4;   // obj num
    rcs5 : ub2;   // file num
    rcs6 : ub1;
  end;
{$IFNDEF CLR}
  PTRD = ^TRD;
{$ENDIF}

{ rowid structure }

{$IFDEF CLR}
  PRowId7 = record
  private
    Ptr: IntPtr;
    function GetUB4Property(Index: Integer): ub4;
    procedure SetUB4Property(Index: Integer; Value: ub4);
    function GetTRDProperty(Index: Integer): PTRD;

  public
    property rd   : PTRD index 0 read GetTRDProperty;
    property rcs7 : ub4 index 7 read GetUB4Property write SetUB4Property;
    property rcs8 : ub4 index 11 read GetUB4Property write SetUB4Property;

    class operator Implicit(AValue: IntPtr): PRowId7;
    class operator Implicit(AValue: PRowId7): IntPtr;
  end;
{$ENDIF}
 TRowId7 = packed record
    rd   : TRD;
    rcs7 : ub4;   // block num
    rcs8 : ub4;   // for Oracle7 - ub2;   // slot num
  end;
type
{$IFNDEF CLR}
  PRowId7 = ^TRowId7;
{$ENDIF}

{ The cda_head struct is strictly PRIVATE.  It is used
   internally only. Do not use this struct in OCI programs. }

  TCDAHead = packed record
    v2_rc : sb2;
    ft    : ub2;
    rpc   : ub4;
    peo   : ub2;
    fc    : ub1;
    rcs1  : ub1;
    rc    : ub2;
    wrn   : ub1;
    rcs2  : ub1;
    rcs3  : sword;
    rid   : TRowId7;
    ose   : sword;
    chk   : ub1;
    rcsp  : IntPtr;
  end;

const
{$IFDEF CLR}
  SizeOfTCDAHead = 44;
{$ELSE}
  SizeOfTCDAHead = sizeof(TCDAHead);
{$ENDIF}

type
{ the real CDA, padded to CDA_SIZE bytes in size }

  TCDA = packed record
    v2_rc : sb2;         { V2 return code }
    ft    : ub2;         { SQL function type }
    rpc   : ub4;         { rows processed count }
    peo   : ub2;         { parse error offset }
    fc    : ub1;         { OCI function code }
    rcs1  : ub1;         { filler area }
    rc    : ub2;         { V7 return code }
    wrn   : ub1;         { warning flags }
    rcs2  : ub1;         { reserved }
    rcs3  : sword;       { reserved }
    rid   : TRowId7;     { rowid }
    ose   : sword;       { OSD dependent error }
    chk   : ub1;
    rcsp  : IntPtr;     { pointer to reserved area }
    rcs9  : array [0 .. CDA_SIZE - SizeOfTCDAHead - 1] of ub1; { filler }
  end;
{$IFDEF CLR}
  PCDA = packed record
  private
    Ptr: IntPtr;

    function GetSB2Property(Index: Integer): sb2;
    procedure SetSB2Property(Index: Integer; Value: sb2);
    function GetSB4Property(Index: Integer): sb4;
    procedure SetSB4Property(Index: Integer; Value: sb4);
    function GetUB1Property(Index: Integer): ub1;
    procedure SetUB1Property(Index: Integer; Value: ub1);
    function GetUB2Property(Index: Integer): ub2;
    procedure SetUB2Property(Index: Integer; Value: ub2);
    function GetUB4Property(Index: Integer): ub4;
    procedure SetUB4Property(Index: Integer; Value: ub4);
    function GetIntPtrProperty(Index: Integer): IntPtr;
    procedure SetIntPtrProperty(Index: Integer; Value: IntPtr);
    function GetTRowId7Property(Index: Integer): PRowId7;
    procedure SetArrayProperty(Index: Integer; ArrayInd: integer; Value: ub1);
    function GetArrayProperty(Index: Integer; ArrayInd: integer): ub1;

  public
    { V2 return code }
    property v2_rc: sb2 index 0 read GetSB2Property write SetSB2Property;
    { SQL function type }
    property ft: ub2 index 2 read GetUB2Property write SetUB2Property;
    { rows processed count }
    property rpc: ub4 index 4 read GetUB4Property write SetUB4Property;
    { parse error offset }
    property peo: ub2 index 8 read GetUB2Property write SetUB2Property;
    { OCI function code }
    property fc: ub1 index 10 read GetUB1Property write SetUB1Property;
    { filler area }
    property rcs1: ub1 index 11 read GetUB1Property write SetUB1Property;
    { V7 return code }
    property rc: ub2 index 12 read GetUB2Property write SetUB2Property;
    { warning flags }
    property wrn: ub1 index 14 read GetUB1Property write SetUB1Property;
    { reserved }
    property rcs2: ub1 index 15 read GetUB1Property write SetUB1Property;
    { reserved }
    property rcs3: sb4 index 16 read GetSB4Property write SetSB4Property;
    { rowid }
    property rid: PRowId7 index 20 read GetTRowId7Property;
    { OSD dependent error }
    property ose: sb4 index 35 read GetSB4Property write SetSB4Property;
    property chk: ub1 index 39 read GetUB1Property write SetUB1Property;
    { pointer to reserved area }
    property rcsp: IntPtr index 40 read GetIntPtrProperty write SetIntPtrProperty;
    { filler }
    property rcs9[i: integer]: ub1  index 44 read GetArrayProperty write SetArrayProperty;

    class operator Implicit(AValue: IntPtr): PCDA;
    class operator Implicit(AValue: PCDA): IntPtr;
  end;
{$ELSE}
  PCDA = ^TCDA;
{$ENDIF}

{ the logon data area (LDA) is the same shape as the CDA }

  TLDA = TCDA;
  PLDA = PCDA;

{ host data area }

  THDA = packed array [1 .. HDA_SIZE] of byte;
{$IFNDEF CLR}
  PHDA = ^THDA;
{$ELSE}
  PHDA = IntPtr;
{$ENDIF}

{ Declare the OCI functions }

  _obindps = function (cursor: IntPtr; opcode: ub1; sqlvar: PAnsiChar; sqlvl: sb4;
               pvctx: pub1; progvl: sb4; ftype: sword; scale: sword; indp: psb2;
               alen: pub2; arcode: pub2; pv_skip: sb4; ind_skip: sb4; alen_skip: sb4;
               rc_skip: sb4; maxsiz: ub4; cursiz: pub4; fmt: IntPtr; fmtl: sb4;
               fmtt: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _obreak = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ocan = function (cursor: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oclose = function (cursor: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ocof = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ocom = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ocon = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _odefinps = function (cursor: IntPtr; opcode: ub1; pos: sword; bufctx: IntPtr;
                bufl: sb4; ftype: sword; scale: sword; indp: psb2; fmt: IntPtr;
                fmtl: sb4; fmtt: sword; rlen: pub2; rcode: pub2; pv_skip: sb4;
                ind_skip: sb4; alen_skip: sb4; rc_skip: sb4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _odessp = function (lda: IntPtr; objnam: PAnsiChar; onlen: size_t; rsv1: pub1;
              rsv1ln: size_t; rsv2: pub1; rsv2ln: size_t; ovrld: pub2; pos: pub2;
              level: pub2; argnam: IntPtr; arnlen: pub2; dtype: pub2; defsup: pub1;
              mode: pub1; dtsiz: pub4; prec: psb2; scale: psb2; radix: pub1;
              spare: pub4; var arrsiz: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _odescr = function (cursor: IntPtr; pos: sword; var dbsize: sb4; var dbtype: sb2;
              cbuf: IntPtr; var cbufl: sb4; var dsize: sb4; var prec: sb2; var scale: sb2;
              var nullok: sb2): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oerhms = function (lda: IntPtr; rcode: sb2; buf: IntPtr; bufsiz: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oermsg = function (rcode: sb2; buf: PAnsiChar): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oexec = function (cursor: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oexfet = function (cursor: IntPtr; nrows: ub4; cancel: sword; exact: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oexn = function (cursor: IntPtr; iters: sword; rowoff: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ofen = function (cursor: IntPtr; nrows: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ofetch = function (cursor: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oflng = function (cursor: IntPtr; pos: sword; buf: pub1; bufl: sb4; dtype: sword;
             retl: pub4; offset: sb4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ogetpi = function (cursor: IntPtr; var piecep: ub1; var ctxpp: IntPtr; var iterp: ub4;
              var indexp: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oopt = function (cursor: IntPtr; rbopt: sword; waitopt: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _opinit = function (mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _olog = function (lda: IntPtr; hda: PHDA; uid: PAnsiChar; uidl: sword; pswd: PAnsiChar;
            pswdl: sword; conn: PAnsiChar; connl: sword; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ologof = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oopen = function (cursor: IntPtr; lda: IntPtr; dbn: IntPtr; dbnl: sword; arsize: sword;
             uid: IntPtr; uidl: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oparse = function (cursor: IntPtr; sqlstm: PAnsiChar; sqllen: sb4; defflg: sword;
              lngflg: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _orol = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _osetpi = function (cursor: IntPtr; piece: ub1; bufp: IntPtr; var lenp: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _sqlld2 = procedure (lda: IntPtr; cname: PAnsiChar; cnlen: psb4); {$IFNDEF CLR} cdecl; {$ENDIF}

  _sqllda = procedure (lda: IntPtr); {$IFNDEF CLR} cdecl; {$ENDIF}

  _onbset = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _onbtst = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _onbclr = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ognfd  = function(lda: IntPtr; fdp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  { OBSOLETE DEFINE CALLS }

  _obndra = function (cursor: IntPtr; sqlvar: PAnsiChar; sqlvl: sword;
                 progv: pub1; progvl: sword; ftype: sword; scale: sword;
                 indp: psb2; alen: pub2; arcode: pub2; maxsiz: ub4;
                 cursiz: pub4; fmt: IntPtr; fmtl: sword; fmtt: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _obndrn = function (cursor: IntPtr; sqlvn: sword; progv: pub1; progvl: sword;
              ftype: sword; scale: sword; indp: psb2; fmt: PAnsiChar; fmtl: sword;
              fmtt: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _obndrv = function (cursor: IntPtr; sqlvar: PAnsiChar; sqlvl: sword; progv: IntPtr;
              progvl: sword; ftype: sword; scale: sword; indp: psb2; fmt: IntPtr;
              fmtl: sword; fmtt: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _odefin = function (cursor: IntPtr; pos: sword; buf: IntPtr; bufl: sword; ftype: sword;
              scale: sword; indp: psb2; fmt: IntPtr; fmtl: sword; fmtt: sword;
              rlen: pub2; rcode: pub2): sword; {$IFNDEF CLR} cdecl; {$ENDIF}



var
  obindps  : _obindps;
  obndra   : _obndra;
  obndrn   : _obndrn;
  obndrv   : _obndrv;
  obreak   : _obreak;
  ocan     : _ocan;
  oclose   : _oclose;
  ocof     : _ocof;
  ocom     : _ocom;
  ocon     : _ocon;
  odefin   : _odefin;
  odefinps : _odefinps;
  odescr   : _odescr;
  odessp   : _odessp;
  oerhms   : _oerhms;
  oermsg   : _oermsg;
  oexec    : _oexec;
  oexfet   : _oexfet;
  oexn     : _oexn;
  ofen     : _ofen;
  ofetch   : _ofetch;
  oflng    : _oflng;
  ognfd    : _ognfd;
  olog     : _olog;
  ologof   : _ologof;
  onbclr   : _onbclr;
  onbset   : _onbset;
  onbtst   : _onbtst;
  oopt     : _oopt;
  oopen    : _oopen;
  oparse   : _oparse;
  opinit   : _opinit;
  orol     : _orol;
  ogetpi   : _ogetpi;
  osetpi   : _osetpi;
  sqllda   : _sqllda;
  sqlld2   : _sqlld2;

// ORACLE 8.0 specific

const
{ Modes }

  OCI_DEFAULT              = $00;   // the default value for parameters and attributes
  OCI_THREADED             = $01;   // the application is in threaded environment
  OCI_OBJECT               = $02;   // the application is in object environment
  OCI_EVENTS               = $04;   // the application is enabled for events
  OCI_SHARED               = $10;   // the application is in shared mode
  OCI_NON_BLOCKING         = $04;   // ??? non blocking mode of operation

  OCI_ENV_NO_MUTEX         = $08;   // the environment handle will not be
  OCI_NO_MUTEX             = $80;   // protected by a mutex internally
  OCI_UTF16                = $4000; // mode for all UTF16 metadata

  OCI_SESSGET_SPOOL        = $01;   // enables OCI session pooling
  OCI_SESSGET_STMTCACHE    = $04;   // Use statement cache


  OCI_SPC_REINITIALIZE     = $01;   // Reinitialize the session pool
  OCI_SPC_HOMOGENEOUS      = $02;   // Session pool is homogeneneous
  OCI_SPC_STMTCACHE        = $04;   // Session pool has stmt cache

{ Handle Types }
                                  // handle types range from 1 - 49 */
  OCI_HTYPE_FIRST          = 1;    // start value of handle type
  OCI_HTYPE_ENV            = 1;    // environment handle
  OCI_HTYPE_ERROR          = 2;    // error handle
  OCI_HTYPE_SVCCTX         = 3;    // service handle
  OCI_HTYPE_STMT           = 4;    // statement handle
  OCI_HTYPE_BIND           = 5;    // bind handle
  OCI_HTYPE_DEFINE         = 6;    // define handle
  OCI_HTYPE_DESCRIBE       = 7;    // describe handle
  OCI_HTYPE_SERVER         = 8;    // server handle
  OCI_HTYPE_SESSION        = 9;    // authentication handle
  OCI_HTYPE_AUTHINFO       = OCI_HTYPE_SESSION;
  OCI_HTYPE_TRANS          = 10;   // transaction handle
  OCI_HTYPE_COMPLEXOBJECT  = 11;   // complex object retrieval handle
  OCI_HTYPE_SECURITY       = 12;   // security handle
  OCI_HTYPE_SUBSCRIPTION   = 13;   // subscription handle
  OCI_HTYPE_DIRPATH_CTX    = 14;   // direct path context
  OCI_HTYPE_DIRPATH_COLUMN_ARRAY = 15;  // direct path column array
  OCI_HTYPE_DIRPATH_STREAM = 16;   // direct path stream
  OCI_HTYPE_PROC           = 17;   // process handle
  OCI_HTYPE_LAST           = 17;   // last value of a handle type
  OCI_HTYPE_SPOOL          = 27;   // session pool handle 

{ Descriptor Types }
                                    // descriptor values range from 50 - 255
  OCI_DTYPE_FIRST             = 50; // start value of descriptor type
  OCI_DTYPE_LOB               = 50; // lob  locator
  OCI_DTYPE_SNAP              = 51; // snapshot descriptor
  OCI_DTYPE_RSET              = 52; // result set descriptor
  OCI_DTYPE_PARAM             = 53; // a parameter descriptor obtained from ocigparm
  OCI_DTYPE_ROWID             = 54; // rowid descriptor
  OCI_DTYPE_COMPLEXOBJECTCOMP = 55; // complex object retrieval descriptor
  OCI_DTYPE_FILE              = 56; // File Lob locator
  OCI_DTYPE_AQENQ_OPTIONS     = 57; // enqueue options
  OCI_DTYPE_AQDEQ_OPTIONS     = 58; // dequeue options
  OCI_DTYPE_AQMSG_PROPERTIES  = 59; // message properties
  OCI_DTYPE_AQAGENT           = 60; // aq agent
  OCI_DTYPE_INTERVAL_YM       = 62; // Interval year month
  OCI_DTYPE_INTERVAL_DS       = 63; // Interval day second
  OCI_DTYPE_AQNFY_DESCRIPTOR  = 64; // AQ notify descriptor
  OCI_DTYPE_DATE              = 65; // Date
  OCI_DTYPE_TIME              = 66; // Time
  OCI_DTYPE_TIME_TZ           = 67; // Time with timezone
  OCI_DTYPE_TIMESTAMP         = 68; // Timestamp
  OCI_DTYPE_TIMESTAMP_TZ      = 69; // Timestamp with timezone
  OCI_DTYPE_TIMESTAMP_LTZ     = 70; // Timestamp with local tz
  OCI_DTYPE_UCB               = 71; // user callback descriptor
  OCI_DTYPE_SRVDN             = 72; // server DN list descriptor
  OCI_DTYPE_SIGNATURE         = 73; // signature
  OCI_DTYPE_RESERVED_1        = 74; // reserved for internal use
  OCI_DTYPE_AQLIS_OPTIONS     = 75; // AQ listen options
  OCI_DTYPE_AQLIS_MSG_PROPERTIES = 76; // AQ listen msg props
  OCI_DTYPE_CHDES             = 77; // Top level change notification desc
  OCI_DTYPE_TABLE_CHDES       = 78; // Table change descriptor
  OCI_DTYPE_ROW_CHDES         = 79; // Row change descriptor

{ Object Ptr Types }

  OCI_OTYPE_NAME           = 1;    // object name
  OCI_OTYPE_REF            = 2;    // REF to TDO
  OCI_OTYPE_PTR            = 3;    // PTR to TDO

{ Attribute Types }

  OCI_ATTR_FNCODE          = 1;
  OCI_ATTR_OBJECT          = 2;
  OCI_ATTR_NONBLOCKING_MODE  = 3;
  OCI_ATTR_SQLCODE         = 4;
  OCI_ATTR_ENV             = 5;
  OCI_ATTR_SERVER          = 6;
  OCI_ATTR_SESSION         = 7;
  OCI_ATTR_TRANS           = 8;
  OCI_ATTR_ROW_COUNT       = 9;
  OCI_ATTR_SQLFNCODE       = 10;
  OCI_ATTR_PREFETCH_ROWS   = 11;
  OCI_ATTR_NESTED_PREFETCH_ROWS  = 12;
  OCI_ATTR_PREFETCH_MEMORY = 13;
  OCI_ATTR_NESTED_PREFETCH_MEMORY  = 14;
  OCI_ATTR_CHAR_COUNT      = 15;    // this specifies the bind and define size in characters
  OCI_ATTR_PDSCL           = 16;
  OCI_ATTR_FSPRECISION     = OCI_ATTR_PDSCL;   
  OCI_ATTR_PDFMT           = 17;
  OCI_ATTR_PARAM_COUNT     = 18;
  OCI_ATTR_ROWID           = 19;
  OCI_ATTR_CHARSET         = 20;
  OCI_ATTR_NCHAR           = 21;
  OCI_ATTR_USERNAME        = 22;    // username attribute
  OCI_ATTR_PASSWORD        = 23;    // password attribute
  OCI_ATTR_STMT_TYPE       = 24;    // statement type
  OCI_ATTR_INTERNAL_NAME   = 25;
  OCI_ATTR_EXTERNAL_NAME   = 26;
  OCI_ATTR_XID             = 27;
  OCI_ATTR_TRANS_LOCK      = 28;
  OCI_ATTR_TRANS_NAME      = 29;
  OCI_ATTR_HEAPALLOC       = 30;    // memory allocated on the heap
  OCI_ATTR_CHARSET_ID      = 31;    // Character Set ID
  OCI_ATTR_CHARSET_FORM    = 32;    // Character Set Form
  OCI_ATTR_MAXDATA_SIZE    = 33;    // Maximumsize of data on the server
  OCI_ATTR_CACHE_OPT_SIZE  = 34;    // object cache optimal size
  OCI_ATTR_CACHE_MAX_SIZE  = 35;    // object cache maximum size percentage
  OCI_ATTR_PINOPTION       = 36;    // object cache default pin option
  OCI_ATTR_ALLOC_DURATION  = 37;    // object cache default allocation duration
  OCI_ATTR_PIN_DURATION    = 38;    // object cache default pin duration
  OCI_ATTR_FDO             = 39;    // Format Descriptor object attribute
  OCI_ATTR_POSTPROCESSING_CALLBACK = 40;   // Callback to process outbind data
  OCI_ATTR_POSTPROCESSING_CONTEXT  = 41;  // Callback context to process outbind data
  OCI_ATTR_ROWS_RETURNED   = 42;    // Number of rows returned in current iter - for Bind handles
  OCI_ATTR_FOCBK           = 43;    // Failover Callback attribute
  OCI_ATTR_IN_V8_MODE      = 44;    // is the server/service context in V8 mode
  OCI_ATTR_LOBEMPTY        = 45;
  OCI_ATTR_SESSLANG        = 46;    // session language handle

{ AQ Attribute Types }
{ Enqueue Options }

  OCI_ATTR_VISIBILITY      = 47;    // visibility
  OCI_ATTR_RELATIVE_MSGID  = 48;    // relative message id
  OCI_ATTR_SEQUENCE_DEVIATION = 49; // sequence deviation

{ Dequeue Options }

  OCI_ATTR_CONSUMER_NAME   = 50;    // consumer name
  OCI_ATTR_DEQ_MODE        = 51;    // dequeue mode
  OCI_ATTR_NAVIGATION      = 52;    // navigation
  OCI_ATTR_WAIT            = 53;    // wait
  OCI_ATTR_DEQ_MSGID       = 54;    // dequeue message id

{ Message Properties }

  OCI_ATTR_PRIORITY        = 55;    // priority
  OCI_ATTR_DELAY           = 56;    // delay
  OCI_ATTR_EXPIRATION      = 57;    // expiration
  OCI_ATTR_CORRELATION     = 58;    // correlation id
  OCI_ATTR_ATTEMPTS        = 59;    // # of attempts
  OCI_ATTR_RECIPIENT_LIST  = 60;    // recipient list
  OCI_ATTR_EXCEPTION_QUEUE = 61;    // exception queue name
  OCI_ATTR_ENQ_TIME        = 62;    // enqueue time (only OCIAttrGet)
  OCI_ATTR_MSG_STATE       = 63;    // message state (only OCIAttrGet)

{ AQ Agent }

  OCI_ATTR_AGENT_NAME      = 64;    // agent name
  OCI_ATTR_AGENT_ADDRESS   = 65;    // agent address
  OCI_ATTR_AGENT_PROTOCOL  = 66;    // agent protocol

  OCI_ATTR_SENDER_ID       = 68;    // sender id
  OCI_ATTR_ORIGINAL_MSGID  = 69;    // original message id

  OCI_ATTR_QUEUE_NAME      = 70;    // queue name
  OCI_ATTR_NFY_MSGID       = 71;    // message id
  OCI_ATTR_MSG_PROP        = 72;    // message properties

  OCI_ATTR_NUM_DML_ERRORS  = 73;    // num of errs in array DML
  OCI_ATTR_DML_ROW_OFFSET  = 74;    // row offset in the array

  OCI_ATTR_DATEFORMAT      = 75;    // default date format string
  OCI_ATTR_BUF_ADDR        = 76;    // buffer address
  OCI_ATTR_BUF_SIZE        = 77;    // buffer size
  OCI_ATTR_DIRPATH_MODE    = 78;    // mode of direct path operation
  OCI_ATTR_DIRPATH_NOLOG   = 79;    // nologging option
  OCI_ATTR_DIRPATH_PARALLEL = 80;   // parallel (temp seg) option
  OCI_ATTR_NUM_ROWS        = 81;    // number of rows in column array
   // NOTE that OCI_ATTR_NUM_COLS is a column array attribute too.

  OCI_ATTR_COL_COUNT       = 82;    // columns of column array processed so far.
  OCI_ATTR_STREAM_OFFSET   = 83;    // str off of last row processed
  OCI_ATTR_SHARED_HEAPALLOC = 84;   // Shared Heap Allocation Size

  OCI_ATTR_SERVER_GROUP    = 85;    // server group name

  OCI_ATTR_MIGSESSION      = 86;    // migratable session attribute

  OCI_ATTR_NOCACHE         = 87;    // Temporary LOBs

  OCI_ATTR_MEMPOOL_SIZE    = 88;    // Pool Size
  OCI_ATTR_MEMPOOL_INSTNAME = 89;   // Instance name
  OCI_ATTR_MEMPOOL_APPNAME = 90;    // Application name
  OCI_ATTR_MEMPOOL_HOMENAME = 91;   // Home Directory name
  OCI_ATTR_MEMPOOL_MODEL   = 92;    // Pool Model (proc,thrd,both)
  OCI_ATTR_MODES           = 93;    // Modes

  OCI_ATTR_SUBSCR_NAME     = 94;    // name of subscription
  OCI_ATTR_SUBSCR_CALLBACK = 95;    // associated callback
  OCI_ATTR_SUBSCR_CTX      = 96;    // associated callback context
  OCI_ATTR_SUBSCR_PAYLOAD  = 97;    // associated payload
  OCI_ATTR_SUBSCR_NAMESPACE = 98;   // associated namespace

  OCI_ATTR_PROXY_CREDENTIALS = 99;  // Proxy user credentials
  OCI_ATTR_INITIAL_CLIENT_ROLES = 100;// Initial client role list

{ Parameter Attribute Types }

  OCI_ATTR_UNK             = 101;   // unknown attribute
  OCI_ATTR_NUM_COLS        = 102;   // number of columns
  OCI_ATTR_LIST_COLUMNS    = 103;   // parameter of the column list
  OCI_ATTR_RDBA            = 104;   // DBA of the segment header
  OCI_ATTR_CLUSTERED       = 105;   // whether the table is clustered
  OCI_ATTR_PARTITIONED     = 106;   // whether the table is partitioned
  OCI_ATTR_INDEX_ONLY      = 107;   // whether the table is index only
  OCI_ATTR_LIST_ARGUMENTS  = 108;   // parameter of the argument list
  OCI_ATTR_LIST_SUBPROGRAMS = 109;  // parameter of the subprogram list
  OCI_ATTR_REF_TDO         = 110;   // REF to the type descriptor
  OCI_ATTR_LINK            = 111;   // the database link name
  OCI_ATTR_MIN             = 112;   // minimum value
  OCI_ATTR_MAX             = 113;   // maximum value
  OCI_ATTR_INCR            = 114;   // increment value
  OCI_ATTR_CACHE           = 115;   // number of sequence numbers cached
  OCI_ATTR_ORDER           = 116;   // whether the sequence is ordered
  OCI_ATTR_HW_MARK         = 117;   // high-water mark
  OCI_ATTR_TYPE_SCHEMA     = 118;   // type's schema name
  OCI_ATTR_TIMESTAMP       = 119;   // timestamp of the object
  OCI_ATTR_NUM_ATTRS       = 120;   // number of sttributes
  OCI_ATTR_NUM_PARAMS      = 121;   // number of parameters
  OCI_ATTR_OBJID           = 122;   // object id for a table or view
  OCI_ATTR_PTYPE           = 123;   // type of info described by
  OCI_ATTR_PARAM           = 124;   // parameter descriptor
  OCI_ATTR_OVERLOAD_ID     = 125;   // overload ID for funcs and procs
  OCI_ATTR_TABLESPACE      = 126;   // table name space
  OCI_ATTR_TDO             = 127;   // TDO of a type
  OCI_ATTR_LTYPE           = 128;   // list type
  OCI_ATTR_PARSE_ERROR_OFFSET = 129;  // Parse Error offset
  OCI_ATTR_IS_TEMPORARY    = 130;   // whether table is temporary
  OCI_ATTR_IS_TYPED        = 131;   // whether table is typed
  OCI_ATTR_DURATION        = 132;   // duration of temporary table
  OCI_ATTR_IS_INVOKER_RIGHTS = 133; // is invoker rights
  OCI_ATTR_OBJ_NAME        = 134;   // top level schema obj name
  OCI_ATTR_OBJ_SCHEMA      = 135;   // schema name
  OCI_ATTR_OBJ_ID          = 136;   // top level schema object id

  OCI_ATTR_MAXCHAR_SIZE     = 163;  // max char size of data
  OCI_ATTR_CURRENT_POSITION = 164;  // for scrollable result sets

  OCI_ATTR_STMTCACHESIZE    = 176;  // size of the stm cache
  OCI_ATTR_STMT_STATE       = 182;
  OCI_ATTR_ENV_UTF16        = 209;  // is env in utf16 mode?

{ notification subscription }

  OCI_ATTR_SUBSCR_QOSFLAGS         = 225;   // QOS flags
  OCI_ATTR_SUBSCR_PAYLOADCBK       = 226;   // Payload callback
  OCI_ATTR_SUBSCR_TIMEOUT          = 227;   // Timeout
  OCI_ATTR_SUBSCR_NAMESPACE_CTX    = 228;   // Namespace context

  OCI_ATTR_CLIENT_IDENTIFIER       = 278;   // User identifier in the session handle

  OCI_ATTR_IS_XMLTYPE              = 315;   // Is the type an XML type?
  OCI_ATTR_XMLSCHEMA_NAME          = 316;   // Name of XML Schema
  OCI_ATTR_XMLELEMENT_NAME         = 317;   // Name of XML Element
  OCI_ATTR_XMLSQLTYPSCH_NAME       = 318;   // SQL type's schema for XML Ele
  OCI_ATTR_XMLSQLTYPE_NAME         = 319;   // Name of SQL type for XML Ele
  OCI_ATTR_XMLTYPE_STORED_OBJ      = 320;   // XML type stored as object?

  OCI_ATTR_TRANSACTION_NO          = 365;   // AQ enq txn number

{ port no attribute in subscription handle }

  OCI_ATTR_SUBSCR_PORTNO           = 390;   // port no to listen

{ DB Change Notification reg handle attributes }

  OCI_ATTR_CHNF_TABLENAMES         = 401;   // out: array of table names
  OCI_ATTR_CHNF_ROWIDS             = 402;   // in: rowids needed
  OCI_ATTR_CHNF_OPERATIONS         = 403;   // in: notification operation filter
  OCI_ATTR_CHNF_CHANGELAG          = 404;   // txn lag between notifications

{ DB Change: Notification Descriptor attributes }

  OCI_ATTR_CHDES_DBNAME            = 405;   // source database
  OCI_ATTR_CHDES_NFYTYPE           = 406;   // notification type flags
  OCI_ATTR_CHDES_XID               = 407;   // XID  of the transaction
  OCI_ATTR_CHDES_TABLE_CHANGES     = 408;   // array of table chg descriptors

  OCI_ATTR_CHDES_TABLE_NAME        = 409;   // table name
  OCI_ATTR_CHDES_TABLE_OPFLAGS     = 410;   // table operation flags
  OCI_ATTR_CHDES_TABLE_ROW_CHANGES = 411;   // array of changed rows
  OCI_ATTR_CHDES_ROW_ROWID         = 412;   // rowid of changed row
  OCI_ATTR_CHDES_ROW_OPFLAGS       = 413;   // row operation flags

{ Statement handle attribute for db change notification }
  OCI_ATTR_CHNF_REGHANDLE          = 414;   // IN: subscription handle

  OCI_ATTR_MSG_DELIVERY_MODE       = 407;   // msg delivery mode

{ Specific for Net = true}
  OCI_ATTR_CONNECTION_TIMEOUT      = 10001;

{ DB Change: Event types }

  OCI_EVENT_NONE                   = 0;     // None
  OCI_EVENT_STARTUP                = 1;     // Startup database
  OCI_EVENT_SHUTDOWN               = 2;     // Shutdown database
  OCI_EVENT_SHUTDOWN_ANY           = 3;     // Startup instance
  OCI_EVENT_DROP_DB                = 4;     // Drop database
  OCI_EVENT_DEREG                  = 5;     // Subscription deregistered
  OCI_EVENT_OBJCHANGE              = 6;     // Object change notification

{ DB Change: Operation types }

  OCI_OPCODE_ALLROWS               = $1;    // all rows invalidated
  OCI_OPCODE_ALLOPS                = $0;    // interested in all operations
  OCI_OPCODE_INSERT                = $2;    // INSERT
  OCI_OPCODE_UPDATE                = $4;    // UPDATE
  OCI_OPCODE_DELETE                = $8;    // DELETE
  OCI_OPCODE_ALTER                 = $10;   // ALTER
  OCI_OPCODE_DROP                  = $20;   // DROP TABLE
  OCI_OPCODE_UNKNOWN               = $40;   // GENERIC/ UNKNOWN

{ Supported QOS values for notification registrations }

  OCI_SUBSCR_QOS_RELIABLE          = $1;    // reliable
  OCI_SUBSCR_QOS_PAYLOAD           = $2;    // payload delivery
  OCI_SUBSCR_QOS_REPLICATE         = $4;    // replicate to director
  OCI_SUBSCR_QOS_SECURE            = $8;    // secure payload delivery
  OCI_SUBSCR_QOS_PURGE_ON_NTFN     = $10;   // purge on first ntfn
  OCI_SUBSCR_QOS_MULTICBK          = $20;   // multi instance callback 

{ Temporary attribute value for UCS2/UTF16 character set ID }

  OCI_UCS2ID                  = 1000; // UCS2 charset ID
  OCI_UTF16ID                 = 1000; // UTF16 charset ID

{ Supported Namespaces }

  OCI_SUBSCR_NAMESPACE_ANONYMOUS   = 0;     // Anonymous Namespace
  OCI_SUBSCR_NAMESPACE_AQ          = 1;     // Advanced Queues
  OCI_SUBSCR_NAMESPACE_DBCHANGE    = 2;     // Change notification

{ Credential Types }

  OCI_CRED_RDBMS           = 1;     // database username/password
  OCI_CRED_EXT             = 2;     // externally provided credentials
  OCI_CRED_PROXY           = 3;     // proxy authentication
{ Error Return Values }

  OCI_SUCCESS              = 0;       // maps to SQL_SUCCESS of SAG CLI
  OCI_SUCCESS_WITH_INFO    = 1;       // maps to SQL_SUCCESS_WITH_INFO
  OCI_NO_DATA              = 100;     // maps to SQL_NO_DATA
  OCI_ERROR                = -1;      // maps to SQL_ERROR
  OCI_INVALID_HANDLE       = -2;      // maps to SQL_INVALID_HANDLE
  OCI_NEED_DATA            = 99;      // maps to SQL_NEED_DATA
  OCI_STILL_EXECUTING      = -3123;   // OCI would block error
  OCI_CONTINUE             = -24200;  // Continue with the body of the OCI function

{ Parsing Syntax Types }

  OCI_V7_SYNTAX            = 2;       // V7 language
  OCI_V8_SYNTAX            = 3;       // V8 language
  OCI_NTV_SYNTAX           = 1;       // Use what so ever is the native lang of server

{ Scrollable Cursor Options }

  OCI_FETCH_NEXT           = $02;
  OCI_FETCH_FIRST          = $04;
  OCI_FETCH_LAST           = $08;
  OCI_FETCH_PRIOR          = $10;
  OCI_FETCH_ABSOLUTE       = $20;
  OCI_FETCH_RELATIVE       = $40;

{ Bind and Define Options }

  OCI_SB2_IND_PTR          = $01;
  OCI_DATA_AT_EXEC         = $02;
  OCI_DYNAMIC_FETCH        = $02;
  OCI_PIECEWISE            = $04;

{ Statement States }

  OCI_STMT_STATE_INITIALIZED  = $01;
  OCI_STMT_STATE_EXECUTED     = $02;
  OCI_STMT_STATE_END_OF_FETCH = $03;

{ Execution Modes }

  OCI_BATCH_MODE           = $01;
  OCI_EXACT_FETCH          = $02;
  OCI_KEEP_FETCH_STATE     = $04;
  OCI_SCROLLABLE_CURSOR    = $08;
  OCI_DESCRIBE_ONLY        = $10;
  OCI_COMMIT_ON_SUCCESS    = $20;

{ Authentication Modes }
  OCI_MIGRATE              = $0001;   // migratable auth context
  OCI_SYSDBA               = $0002;   // for SYSDBA authorization
  OCI_SYSOPER              = $0004;   // for SYSOPER authorization
  OCI_PRELIM_AUTH          = $0008;   // for preliminary authorization
  OCI_STMT_CACHE           = $0040;   // enable OCI Stmt Caching
  OCI_SYSASM               = $8000;   // for SYSASM authorization

{ Piece Information }
  OCI_PARAM_IN             = $01;
  OCI_PARAM_OUT            = $02;

{ Transaction Start Flags }

  OCI_TRANS_OLD            = $00000000;
  OCI_TRANS_NEW            = $00000001;
  OCI_TRANS_JOIN           = $00000002;
  OCI_TRANS_RESUME         = $00000004;
  OCI_TRANS_STARTMASK      = $000000ff;
  OCI_TRANS_READONLY       = $00000100;
  OCI_TRANS_READWRITE      = $00000200;
  OCI_TRANS_SERIALIZABLE   = $00000400;
  OCI_TRANS_ISOLMASK       = $0000ff00;
  OCI_TRANS_LOOSE          = $00010000;
  OCI_TRANS_TIGHT          = $00020000;
  OCI_TRANS_TYPEMASK       = $000f0000;
  OCI_TRANS_NOMIGRATE      = $00100000;


{ Transaction End Flags }
  OCI_TRANS_TWOPHASE       = $01000000;

{ Visibility flags }
  OCI_ENQ_IMMEDIATE        = 1;
  OCI_ENQ_ON_COMMIT        = 2;

{ Dequeue mode flags }
  OCI_DEQ_BROWSE           = 1;
  OCI_DEQ_LOCKED           = 2;
  OCI_DEQ_REMOVE           = 3;

{ Dequeue navigation flags }
  OCI_DEQ_FIRST_MSG        = 1;
  OCI_DEQ_NEXT_MSG         = 3;
  OCI_DEQ_NEXT_TRANSACTION = 2;

{ Message states }
  OCI_MSG_WAITING          = 1;
  OCI_MSG_READY            = 0;
  OCI_MSG_PROCESSED        = 2;
  OCI_MSG_EXPIRED          = 3;

{ Sequence deviation }
  OCI_ENQ_BEFORE           = 2;
  OCI_ENQ_TOP              = 3;

{ Visibility flags }
  OCI_DEQ_IMMEDIATE        = 1;
  OCI_DEQ_ON_COMMIT        = 2;

{ Wait }
  OCI_DEQ_WAIT_FOREVER     = -1;
  OCI_DEQ_NO_WAIT          = 0;

{ Delay }
  OCI_MSG_NO_DELAY         = 0;

{ Expiration }
  OCI_MSG_NO_EXPIRATION    = -1;

  OCI_MSG_PERSISTENT_OR_BUFFERED = 3;
  OCI_MSG_BUFFERED               = 2;
  OCI_MSG_PERSISTENT             = 1;

{ END AQ Constants }

{ Object Types }

  OCI_OTYPE_UNK            = 0;
  OCI_OTYPE_TABLE          = 1;
  OCI_OTYPE_VIEW           = 2;
  OCI_OTYPE_SYN            = 3;
  OCI_OTYPE_PROC           = 4;
  OCI_OTYPE_FUNC           = 5;
  OCI_OTYPE_PKG            = 6;
  OCI_OTYPE_STMT           = 7;


 OCI_ATTR_CHAR_USED        = 285;  // char length semantics
 OCI_ATTR_CHAR_SIZE        = 286;  // char length

{ Describe Handle Parameter Attributes }
// Attributes common to Columns and Stored Procs
  OCI_ATTR_DATA_SIZE       = 1;
  OCI_ATTR_DATA_TYPE       = 2;
  OCI_ATTR_DISP_SIZE       = 3;
  OCI_ATTR_NAME            = 4;
  OCI_ATTR_PRECISION       = 5;
  OCI_ATTR_SCALE           = 6;
  OCI_ATTR_IS_NULL         = 7;
  OCI_ATTR_TYPE_NAME       = 8;
  OCI_ATTR_SCHEMA_NAME     = 9;
  OCI_ATTR_SUB_NAME        = 10;
  OCI_ATTR_POSITION        = 11;

{ complex object retrieval parameter attributes }
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE        = 50;
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL  = 51;
  OCI_ATTR_COMPLEXOBJECT_LEVEL           = 52;
  OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE  = 53;

{ Only Columns }
  OCI_ATTR_DISP_NAME       = 100;

{ Only Stored Procs }
  OCI_ATTR_OVERLOAD        = 210;
  OCI_ATTR_LEVEL           = 211;
  OCI_ATTR_HAS_DEFAULT     = 212;
  OCI_ATTR_IOMODE          = 213;
  OCI_ATTR_RADIX           = 214;
  OCI_ATTR_NUM_ARGS        = 215;

{ only user-defined Type's }
  OCI_ATTR_TYPECODE             = 216;
  OCI_ATTR_COLLECTION_TYPECODE  = 217;
  OCI_ATTR_VERSION              = 218;
  OCI_ATTR_IS_INCOMPLETE_TYPE   = 219;
  OCI_ATTR_IS_SYSTEM_TYPE       = 220;
  OCI_ATTR_IS_PREDEFINED_TYPE   = 221;
  OCI_ATTR_IS_TRANSIENT_TYPE    = 222;
  OCI_ATTR_IS_SYSTEM_GENERATED_TYPE = 223;
  OCI_ATTR_HAS_NESTED_TABLE     = 224;
  OCI_ATTR_HAS_LOB              = 225;
  OCI_ATTR_HAS_FILE             = 226;
  OCI_ATTR_COLLECTION_ELEMENT   = 227;
  OCI_ATTR_NUM_TYPE_ATTRS       = 228;
  OCI_ATTR_LIST_TYPE_ATTRS      = 229;
  OCI_ATTR_NUM_TYPE_METHODS     = 230;
  OCI_ATTR_LIST_TYPE_METHODS    = 231;
  OCI_ATTR_MAP_METHOD           = 232;
  OCI_ATTR_ORDER_METHOD         = 233;

{ for inheritance - part 2 }
  OCI_ATTR_IS_FINAL_TYPE        = 279;
  OCI_ATTR_IS_INSTANTIABLE_TYPE = 280;
  OCI_ATTR_IS_FINAL_METHOD      = 281;     //* is final method ? */
  OCI_ATTR_IS_INSTANTIABLE_METHOD = 282;   //* is instantiable method ? */
  OCI_ATTR_IS_OVERRIDING_METHOD = 283;     //* is overriding method ? */

{ only collection element }
  OCI_ATTR_NUM_ELEMENTS         = 234;

{ only type methods }
  OCI_ATTR_ENCAPSULATION   = 235;
  OCI_ATTR_IS_SELFISH      = 236;
  OCI_ATTR_IS_VIRTUAL      = 237;
  OCI_ATTR_IS_INLINE       = 238;
  OCI_ATTR_IS_CONSTANT     = 239;
  OCI_ATTR_HAS_RESULT      = 240;
  OCI_ATTR_IS_CONSTRUCTOR  = 241;
  OCI_ATTR_IS_DESTRUCTOR   = 242;
  OCI_ATTR_IS_OPERATOR     = 243;
  OCI_ATTR_IS_MAP          = 244;
  OCI_ATTR_IS_ORDER        = 245;
  OCI_ATTR_IS_RNDS         = 246;
  OCI_ATTR_IS_RNPS         = 247;
  OCI_ATTR_IS_WNDS         = 248;
  OCI_ATTR_IS_WNPS         = 249;
{ describing public objects }
  OCI_ATTR_DESC_PUBLIC     = 250;

{ ocicpw Modes }
  OCI_AUTH                 = $08;      // Change the password but donot login

{ Other Constants }
  OCI_MAX_FNS              = 100;      // max number of OCI Functions
  OCI_SQLSTATE_SIZE        = 5;
  OCI_ERROR_MAXMSG_SIZE    = 1024;
  //OCI_LOBMAXSIZE           = MINUB4MAXVAL;
  OCI_ROWID_LEN            = 23;

{ Fail Over Events }
  OCI_FO_END               = $00000001;
  OCI_FO_ABORT             = $00000002;
  OCI_FO_REAUTH            = $00000004;
  OCI_FO_BEGIN             = $00000008;
  OCI_FO_ERROR             = $00000010;

{ Fail Over Callback Return Codes }
  OCI_FO_RETRY              = 25410;


{ Function Codes }
{ Fail Over Types }
  OCI_FO_NONE              = $00000001;
  OCI_FO_SESSION           = $00000002;
  OCI_FO_SELECT            = $00000004;
  OCI_FO_TXNAL             = $00000008;

{ FILE open modes }
  OCI_FILE_READONLY        = 1;      // readonly mode open for FILE types

{ LOB Buffering Flush Flags }
  OCI_LOB_BUFFER_FREE      = 1;
  OCI_LOB_BUFFER_NOFREE    = 2;

{ LOB types }
  OCI_TEMP_BLOB            = 1;      // LOB type - BLOB
  OCI_TEMP_CLOB            = 2;      // LOB type - CLOB
  OCI_TEMP_NCLOB           = 3;      // LOB type - NCLOB ???

{ OCI Statement Types }

  OCI_STMT_SELECT          = 1;      // select statement
  OCI_STMT_UPDATE          = 2;      // update statement
  OCI_STMT_DELETE          = 3;      // delete statement
  OCI_STMT_INSERT          = 4;      // Insert Statement
  OCI_STMT_CREATE          = 5;      // create statement
  OCI_STMT_DROP            = 6;      // drop statement
  OCI_STMT_ALTER           = 7;      // alter statement
  OCI_STMT_BEGIN           = 8;      // begin ... (pl/sql statement)
  OCI_STMT_DECLARE         = 9;      // declare .. (pl/sql statement )
  OCI_STMT_EXPLAIN         = 10;     // explain plan

{ OCI Parameter Types }

  OCI_PTYPE_UNK            = 0;      // unknown
  OCI_PTYPE_TABLE          = 1;      // table
  OCI_PTYPE_VIEW           = 2;      // view
  OCI_PTYPE_PROC           = 3;      // procedure
  OCI_PTYPE_FUNC           = 4;      // function
  OCI_PTYPE_PKG            = 5;      // package
  OCI_PTYPE_TYPE           = 6;      // user-defined type
  OCI_PTYPE_SYN            = 7;      // synonym
  OCI_PTYPE_SEQ            = 8;      // sequence
  OCI_PTYPE_COL            = 9;      // column
  OCI_PTYPE_ARG            = 10;     // argument
  OCI_PTYPE_LIST           = 11;     // list
  OCI_PTYPE_TYPE_ATTR      = 12;     // user-defined type's attribute
  OCI_PTYPE_TYPE_COLL      = 13;     // collection type's element
  OCI_PTYPE_TYPE_METHOD    = 14;     // user-defined type's method
  OCI_PTYPE_TYPE_ARG       = 15;     // user-defined type method's argument
  OCI_PTYPE_TYPE_RESULT    = 16;     // user-defined type method's result

{ OCI List Types }

  OCI_LTYPE_UNK            = 0;      // unknown
  OCI_LTYPE_COLUMN         = 1;      // column list
  OCI_LTYPE_ARG_PROC       = 2;      // procedure argument list
  OCI_LTYPE_ARG_FUNC       = 3;      // function argument list
  OCI_LTYPE_SUBPRG         = 4;      // subprogram list
  OCI_LTYPE_TYPE_ATTR      = 5;      // type attribute
  OCI_LTYPE_TYPE_METHOD    = 6;      // type method
  OCI_LTYPE_TYPE_ARG_PROC  = 7;      // type method w/o result argument list
  OCI_LTYPE_TYPE_ARG_FUNC  = 8;      // type method w/result argument list

{ TYPE CODE }

  OCI_TYPECODE_REF         = SQLT_REF;    // SQL/OTS OBJECT REFERENCE
  OCI_TYPECODE_DATE        = SQLT_DAT;    // SQL DATE  OTS DATE
  OCI_TYPECODE_SIGNED8     = 27;          // SQL SIGNED INTEGER(8)  OTS SINT8
  OCI_TYPECODE_SIGNED16    = 28;          // SQL SIGNED INTEGER(16)  OTS SINT16
  OCI_TYPECODE_SIGNED32    = 29;          // SQL SIGNED INTEGER(32)  OTS SINT32
  OCI_TYPECODE_REAL        = 21;          // SQL REAL  OTS SQL_REAL
  OCI_TYPECODE_DOUBLE      = 22;          // SQL DOUBLE PRECISION  OTS SQL_DOUBLE
  OCI_TYPECODE_FLOAT       = SQLT_FLT;    // SQL FLOAT(P)  OTS FLOAT(P)
  OCI_TYPECODE_NUMBER      = SQLT_NUM;    // SQL NUMBER(P S)  OTS NUMBER(P S)
  OCI_TYPECODE_DECIMAL     = SQLT_PDN;    // SQL DECIMAL(P S)  OTS DECIMAL(P S)
  OCI_TYPECODE_UNSIGNED8   = SQLT_BIN;    // SQL UNSIGNED INTEGER(8)  OTS UINT8
  OCI_TYPECODE_UNSIGNED16  = 25;          // SQL UNSIGNED INTEGER(16)  OTS UINT16
  OCI_TYPECODE_UNSIGNED32  = 26;          // SQL UNSIGNED INTEGER(32)  OTS UINT32
  OCI_TYPECODE_OCTET       = 245;         // SQL ???  OTS OCTET
  OCI_TYPECODE_SMALLINT    = 246;         // SQL SMALLINT  OTS SMALLINT
  OCI_TYPECODE_INTEGER     = SQLT_INT;    // SQL INTEGER  OTS INTEGER
  OCI_TYPECODE_RAW         = SQLT_LVB;    // SQL RAW(N)  OTS RAW(N)
  OCI_TYPECODE_PTR         = 32;          // SQL POINTER  OTS POINTER
  OCI_TYPECODE_VARCHAR2    = SQLT_VCS;    // SQL VARCHAR2(N)  OTS SQL_VARCHAR2(N)
  OCI_TYPECODE_CHAR        = SQLT_AFC;    // SQL CHAR(N)  OTS SQL_CHAR(N)
  OCI_TYPECODE_VARCHAR     = SQLT_CHR;    // SQL VARCHAR(N)  OTS SQL_VARCHAR(N)
  OCI_TYPECODE_MLSLABEL    = SQLT_LAB;    // OTS MLSLABEL
  OCI_TYPECODE_VARRAY      = 247;         // SQL VARRAY  OTS PAGED VARRAY
  OCI_TYPECODE_TABLE       = 248;         // SQL TABLE  OTS MULTISET
  OCI_TYPECODE_OBJECT      = SQLT_NTY;    // SQL/OTS NAMED OBJECT TYPE
  OCI_TYPECODE_NAMEDCOLLECTION = SQLT_NCO;// SQL/OTS NAMED COLLECTION TYPE
  OCI_TYPECODE_BLOB        = SQLT_BLOB;   // SQL/OTS BINARY LARGE OBJECT
  OCI_TYPECODE_BFILE       = SQLT_BFILE;  // SQL/OTS BINARY FILE OBJECT
  OCI_TYPECODE_CLOB        = SQLT_CLOB;   // SQL/OTS CHARACTER LARGE OBJECT
  OCI_TYPECODE_CFILE       = SQLT_CFILE;  // SQL/OTS CHARACTER FILE OBJECT
  OCI_TYPECODE_OPAQUE      = 58;

  OCI_TYPECODE_OTMFIRST    = 228;         // first Open Type Manager typecode
  OCI_TYPECODE_OTMLAST     = 320;         // last OTM typecode
  OCI_TYPECODE_SYSFIRST    = 228;         // first OTM system type (internal)
  OCI_TYPECODE_SYSLAST     = 235;         // last OTM system type (internal)

  // the following are PL/SQL-only internal. They should not be used
  OCI_TYPECODE_ITABLE      = SQLT_TAB;    // PLSQL indexed table
  OCI_TYPECODE_RECORD      = SQLT_REC;    // PLSQL record
  OCI_TYPECODE_BOOLEAN     = SQLT_BOL;    // PLSQL boolean

{ OBJECT INDICATOR }

  OCI_IND_NOTNULL          = 0;      // not NULL
  OCI_IND_NULL             = -1;     // NULL
  OCI_IND_BADNULL          = -2;     // BAD NULL
  OCI_IND_NOTNULLABLE      = -3;     // not NULLable

{ OBJECT PIN OPTION }

  // 0 = uninitialized
  OCI_PIN_DEFAULT          = 1;      // default pin option
  OCI_PIN_ANY              = 3;      // pin any copy of the object
  OCI_PIN_RECENT           = 4;      // pin recent copy of the object
  OCI_PIN_LATEST           = 5;      // pin latest copy of the object

{ OBJECT LOCK OPTION }

  // 0 = uninitialized
  OCI_LOCK_NONE            = 1;      // null (same as no lock)
  OCI_LOCK_X               = 2;      // exclusive lock

{ OBJECT MODIFYING OPTION }

  // 0 = uninitialized
  OCI_MARK_DEFAULT         = 1;      // default (the same as OCI_MARK_NONE)
  OCI_MARK_NONE            = OCI_MARK_DEFAULT;   // object has not been modified
  OCI_MARK_UPDATE          = 2;      // object is to be updated

{ OCIDuration Types }
  OCI_DURATION_INVALID     = $FFFF;                  //Invalid duration

  OCI_DURATION_BEGIN         = 10;                      //beginning sequence of duration
  OCI_DURATION_SESSION       = OCI_DURATION_BEGIN;
  OCI_DURATION_TRANS         = OCI_DURATION_BEGIN + 1;
  OCI_DURATION_CALL          = OCI_DURATION_BEGIN + 2;
  OCI_DURATION_STATEMENT     = OCI_DURATION_BEGIN + 3;
  OCI_DURATION_CALLOUT       = OCI_DURATION_BEGIN + 4;
  OCI_DURATION_NULL          = OCI_DURATION_BEGIN - 1;
  OCI_DURATION_DEFAULT       = OCI_DURATION_BEGIN - 2;
  OCI_DURATION_USER_CALLBACK = OCI_DURATION_BEGIN - 3;
  OCI_DURATION_NEXT          = OCI_DURATION_BEGIN - 4;   //next special duration
  OCI_DURATION_PROCESS       = OCI_DURATION_BEGIN - 5;
  OCI_DURATION_LAST          = OCI_DURATION_CALLOUT;     //last of predefined durations
  
{ OBJECT FREE OPTION }

  OCI_OBJECTFREE_FORCE     = $0001;
  OCI_OBJECTFREE_NONULL    = $0002;

{ OBJECT LIFETIME }

  // 0 = uninitialized
  OCI_OBJECT_PERSISTENT    = 1;     // persistent object
  OCI_OBJECT_TRANSIENT     = 2;     // transient object
  OCI_OBJECT_VALUE         = 3;     // value object

{ OBJECT MARK STATUS }

  OCI_OBJECT_NEW           = $0001;   // new object
  OCI_OBJECT_DELETED       = $0002;   // object marked deleted
  OCI_OBJECT_UPDATED       = $0004;   // object marked updated

{ OCITypeGetOpt Types }

  OCI_TYPEGET_HEADER       = 0;
  OCI_TYPEGET_ALL          = 1;

{ NUMBER TYPE SIZE }

  OCI_NUMBER_SIZE          = 22;

{ OCINumberToInt }

  OCI_NUMBER_UNSIGNED      = 0;       // Unsigned type -- ubX
  OCI_NUMBER_SIGNED        = 2;       // Signed type -- sbX

{ OCIInterval }
  OCI_INTER_INVALID_DAY        = $1;
  OCI_INTER_DAY_BELOW_VALID    = $2;
  OCI_INTER_INVALID_MONTH      = $4;
  OCI_INTER_MONTH_BELOW_VALID  = $8;
  OCI_INTER_INVALID_YEAR       = $10;
  OCI_INTER_YEAR_BELOW_VALID   = $20;
  OCI_INTER_INVALID_HOUR       = $40;
  OCI_INTER_HOUR_BELOW_VALID   = $80;
  OCI_INTER_INVALID_MINUTE     = $100;
  OCI_INTER_MINUTE_BELOW_VALID = $200;
  OCI_INTER_INVALID_SECOND     = $400;
  OCI_INTER_SECOND_BELOW_VALID = $800;
  OCI_INTER_INVALID_FRACSEC    = $1000;
  OCI_INTER_FRACSEC_BELOW_VALID= $2000;

{ OCIDateTime }
  OCI_DT_INVALID_DAY           = $1;
  OCI_DT_DAY_BELOW_VALID       = $2;
  OCI_DT_INVALID_MONTH         = $4;
  OCI_DT_MONTH_BELOW_VALID     = $8;
  OCI_DT_INVALID_YEAR          = $10;
  OCI_DT_YEAR_BELOW_VALID      = $20;
  OCI_DT_INVALID_HOUR          = $40;
  OCI_DT_HOUR_BELOW_VALID      = $80;
  OCI_DT_INVALID_MINUTE        = $100;
  OCI_DT_MINUTE_BELOW_VALID    = $200;
  OCI_DT_INVALID_SECOND        = $400;
  OCI_DT_SECOND_BELOW_VALID    = $800;
  OCI_DT_DAY_MISSING_FROM_1582 = $1000;
  OCI_DT_YEAR_ZERO             = $2000;
  OCI_DT_INVALID_TIMEZONE      = $4000;
  OCI_DT_INVALID_FORMAT        = $8000;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}

{ Connection flags }

  ORAMTS_CFLG_ALLDEFAULT = $00;  // default flags
  ORAMTS_CFLG_NOIMPLICIT = $01;  // don't do implicit enlistment
  ORAMTS_CFLG_UNIQUESRVR = $02;  // need a separate Net8 connect
  ORAMTS_CFLG_SYSDBALOGN = $04;  // logon as a SYSDBA
  ORAMTS_CFLG_SYSOPRLOGN = $10;  // logon as a SYSOPER
  ORAMTS_CFLG_PRELIMAUTH = $20;  // preliminary internal login

  ORAMTS_ENFLG_DEFAULT = 0;      // default flags
  ORAMTS_ENFLG_RESUMTX = 1;      // resume a detached transact.
  ORAMTS_ENFLG_DETCHTX = 2;      // detached from the transact.


{ Error codes reported by the OraMTS<> functions }

  ORAMTSERR_NOERROR      = 0;          // success code

  ORAMTSERR_NOMTXDISPEN  = 1001;       // no MTXDM.DLL available  
  ORAMTSERR_DSPCREAFAIL  = 1002;       // failure to create dispen
  ORAMTSERR_DSPMAXSESSN  = 1003;       // exceeded max sessions   
  ORAMTSERR_DSPINVLSVCC  = 1004;       // invalid OCI Svc ctx     
  ORAMTSERR_DSPNODBIDEN  = 1005;       // can't create new dbiden 

  ORAMTSERR_NOSERVEROBJ  = 2001;       // unable to alloc a server

  ORAMTSERR_INVALIDSRVR  = 3001;       // invalid server object   
  ORAMTSERR_FAILEDATTCH  = 3002;       // failed attach to Oracle 
  ORAMTSERR_FAILEDDETCH  = 3003;       // failed detach from db   
  ORAMTSERR_FAILEDTRANS  = 3004;       // failed to start trans.  
  ORAMTSERR_SETATTRIBUT  = 3005;       // OCI set attrib failed   
  ORAMTSERR_CONNXBROKEN  = 3006;       // conn to Oracle broken   
  ORAMTSERR_NOTATTACHED  = 3007;       // not attached to Oracle  
  ORAMTSERR_ALDYATTACHD  = 3008;       // alrdy attached to Oracle

  ORAMTSERR_INVALIDSESS  = 4001;       // invalid session object  
  ORAMTSERR_FAILEDLOGON  = 4002;       // failed logon to Oracle  
  ORAMTSERR_FAILEDLOGOF  = 4003;       // failed logoff from db   
  ORAMTSERR_TRANSEXISTS  = 4004;       // no transaction beneath  
  ORAMTSERR_LOGONEXISTS  = 4005;       // already logged on to db 
  ORAMTSERR_NOTLOGGEDON  = 4006;       // not logged on to Oracle 

  ORAMTSERR_RPCINVLCTXT  = 5001;       // RPC context is invalid  
  ORAMTSERR_RPCCOMMUERR  = 5002;       // generic communic. error 
  ORAMTSERR_RPCALRDYCON  = 5003;       // endpoint already connect
  ORAMTSERR_RPCNOTCONNE  = 5004;       // endpoint not connected  
  ORAMTSERR_RPCPROTVIOL  = 5005;       // protocol violation      
  ORAMTSERR_RPCACCPTIMO  = 5006;       // timeout accepting conn. 
  ORAMTSERR_RPCILLEGOPC  = 5007;       // invalid RPC opcode      
  ORAMTSERR_RPCBADINCNO  = 5008;       // mismatched incarnation# 
  ORAMTSERR_RPCCONNTIMO  = 5009;       // client connect timeout  
  ORAMTSERR_RPCSENDTIMO  = 5010;       // synch. send timeout     
  ORAMTSERR_RPCRECVTIMO  = 5011;       // synch. receive timedout 
  ORAMTSERR_RPCCONRESET  = 5012;       // connection reset by peer

  ORAMTSERR_INVALIDARGU  = 6001;       // invalid args to function
  ORAMTSERR_INVALIDOBJE  = 6002;       // an object was invalid   
  ORAMTSERR_ILLEGALOPER  = 6003;       // illegal operation       
  ORAMTSERR_ALLOCMEMORY  = 6004;       // memory allocation error 
  ORAMTSERR_ERRORSYNCHR  = 6005;       // synchr. object error    
  ORAMTSERR_NOORAPROXY   = 6006;       // no Oracle Proxy server  
  ORAMTSERR_ALRDYENLIST  = 6007;       // session already enlisted
  ORAMTSERR_NOTENLISTED  = 6008;       // session is not enlisted 
  ORAMTSERR_TYPMANENLIS  = 6009;       // illeg on manuenlst sess 
  ORAMTSERR_TYPAUTENLIS  = 6010;       // illeg on autoenlst sess 
  ORAMTSERR_TRANSDETACH  = 6011;       // error detaching trans.  
  ORAMTSERR_OCIHNDLALLC  = 6012;       // OCI handle alloc error  
  ORAMTSERR_OCIHNDLRELS  = 6013;       // OCI handle dealloc error
  ORAMTSERR_TRANSEXPORT  = 6014;       // error exporting trans.  
  ORAMTSERR_OSCREDSFAIL  = 6105;       // error getting NT creds  
  ORAMTSERR_ISONOSUPPORT = 6108;       // txn iso-level not supported 
  ORAMTSERR_MIXEDTXNISO  = 6109;       // differ iso lvls for same txn
{$ENDIF}
{$ENDIF}

{ Handles and descriptors for direct path operations (OCIDirPath*) }
type
{$IFDEF CLR}
  pOCIDirPathCtx      = IntPtr;
  pOCIDirPathColArray = IntPtr;
  pOCIDirPathStream   = IntPtr;
  pOCIDirPathDesc     = IntPtr;
{$ELSE}
  OCIDirPathCtx      = record end;    // context
  OCIDirPathColArray = record end;    // column array
  OCIDirPathStream   = record end;    // stream
  OCIDirPathDesc     = record end;    // direct path descriptor

  pOCIDirPathCtx      = ^OCIDirPathCtx;
  pOCIDirPathColArray = ^OCIDirPathColArray;
  pOCIDirPathStream   = ^OCIDirPathStream;
  pOCIDirPathDesc     = ^OCIDirPathDesc;
{$ENDIF}

{ Defines for Direct Path Options }
const
  // values for OCI_ATTR_DIRPATH_MODE attribute
  OCI_DIRPATH_LOAD         = 1;       // direct path load operation
  OCI_DIRPATH_UNLOAD       = 2;       // direct path unload operation
  OCI_DIRPATH_CONVERT      = 3;       // direct path convert only operation

  // values for OCI_ATTR_STATE attribute of OCIDirPathCtx
  OCI_DIRPATH_NORMAL       = 1;       // can accept rows, last row complete
  OCI_DIRPATH_PARTIAL      = 2;       // last row was partial
  OCI_DIRPATH_NOT_PREPARED = 3;       // direct path context is not prepared

  // values for cflg argument to OCIDirpathColArrayEntrySet
  OCI_DIRPATH_COL_COMPLETE = 0;       // column data is complete
  OCI_DIRPATH_COL_NULL     = 1;       // column is null
  OCI_DIRPATH_COL_PARTIAL  = 2;       // column data is partial

type

  TRowId8 = packed record
    ridobjnum   : ub4; // data obj#--this field is unused in restricted ROWIDs
    ridfilenum  : ub2;
    filler      : ub2; // ub1; oracle bag
    ridblocknum : ub4;
    ridslotnum  : ub2;
  end;
{$IFDEF CLR}
  PRowId8 = packed record
  private
    Ptr: IntPtr;
    function GetUB2Property(Index: Integer): ub2;
    procedure SetUB2Property(Index: Integer; Value: ub2);
    function GetUB4Property(Index: Integer): ub4;
    procedure SetUB4Property(Index: Integer; Value: ub4);

  public
    property ridobjnum   : ub4 index 0 read GetUB4Property write SetUB4Property;
    property ridfilenum  : ub2 index 4 read GetUB2Property write SetUB2Property;
    property filler      : ub2 index 6 read GetUB2Property write SetUB2Property;
    property ridblocknum : ub4 index 8 read GetUB4Property write SetUB4Property;
    property ridslotnum  : ub2 index 12 read GetUB2Property write SetUB2Property;

    class operator Implicit(AValue: IntPtr): PRowId8;
    class operator Implicit(AValue: PRowId8): IntPtr;
  end;
{$ELSE}
  PRowid8 = ^TRowid8;
{$ENDIF}

const
  sizeof_TRowId8 = 14;

type
  TRowId81 = packed record
    filler      : ub1; // unknown
    ridobjnum   : ub4; // data obj#--this field is unused in restricted ROWIDs
    ridfilenum  : ub2;
    ridblocknum : ub4;
    ridslotnum  : ub2;
  end;
{$IFDEF CLR}
  PRowid81 = packed record
  private
    Ptr: IntPtr;
    function GetUB1Property(Index: Integer): ub1;
    procedure SetUB1Property(Index: Integer; Value: ub1);
    function GetUB2Property(Index: Integer): ub2;
    procedure SetUB2Property(Index: Integer; Value: ub2);
    function GetUB4Property(Index: Integer): ub4;
    procedure SetUB4Property(Index: Integer; Value: ub4);

  public
    property filler      : ub1 index 0 read GetUB1Property write SetUB1Property;
    property ridobjnum   : ub4 index 1 read GetUB4Property write SetUB4Property;
    property ridfilenum  : ub2 index 5 read GetUB2Property write SetUB2Property;
    property ridblocknum : ub4 index 7 read GetUB4Property write SetUB4Property;
    property ridslotnum  : ub2 index 11 read GetUB2Property write SetUB2Property;

    class operator Implicit(AValue: IntPtr): PRowid81;
    class operator Implicit(AValue: PRowid81): IntPtr;
  end;
{$ELSE}
  PRowid81 = ^TRowid81;
{$ENDIF}

const
  sizeof_TRowId81 = 13;

type
{ OCI descriptor types }
  OCIRowid = packed record
    Unknown : array [0..7] of byte;
    RowId   : TRowId8;
  end;
{$IFDEF CLR}
  POCIRowid = packed record
  private
    Ptr: IntPtr;
    function GetPRowId8Property(Index: Integer): PRowId8;

  public
    property RowId   : PRowId8 index 8 read GetPRowId8Property;

    class operator Implicit(AValue: IntPtr): POCIRowid;
    class operator Implicit(AValue: POCIRowid): IntPtr;
  end;
{$ELSE}
  POCIRowid = ^OCIRowid;
{$ENDIF}

const
  sizeof_OCIRowid = 8 + sizeof_TRowId8;

type
  OCIRowid81 = packed record  // for Oracle 8i
    Unknown : array [0..7] of byte;
    RowId   : PRowId81;
  end;
{$IFDEF CLR}
  POCIRowid81 = packed record  // for Oracle 8i
  private
    Ptr: IntPtr;
    function GetPRowId81Property(Index: Integer): PRowId81;
    procedure SetPRowId81Property(Index: Integer; Value: PRowId81);

  public
    property RowId : PRowId81 index 8 read GetPRowId81Property write SetPRowId81Property;

    class operator Implicit(AValue: IntPtr): pOCIRowid81;
    class operator Implicit(AValue: pOCIRowid81): IntPtr;
  end;
{$ELSE}
  POCIRowid81 = ^OCIRowid81;
{$ENDIF}
const
  sizeof_OCIRowid81 = 12;


type
{ OTS types }
  OCINumber = record
    OCINumberPart: array[0..OCI_NUMBER_SIZE -1] of byte;
  end;

  OCITime = packed record
    OCITimeHH: ub1;         // hours; range is 0 <= hours <=23
    OCITimeMI: ub1;         // minutes; range is 0 <= minutes <= 59
    OCITimeSS: ub1;         // seconds; range is 0 <= seconds <= 59
  end;

  OCIDate = packed record
    OCIDateYYYY: sb2;       // gregorian year; range is -4712 <= year <= 9999
    OCIDateMM: ub1;         // month; range is 1 <= month < 12
    OCIDateDD: ub1;         // day; range is 1 <= day <= 31
    OCIDateTime: OCITime;   // time
  end;

  TOCICallbackFailover = function (svchp: IntPtr; envhp: IntPtr; fo_ctx: IntPtr; fo_type: ub4; fo_event: ub4): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}

  TOCIFoCbkStruct = packed record
    callback_function: IntPtr;
    fo_ctx: IntPtr;
  end;

{ OCI handle types }
  pOCIBind     = IntPtr;
  pOCIDefine   = IntPtr;
  pOCIDescribe = IntPtr;
  pOCIEnv      = IntPtr;
  pOCIError    = IntPtr;
  pOCIServer   = IntPtr;
  pOCISession  = IntPtr;
  pOCIStmt     = IntPtr;
  pOCISvcCtx   = IntPtr;
  pOCITrans    = IntPtr;
  pOCIComplexObject = IntPtr;
  pOCISPool    = IntPtr;
  pOCIAuthInfo = IntPtr;

  pOCIParam       = IntPtr;
  pOCISnapshot    = IntPtr;
  pOCILobLocator  = IntPtr;
  pOCIDateTime    = IntPtr;
  pOCIInterval    = IntPtr;
  pOCIType        = IntPtr;
  ppOCIString     = IntPtr;

  pOCINumber   = IntPtr;
  pOCIDate     = IntPtr;
  pOCIString   = IntPtr;
  pOCIRaw      = IntPtr;
  pOCIRef      = IntPtr;
  pOCIColl     = IntPtr;
  pOCIArray    = IntPtr;
  pOCITable    = IntPtr;

  pOCIExtProcContext = IntPtr;

  pOCISubscription = IntPtr;

{$IFDEF CLR}

  pOCIHandle   = IntPtr;
  pOCIDescriptor  = IntPtr;

  ppOCIStmt    = IntPtr;
  ppOCIParam      = IntPtr;
  ppOCILobLocator = IntPtr;
  ppOCIDateTime   = IntPtr;
  ppOCIInterval   = IntPtr;
  ppOCIDescriptor = IntPtr;
  ppOCIRef     = IntPtr;
  ppOCIType    = IntPtr;

{$ELSE}

  ppOCIEnv     = ^pOCIEnv;
  ppOCIBind    = ^pOCIBind;
  ppOCIDefine  = ^pOCIDefine;
  ppOCIStmt    = ^pOCIStmt;
  ppOCISvcCtx  = ^pOCISvcCtx;

  pOCIHandle   = IntPtr;
  ppOCIHandle  = ^pOCIHandle;

{ OCI descriptor types }

  OCIParam      = integer;  // OCI_DTYPE_PARAM
  OCISnapshot   = integer;  // OCI_DTYPE_SNAP
  OCILobLocator = integer;  // OCI_DTYPE_LOB, OCI_DTYPE_FILE
  OCIDateTime   = integer;  // OCI_DTYPE_TIMESTAMP, OCI_DTYPE_TIMESTAMP_TZ,
                            // OCI_DTYPE_TIMESTAMP_LTZ
  OCIInterval   = integer;  // OCI_DTYPE_INTERVAL_YM, OCI_DTYPE_INTERVAL_DS

  pOCIDescriptor  = IntPtr;


  ppOCIDescriptor = ^pOCIDescriptor;

  ppOCIParam      = ^pOCIParam;
  ppOCILobLocator = ^pOCILobLocator;
  ppOCIDateTime   = ^pOCIDateTime;
  ppOCIInterval   = ^pOCIInterval;

{ OTS types }

  OCIType = integer;

  OCIString = packed record
  end;

  OCIRaw = packed record
  end;

  OCIRef = packed record
  end;

  OCIColl = packed record
  end;

  OCIArray = packed record
  end;

  OCITable = packed record
  end;

  OCIIter = packed record
  end;

  ppOCIType    = ^pOCIType;
  ppOCIRef     = ^pOCIRef;

{$ENDIF}

  tenum = longint;
  OCIDuration = ub2;
  OCIInd = sb2;
  OCILockOpt = tenum;
  OCIMarkOpt = tenum;
  OCIObjectEvent = tenum;
  OCIObjectProperty = tenum;
  OCIPinOpt = tenum;
  OCIRefreshOpt = tenum;
  OCITypeCode = ub2;
  OCITypeEncap = tenum;
  OCITypeGetOpt = tenum;
  OCITypeMethodFlag = tenum;
  OCITypeParamMode = tenum;
  OCIObjectPropId = ub1;
  OCIObjectLifetime = tenum;
  OCIObjectMarkstatus = uword;

{$IFDEF CLR}
  pOCIInd = IntPtr;
{$ELSE}
  pOCIInd = ^OCIInd;
{$ENDIF}

{$IFDEF CLR}
  [Serializable, AttributeUsage (AttributeTargets.Delegate)]
  CallConvCdeclAttribute = class (Attribute)
  end;
{$ENDIF}

  TXID = packed record
    FormatID: integer;
    Gtrid_length: integer; // value from 1 through 64
    Bqual_length: integer; // value from 1 through 64
    Data: array [0 .. 127] of byte;
  end;

const
  XID_SIZE = 140; // SizeOf(TXID) incorrect in CLR

type
  {$IFDEF CLR}[CallConvCdecl]{$ENDIF}
  TOCISubscriptionNotify = function(pCtx: IntPtr; pSubscrHp: pOCISubscription;
    pPayload: IntPtr; iPayloadLen: ub4; pDescriptor: IntPtr; iMode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIAttrGet1 = function(trgthndlp: IntPtr; trghndltyp: ub4; attributep: IntPtr;
    sizep: pub4; attrtype: ub4; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIAttrGet2 = function(trgthndlp: IntPtr; trghndltyp: ub4; var attributep: Integer;
    sizep: pub4; attrtype: ub4; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIAttrSet1 = function (trgthndlp: IntPtr; trghndltyp: ub4; attributep: IntPtr;
    size: ub4; attrtype: ub4; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  //_OCIAttrSet3 = function (trgthndlp: IntPtr; trghndltyp: ub4; attributep: PChar;
  //  size: ub4; attrtype: ub4; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIAttrSet2 = function (trgthndlp: IntPtr; trghndltyp: ub4; var attributep: Integer;
    size: ub4; attrtype: ub4; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIBindArrayOfStruct = function (bindp: pOCIBind; errhp: pOCIError; pvskip: ub4;
    indskip: ub4; alskip: ub4; rcskip: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIBindByName = function (stmtp: pOCIStmt; var bindpp: pOCIBind; errhp: pOCIError;
    placeholder: IntPtr; placeh_len: sb4; valuep: IntPtr; value_sz: sb4;
    dty: ub2; indp: IntPtr; alenp: pub2; rcodep: pub2; maxarr_len: ub4;
    curelep: pub4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIBindByPos = function (stmtp: pOCIStmt; var bindpp: pOCIBind; errhp: pOCIError;
    position: ub4; valuep: IntPtr; value_sz: sb4; dty: ub2;
    indp: IntPtr; alenp: pub2; rcodep: pub2; maxarr_len: ub4;
    curelep: pub4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  {$IFDEF CLR}[CallConvCdecl]{$ENDIF} //System.Runtime.CompilerServices.CallConvCdecl
  TOCICallbackInBind = function (ictxp: IntPtr; bindp: pOCIBind; iter: ub4; index: ub4;
    var bufpp: IntPtr; var alenp: ub4; var piecep: ub1; var indpp: IntPtr): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}

  {$IFDEF CLR}[CallConvCdecl]{$ENDIF}
  TOCICallbackOutBind = function (octxp: IntPtr; bindp: pOCIBind; iter: ub4; index: ub4;
    var bufpp: IntPtr; var alenpp: pub4; var piecep: ub1; var indpp: IntPtr;
    var rcodepp: pub2): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIBindDynamic = function (bindp: pOCIBind; errhp: pOCIError; ictxp: IntPtr;
    icbfp: IntPtr; octxp: IntPtr; ocbfp: IntPtr): sword;  {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIBindObject = function (bindp: pOCIBind; errhp: pOCIError; const otype: pOCIType;
    pgvpp: IntPtr; pvszsp: pub4; indpp: IntPtr; indszp: pub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIBreak = function (hndlp: pOCIHandle; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDefineArrayOfStruct = function (defnp: pOCIDefine; errhp: pOCIError;
    pvskip: ub4; indskip: ub4; rlskip: ub4; rcskip: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDefineByPos = function (stmtp: pOCIStmt; var defnpp: pOCIDefine; errhp: pOCIError;
    position: ub4; valuep: IntPtr; value_sz: sb4; dty: ub2;
    indp: IntPtr; rlenp: pub2; rcodep: pub2; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}


  {$IFDEF CLR}[CallConvCdecl]{$ENDIF}
  TOCICallbackDefine = function (octxp: IntPtr; defnp: pOCIDefine; iter: ub4; var bufpp: IntPtr;
    var alenpp: pub4; var piecep: ub1; var indpp: IntPtr; var rcodep: pub2): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDefineDynamic = function (defnp: pOCIDefine; errhp: pOCIError; octxp: IntPtr;
    ocbfp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDefineObject = function (defnp: pOCIDefine; errhp: pOCIError; const otype: pOCIType;
    pgvpp: IntPtr; pvszsp: pub4; indpp: IntPtr; indszp: pub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDescribeAny = function (svchp: pOCISvcCtx; errhp: pOCIError; objptr: IntPtr;
    objnm_len: ub4; objptr_typ: ub1; info_level: ub1; objtyp: ub1;  // WAR objtyp: ub1
    dschp: pOCIDescribe): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDescriptorAlloc = function (parenth: IntPtr; var descpp: pOCIDescriptor;
    dtype: ub4; xtramem_sz: size_t; usrmempp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDescriptorFree = function (descp: pOCIDescriptor; dtype: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIEnvInit = function (var envhpp: pOCIEnv; mode: ub4; xtramemsz: size_t; usrmempp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIErrorGet = function (hndlp: pOCIHandle; recordno: ub4; sqlstate: IntPtr;
    var errcodep: sb4; bufp: IntPtr; bufsiz: ub4; htype: ub4 ): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIHandleAlloc = function (parenth: IntPtr; var hndlpp: pOCIHandle; htype: ub4;
    xtramem_sz: size_t; usrmempp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIHandleFree = function (hndlp: pOCIHandle; htype: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIInitialize = function (mode: ub4; ctxp: IntPtr; malocfp: IntPtr;
    ralocfp: IntPtr; mfreefp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILdaToSvcCtx = function (var svchpp: pOCISvcCtx; errhp: pOCIError; ldap: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIParamGet = function (hndlp: pOCIHandle; htype: ub4; errhp: pOCIError;
    var parmdpp: pOCIParam; pos: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIPasswordChange = function (svchp: pOCISvcCtx; errhp: pOCIError; const user_name: IntPtr;
    usernm_len: ub4; const opasswd: IntPtr; opasswd_len: ub4;
    const npasswd: IntPtr; npasswd_len: sb4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIReset = function (hndlp: pOCIHandle; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIServerAttach = function (srvhp: pOCIServer; errhp: pOCIError; dblink: IntPtr;
    dblink_len: sb4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIServerDetach = function (srvhp: pOCIServer; errhp: pOCIError; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIServerVersion = function (hndlp: IntPtr; errhp: pOCIError; bufp: IntPtr; bufsz: ub4; hndltype: ub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISessionBegin = function (svchp: pOCISvcCtx; errhp: pOCIError; usrhp: pOCISession;
    credt: ub4; mode: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISessionEnd = function (svchp: pOCISvcCtx; errhp: pOCIError; usrhp: pOCISession;
    mode: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISessionGet = function(envhp: pOCIEnv; errhp: pOCIError; var svchp: pOCISvcCtx;
    authhp: pOCIAuthInfo; poolName: IntPtr; poolName_len: ub4;
    tagInfo: IntPtr; tagInfo_len: ub4; var retTagInfo: IntPtr;
    var retTagInfo_len: ub4; var found: longbool; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISessionRelease = function(svchp: pOCISvcCtx; errhp: pOCIError; tag: IntPtr;
    tag_len: ub4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISessionPoolCreate = function (envhp: pOCIEnv; errhp: pOCIError; spoolhp: pOCISpool;
    var poolName: IntPtr; var poolNameLen: ub4; connStr: IntPtr; connStrLen: ub4;
    sessMin: ub4; sessMax: ub4; sessIncr: ub4; userid: IntPtr; useridLen: ub4;
    password: IntPtr; passwordLen: ub4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISessionPoolDestroy = function(spoolhp: pOCISPool; errhp: pOCIError;
    mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITransStart = function (svchp: pOCISvcCtx; errhp: pOCIError; timeout: word;
    flags: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITransRollback = function (svchp:pOCISvcCtx; errhp:pOCIError; flags: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITransCommit = function (svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITransDetach = function (svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITransPrepare = function (svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITransForget = function (svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtExecute = function (svchp: pOCISvcCtx; stmtp: pOCIStmt; errhp: pOCIError;
    iters: ub4; rowoff: ub4; snap_in: pOCISnapshot; snap_out: pOCISnapshot;
    mode: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtFetch = function (stmtp: pOCIStmt; errhp: pOCIError; nrows: ub4; orientation: ub2;
    mode: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtGetPieceInfo = function (stmtp: pOCIStmt; errhp: pOCIError; var hndlpp: pOCIHandle;
    htypep: pub4; in_outp: pub1; iterp: pub4; idxp: pub4;
    piecep: pub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtPrepare = function (stmtp: pOCIStmt; errhp: pOCIError; stmt: IntPtr;
    stmt_len: ub4; language: ub4; mode: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtPrepare2 = function(svchp: pOCISvcCtx; var stmtp: pOCIStmt; errhp: pOCIError;
    stmt: IntPtr; stmt_len: ub4; key: IntPtr; key_len: ub4; language: ub4;
    mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtRelease = function(stmtp: pOCIStmt; errhp: pOCIError; key: IntPtr; key_len: ub4;
    mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtSetPieceInfo = function (hndlp: pOCIHandle; htype: ub4; errhp: pOCIError;
    const bufp: IntPtr; alenp: pub4; piece: ub1;
    const indp: IntPtr; rcodep: pub2): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISvcCtxToLda = function (srvhp: pOCISvcCtx; errhp: pOCIError; ldap: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

{ LOB supports }

  _OCILobAppend = function (svchp: pOCISvcCtx; errhp: pOCIError; dst_locp: pOCILobLocator;
    src_locp: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobAssign = function (envhp: pOCIEnv; errhp: pOCIError;
    const src_locp: pOCILobLocator; var dst_locpp: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}


  _OCILobCharSetForm = function (envhp: pOCIEnv; errhp: pOCIError;
    const locp: pOCILobLocator; var csfrm: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobCharSetId = function (envhp: pOCIEnv; errhp: pOCIError;
    const locp: pOCILobLocator; csid: pub2): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobCopy = function (svchp: pOCISvcCtx; errhp: pOCIError; dst_locp: pOCILobLocator;
    src_locp: pOCILobLocator; amount: ub4; dst_offset: ub4;
    src_offset: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobOpen = function (svchp: pOCISvcCtx; errhp: pOCIError;
    locp: pOCILobLocator; mode: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobClose = function (svchp: pOCISvcCtx; errhp: pOCIError;
    locp: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobIsOpen = function (svchp: pOCISvcCtx; errhp: pOCIError;
    locp: pOCILobLocator; var flag: LongBool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobCreateTemporary = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    csid: ub2; csfrm: ub1; lobtype: ub1; cache: tbool; duration: OCIDuration): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFreeTemporary = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobIsTemporary = function (envhp: pOCIEnv; errhp: pOCIError; locp: pOCILobLocator;
    var is_temporary: LongBool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobDisableBuffering = function (svchp: pOCISvcCtx; errhp: pOCIError;
    locp: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobEnableBuffering = function (svchp: pOCISvcCtx; errhp: pOCIError;
    locp: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobErase = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    amount: pub4; offset: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFileClose = function (svchp: pOCISvcCtx; errhp: pOCIError; filep: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFileExists = function (svchp: pOCISvcCtx; errhp: pOCIError; filep: pOCILobLocator;
    var flag: tbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFileGetName = function (envhp: pOCIEnv; errhp: pOCIError; const filep: pOCILobLocator;
    dir_alias: IntPtr; d_length: pub2; filename: IntPtr; f_length: pub2): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFileIsOpen = function (svchp: pOCISvcCtx; errhp: pOCIError; filep: pOCILobLocator;
    var flag: tbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFileOpen = function (svchp: pOCISvcCtx; errhp: pOCIError; filep: pOCILobLocator;
    mode: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFileSetName = function (envhp: pOCIEnv; errhp: pOCIError; filepp: ppOCILobLocator;
    const dir_alias: IntPtr; d_length: ub2; const filename: IntPtr;
    f_length: ub2): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFlushBuffer = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    flag: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobGetLength = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    var lenp: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobIsEqual = function (envhp: pOCIEnv; const x: pOCILobLocator; const y: pOCILobLocator;
    is_equal: pbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobLoadFromFile = function (svchp: pOCISvcCtx; errhp: pOCIError;
    dst_locp: pOCILobLocator; src_locp: pOCILobLocator;
    amount: ub4; dst_offset: ub4; src_offset: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobLocatorIsInit = function (envhp: pOCIEnv; errhp: pOCIError; const locp: pOCILobLocator;
    var is_initialized: tbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobRead = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    var amtp: ub4; offset: ub4; bufp: IntPtr; bufl: ub4; ctxp: IntPtr;
    cbfp: IntPtr; csid: ub2; csfrm: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobRead2 = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    var byte_amtp: ub8; var char_amtp: ub8; offset: ub8; bufp: IntPtr; bufl: ub8;
    piece: ub1; ctxp: IntPtr; cbfp: IntPtr; csid: ub2; csfrm: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobTrim = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    newlen: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobWrite = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    var amtp: ub4; offset: ub4; bufp: IntPtr; bufl: ub4; piece: ub1;
    ctxp: IntPtr; cbfp: IntPtr; csid: ub2; csfrm: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

{ Objects supports }

  TGetFlushRef = function (context: IntPtr; last: pub1): pOCIRef; cdecl;

  _OCICacheFlush = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
    context: IntPtr; get: TGetFlushRef; var ref: pOCIRef): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICacheFree = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  TGetRefreshRef = function (context: IntPtr): pOCIRef; cdecl;

  _OCICacheRefresh = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
    option: OCIRefreshOpt; context: IntPtr; get: TGetRefreshRef;
    var ref: pOCIRef): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICacheUnmark = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICacheUnpin = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx): sword;{$IFNDEF CLR} cdecl; {$ENDIF}


  _OCIObjectCopy = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
    source: IntPtr; null_source: IntPtr; target: IntPtr;
    null_target: IntPtr; tdo: pOCIType; duration: OCIDuration;
    option: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectExists = function (env: pOCIEnv; err: pOCIError; ins: IntPtr; var exist: tbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectFlush = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectFree = function (env: pOCIEnv; err: pOCIError; instance: IntPtr; flags: ub2): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectGetAttr = function (env: pOCIEnv; err: pOCIError; instance: IntPtr;
    null_struct: IntPtr; tdo: pOCIType; namesp: IntPtr;
    const lengths: pub4; const name_count: ub4; const indexes: pub4;
    const index_count: ub4; attr_null_status: pOCIInd;
    attr_null_structp: IntPtr; attr_valuep: IntPtr; attr_tdop: ppOCIType): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectGetInd = function (env: pOCIEnv; err: pOCIError; instance: IntPtr; null_structp: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectGetObjectRef = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr;
    object_ref: pOCIRef): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectGetProperty = function (env: pOCIEnv; err: pOCIError; const obj: IntPtr;
    propertyId: OCIObjectPropId; prop: OCIObjectPropId; size: pub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectGetTypeRef = function (env: pOCIEnv; err: pOCIError; instance: IntPtr;
    type_ref: pOCIRef): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectIsDirty = function (env: pOCIEnv; err: pOCIError; ins: IntPtr; var dirty: tbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectIsLocked = function (env: pOCIEnv; err: pOCIError; ins: IntPtr; var lock: tbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectLock = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectMarkDelete = function (env: pOCIEnv; err: pOCIError; instance: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectMarkDeleteByRef = function (env: pOCIEnv; err: pOCIError; object_ref: pOCIRef): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectMarkUpdate = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectNew = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
    typecode: OCITypeCode; tdo: pOCIType; table: IntPtr;
    duration: OCIDuration; value: tbool; var instance: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectPin = function (env: pOCIEnv; err: pOCIError; object_ref: pOCIRef;
    corhdl: pOCIComplexObject; pin_option: OCIPinOpt; pin_duration: OCIDuration;
    lock_option: OCILockOpt; pobjectp: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectPinCountReset = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectPinTable = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
    const schema_name: IntPtr; s_n_length: ub4; const object_name: IntPtr;
    o_n_length: ub4; not_used: IntPtr; pin_duration: OCIDuration;
    var pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectRefresh = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectSetAttr = function (env: pOCIEnv; err: pOCIError; instance: IntPtr;
    null_struct: IntPtr; tdo: pOCIType; namesp: IntPtr;
    const lengths: pub4; const name_count: ub4; const indexes: pub4;
    const index_count: ub4; const null_status: OCIInd;
    const attr_null_struct: IntPtr; const attr_value: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectUnmark = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectUnmarkByRef = function (env: pOCIEnv; err: pOCIError; ref: pOCIRef): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectUnpin = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITypeByName = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
    const schema_name: IntPtr; s_length: ub4; const type_name: IntPtr;
    t_length: ub4; version_name: IntPtr; v_length: ub4;
    pin_duration: OCIDuration; get_option: OCITypeGetOpt; var tdo: pOCIType): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITypeByRef = function (env: pOCIEnv; err: pOCIError; const type_ref: pOCIRef;
    pin_duration: OCIDuration; get_option: OCITypeGetOpt; tdo: pOCIType): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

{ OTS types }

  _OCICollAppend = function (env: pOCIEnv; err: pOCIError; const elem: IntPtr;
    const elemind: IntPtr; coll: pOCIColl): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICollAssign = function (env: pOCIEnv; err: pOCIError; const rhs: pOCIColl;
    lhs: pOCIColl): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICollAssignElem = function (env: pOCIEnv; err: pOCIError; index: sb4;
    const elem: IntPtr; const elemind: IntPtr; coll: pOCIColl): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICollGetElem = function (env: pOCIEnv; err: pOCIError; const coll: pOCIColl;
    index: sb4; var exists: tbool; var elem: IntPtr; var elemind: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICollMax = function (env: pOCIEnv; const coll: pOCIColl): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICollSize = function (env: pOCIEnv; err: pOCIError; const coll: pOCIColl; var size: sb4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICollTrim = function (env: pOCIEnv; err: pOCIError; trim_num: sb4; coll: pOCIColl): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateAssign = function (err: pOCIError; const from: pOCIDate; todate: pOCIDate): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateFromText = function (err: pOCIError; date_str: IntPtr; d_str_length: ub4;
    const fmt: IntPtr; fmt_length: ub1; const lang_name: IntPtr;
    lang_length: ub4; date: pOCIDate): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateGetDate = function (const date: pOCIDate; year: psb2; month: pub1; day: pub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateGetTime = function (const date: pOCIDate; hour: pub1; min: pub1; sec: pub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateSetDate = function (date: pOCIDate; year: sb2; month: ub1; day: ub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateSetTime = function (date: pOCIDate; hour: ub1; min: ub1; sec: ub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateToText = function (err: pOCIError; const date: pOCIDate; const fmt: IntPtr;
    fmt_length: ub1; const lang_name: IntPtr; lang_length: ub4;
    buf_size: pub4; buf: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberAssign = function (err: pOCIError; const from: pOCINumber; tonum: pOCINumber): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberCmp = function ( err: pOCIError; const number1: pOCINumber;
    const number2: pOCINumber; var result: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberFromInt = function (err: pOCIError; var inum: int64;
    inum_length: uword; inum_s_flag: uword; number: pOCINumber): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberFromReal = function (err: pOCIError; var rnum: double; rnum_length: uword;
    number: pOCINumber): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberFromText = function (err: pOCIError; const str: IntPtr; str_length: ub4;
    const fmt: IntPtr; fmt_length: ub4; const nls_params: IntPtr;
    nls_p_length: ub4; number: pOCINumber): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberToInt = function (err: pOCIError; number: pOCINumber; rsl_length: uword;
    rsl_flag: uword; var rsl: int64): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberToReal = function (err: pOCIError; const number: pOCINumber; rsl_length: uword;
    var rsl: double): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberToText = function (err: pOCIError; number: pOCINumber;
    const fmt: IntPtr; fmt_length: ub4; const nls_params: IntPtr;
    nls_p_length: ub4; var buf_size: ub4; buf: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIRefAssign = function (env: pOCIEnv; err: pOCIError; const source: pOCIRef;
    var target: pOCIRef): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIRefClear = function (env: pOCIEnv; ref: pOCIRef): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIRefIsEqual = function (env: pOCIEnv; const x: pOCIRef; const y: pOCIRef): tbool; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIRefIsNull = function (env: pOCIEnv; const ref: pOCIRef): tbool; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIRefToHex = function (env: pOCIEnv; err: pOCIError; const ref: pOCIRef;
    hex: IntPtr; var hex_length: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStringAllocSize = function (env: pOCIEnv; err: pOCIError; const vs: pOCIString; allocsize: pub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStringAssign = function (env: pOCIEnv; err: pOCIError; const rhs: pOCIString;
    lhs: ppOCIString): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStringAssignText = function (env: pOCIEnv; err: pOCIError; const rhs: IntPtr;
    rhs_len: ub4; lhs: ppOCIString): sword; {$IFNDEF CLR} cdecl; {$ENDIF}// Oracle documentation bag rhs_len: ub2;

  _OCIStringPtr = function (env: pOCIEnv; const vs: pOCIString): IntPtr; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStringResize = function (env: pOCIEnv; err: pOCIError; new_size: ub4; str: ppOCIString): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStringSize = function (env: pOCIEnv; const vs: pOCIString): ub4; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITableDelete = function (env: pOCIEnv; err: pOCIError; index: sb4; tbl: pOCITable): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITableExists = function (env: pOCIEnv; err: pOCIError; const tbl: pOCITable;
    index: sb4; exists: pbool): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITableFirst = function (env: pOCIEnv; err: pOCIError; const tbl: pOCITable;
    index: psb4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITableLast = function (env: pOCIEnv; err: pOCIError; const tbl: pOCITable;
    index: psb4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITableNext = function (env: pOCIEnv; err: pOCIError; index: sb4; const tbl: pOCITable;
    var next_index: sb4; var exists: tbool): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITablePrev = function (env: pOCIEnv; err: pOCIError; index: sb4; const tbl: pOCITable;
    prev_index: psb4; exists: pbool): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITableSize = function (env: pOCIEnv; err: pOCIError; const tbl: pOCITable; var size: sb4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ OCI81 }

  _OCIEnvCreate = function (var envhpp: pOCIEnv; mode: ub4; const ctxp: IntPtr;
    const malocfp: IntPtr; const ralocfp: IntPtr;
    const mfreefp: IntPtr; xtramemsz: size_t; usrmempp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ Direct path load interface support }

  _OCIDirPathAbort = function (dpctx: pOCIDirPathCtx; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathColArrayEntryGet = function (dpca: pOCIDirPathColArray; errhp: pOCIError;
    rownum: ub4; colIdx: ub2; var cvalpp: pub1; clenp: ub4;
    cflgp: pub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathColArrayEntrySet = function (dpca: pOCIDirPathColArray; errhp: pOCIError;
    rownum: ub4; colIdx: ub2; cvalp: pub1; clen: ub4;
    cflg: ub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathColArrayRowGet = function (dpca: pOCIDirPathColArray; errhp: pOCIError;
    rownum: ub4; var cvalppp: pub1; var clenpp: pub4;
    cflgpp: ppub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathColArrayReset = function (dpca: pOCIDirPathColArray; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathColArrayToStream = function (dpca: pOCIDirPathColArray; const dpctx: pOCIDirPathCtx;
    dpstr: pOCIDirPathStream; errhp: pOCIError; rowcnt: ub4;
    rowoff: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathFinish = function (dpctx: pOCIDirPathCtx; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathLoadStream = function (dpctx: pOCIDirPathCtx; dpstr: pOCIDirPathStream;
    errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathPrepare = function (dpctx: pOCIDirPathCtx; svchp: pOCISvcCtx;
    errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathStreamReset = function (dpstr: pOCIDirPathStream; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ OCI9 }

{ Timestamp and interval types support }

  _OCIDateTimeConstruct = function (hndl: IntPtr; err: pOCIError;
    datetime: pOCIDateTime; year: sb2; month, day, hour, min, sec: ub1; fsec: ub4;
    timezone: IntPtr; timezone_length: integer): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeCheck = function (hndl: IntPtr; err: pOCIError; date: pOCIDateTime;
		var valid: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeFromText = function (hndl: IntPtr; err: pOCIError;
    date_str: IntPtr; d_str_length: integer; fmt: IntPtr; fmt_length: ub1; lang_name: IntPtr;
    lang_length: integer; date: pOCIDateTime): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeToText = function (hndl: IntPtr; err: pOCIError; date: pOCIDateTime;
    fmt: IntPtr; fmt_length, fsprec: ub1; lang_name: IntPtr; lang_length: integer;
    var buf_size: ub4; buf: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeGetDate = function (hndl: IntPtr; err: pOCIError;
    date: pOCIDateTime; var year: sb2; var month: ub1; var day: ub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeGetTime = function (hndl: IntPtr; err: pOCIError;
    datetime: pOCIDateTime; var hour: ub1; var minute: ub1; var sec: ub1;
    var fsec: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeGetTimeZoneOffset = function (hndl: IntPtr; err: pOCIError;
    datetime: pOCIDateTime; var hour: sb1; var minute: sb1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeGetTimeZoneName = function (hndl: IntPtr; err: pOCIError;
    datetime: pOCIDateTime; buf: pub1; var buflen: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeAssign = function (hndl: IntPtr; err: pOCIError; src: pOCIDateTime;
    dst: pOCIDateTime): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeCompare = function (hndl: IntPtr; err: pOCIError; const date1: pOCIDateTime;
    const date2: pOCIDateTime; var result: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalFromText = function (hndl: IntPtr; err: pOCIError; inpstr: IntPtr;
		str_len: integer; result: pOCIInterval): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalToText = function (hndl: IntPtr; err: pOCIError; inter: pOCIInterval;
    lfprec, fsprec: ub1; buffer: IntPtr; buflen: size_t;
    var resultlen: size_t): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalCheck = function (hndl: IntPtr; err: pOCIError; interval: pOCIInterval;
		var valid: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalAssign = function (hndl: IntPtr; err: pOCIError; ininter: pOCIInterval;
		outinter: pOCIInterval): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalCompare = function (hndl: IntPtr; err: pOCIError; inter1: pOCIInterval;
    inter2: pOCIInterval; var result: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalSetYearMonth = function (hndl: IntPtr; err: pOCIError; yr, mnth: sb4;
    result: pOCIInterval): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalGetYearMonth = function (hndl: IntPtr; err: pOCIError; var yr: sb4; var mnth: sb4;
    result: pOCIInterval): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalSetDaySecond = function (hndl: IntPtr; err: pOCIError; dy, hr,
    mm, ss, fsec: sb4; result: pOCIInterval): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalGetDaySecond = function (hndl: IntPtr; err: pOCIError; var dy: sb4; var hr: sb4;
    var mm: sb4; var ss: sb4; var fsec: sb4; result: pOCIInterval): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalFromNumber = function (hndl: IntPtr; err: pOCIError; interval: pOCIInterval;
    number: pOCINumber): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ Scrollable cursors }

  _OCIStmtFetch2 = function (stmtp: pOCIStmt; errhp: pOCIError; nrows: ub4; orientation: ub2;
    scrollOffset: sb4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ OCI10 }
  _OCIClientVersion = function (var major_version, minor_version, update_num, patch_num, port_update_num: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIPing = function (svchp: pOCISvcCtx; errhp: pOCIError; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ XMLType supports }

  pOCIXMLType = IntPtr;
  pOCIDOMDocument = IntPtr;
  ppOCIXMLType = IntPtr;
  ppOCIDOMDocument = IntPtr;
{$IFNDEF CLR}
  OCIXMLType = record end;
  OCIDOMDocument = record end;
{$ENDIF}


  _OCIXMLTypeNew = function (svchp: pOCISvcCtx; errhp: pOCIError; dur: OCIDuration;
    elname: PAnsiChar; elname_Len: ub4; schemaURL: PAnsiChar;
    schemaURL_Len: ub4; var retInstance: pOCIXMLType): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeCreateFromSrc = function (svchp: pOCISvcCtx; errhp: pOCIError; dur: OCIDuration;
    src_type: ub1; src_ptr: IntPtr; ind: sb4; var retInstance: pOCIXMLType): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeExtract = function (errhp: pOCIError; doc: pOCIXMLType; dur: OCIDuration;
    xpathexpr: PAnsiChar; xpathexpr_Len: ub4; nsmap: PAnsiChar; nsmap_Len: ub4;
    var retDoc: pOCIXMLType): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeTransform = function (errhp: pOCIError; dur: OCIDuration;
    doc: pOCIXMLType; xsldoc: pOCIXMLType;
    var retDoc: pOCIXMLType): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeExists = function (errhp: pOCIError; doc: pOCIXMLType;
    xpathexpr: PAnsiChar; xpathexpr_Len: ub4; nsmap: PAnsiChar; nsmap_Len: ub4;
    var retval: LongWord): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeIsSchemaBased = function (errhp: pOCIError;
    doc: pOCIXMLType; var retval: LongWord): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeGetSchema = function (errhp: pOCIError; doc: pOCIXMLType;
    var schemadoc: pOCIXMLType; var schemaURL: IntPtr; var schemaURL_Len: ub4;
    var rootelem: IntPtr; var rootelem_Len: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeValidate = function (errhp: pOCIError; doc: pOCIXMLType;
    schemaURL: PAnsiChar; schemaURL_Len: ub4; var retval: LongWord): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeGetDOM = function (errhp: pOCIError; doc: pOCIXMLType; dur: OCIDuration;
    var retDom: pOCIDOMDocument): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeGetFromDOM = function (errhp: pOCIError; domdoc: pOCIDOMDocument;
    var retXMLType: pOCIXMLType): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDOMFree = function (errhp: pOCIError; domdoc: pOCIDOMDocument): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIPStreamFromXMLType = function(errhp: pOCIError; phOCIDescriptor: IntPtr; pobject: IntPtr; res: integer): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIPStreamRead = function(errhp: pOCIError; phOCIDescriptor: IntPtr; pStr: IntPtr; var Len: int64; res: integer): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIPStreamClose = function(errhp: pOCIError; phOCIDescriptor: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ Misc }

  _OCIRowidToChar = function (rowidDesc: IntPtr; outbfp: IntPtr;
    var outbflp: ub2; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ Oracle External procedures support }

  _OCIExtProcGetEnv = function(with_context: pOCIExtProcContext;
    var envh: pOCIEnv; var svch: pOCISvcCtx; var errh: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIExtProcAllocCallMemory = function(with_context: pOCIExtProcContext;
    amount: cardinal): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIExtProcRaiseExcpWithMsg = function(with_context: pOCIExtProcContext;
    errnum: Integer; errmsg: PAnsiChar; msglen: Integer): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ Publish - subscribe support }

  _OCISubscriptionRegister = function(svchp: pOCISvcCtx; var subscrhpp: pOCISubscription;
    count: ub2; errhp: pOCIError; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISubscriptionUnRegister = function(svchp: pOCISvcCtx; subscrhp: pOCISubscription;
    errhp: pOCIError; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISubscriptionEnable = function(subscrhp: pOCISubscription;
    errhp: pOCIError; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISubscriptionDisable = function(subscrhp: pOCISubscription;
    errhp: pOCIError; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  { MTS support }
  _OraMTSSvcGet = function(lpUName: PAnsiChar; lpPsswd: PAnsiChar; lpDbnam: PAnsiChar; var pOCISvc: pOCISvcCtx;
                   var pOCIEnv: pOCIEnv; ConFlg: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OraMTSSvcRel = function(OCISvc: pOCISvcCtx): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OraMTSJoinTxn = function(svchp: pOCISvcCtx; lpTrans: ICRTransactionSC): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OraMTSEnlCtxGet = function(lpUName: PAnsiChar; lpPsswd: PAnsiChar; lpDbnam: PAnsiChar;
                               pOCISvc: pOCISvcCtx; errhp: pOCIError; dwFlags: ub4;
                               var pCtxt: pOCISvcCtx): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OraMTSEnlCtxRel = function(pCtxt: pOCISvcCtx): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OraMTSSvcEnlist = function(OCISvc: pOCISvcCtx; OCIErr: pOCIError; lpTrans: ICRTransactionSC;
                       dwFlags: LongInt): sword; {$IFNDEF CLR} cdecl; {$ENDIF}
{$ENDIF}
{$ENDIF}

var
  OCIAttrGet1            : _OCIAttrGet1;
  OCIAttrGet2            : _OCIAttrGet2;
  OCIAttrSet1            : _OCIAttrSet1;
  //OCIAttrSet3            : _OCIAttrSet3;
  OCIAttrSet2            : _OCIAttrSet2;
  OCIBindArrayOfStruct   : _OCIBindArrayOfStruct;
  OCIBindByName          : _OCIBindByName;
  OCIBindByPos           : _OCIBindByPos;
  OCIBindDynamic         : _OCIBindDynamic;
  OCIBindObject          : _OCIBindObject;
  OCIBreak               : _OCIBreak;
  OCIDefineArrayOfStruct : _OCIDefineArrayOfStruct;
  OCIDefineByPos         : _OCIDefineByPos;
  OCIDefineDynamic       : _OCIDefineDynamic;
  OCIDefineObject        : _OCIDefineObject;
  OCIDescribeAny         : _OCIDescribeAny;
  OCIDescriptorAlloc     : _OCIDescriptorAlloc;
  OCIDescriptorFree      : _OCIDescriptorFree;
  OCIEnvInit             : _OCIEnvInit;
  OCIErrorGet            : _OCIErrorGet;
  OCIHandleAlloc         : _OCIHandleAlloc;
  OCIHandleFree          : _OCIHandleFree;
  OCIInitialize          : _OCIInitialize;
  OCILdaToSvcCtx         : _OCILdaToSvcCtx;
  OCIParamGet            : _OCIParamGet;
  OCIPasswordChange      : _OCIPasswordChange;
  OCIReset               : _OCIReset;
  OCIServerAttach        : _OCIServerAttach;
  OCIServerDetach        : _OCIServerDetach;
  OCIServerVersion       : _OCIServerVersion;
  OCISessionBegin        : _OCISessionBegin;
  OCISessionEnd          : _OCISessionEnd;
  OCISessionGet          : _OCISessionGet;
  OCISessionRelease      : _OCISessionRelease;
  OCISessionPoolCreate   : _OCISessionPoolCreate;
  OCISessionPoolDestroy  : _OCISessionPoolDestroy;
  OCITransStart          : _OCITransStart;
  OCITransRollback       : _OCITransRollback;
  OCITransCommit         : _OCITransCommit;
  OCITransDetach         : _OCITransDetach;
  OCITransPrepare        : _OCITransPrepare;
  OCITransForget         : _OCITransForget;
  OCIStmtExecute         : _OCIStmtExecute;
  OCIStmtFetch           : _OCIStmtFetch;
  OCIStmtFetch2          : _OCIStmtFetch2;
  OCIStmtGetPieceInfo    : _OCIStmtGetPieceInfo;
  OCIStmtPrepare         : _OCIStmtPrepare;
  OCIStmtPrepare2        : _OCIStmtPrepare2;
  OCIStmtRelease         : _OCIStmtRelease; 
  OCIStmtSetPieceInfo    : _OCIStmtSetPieceInfo;
  OCISvcCtxToLda         : _OCISvcCtxToLda;

  OCILobAppend           : _OCILobAppend;
  OCILobAssign           : _OCILobAssign;
  OCILobCharSetForm      : _OCILobCharSetForm;
  OCILobCharSetId        : _OCILobCharSetId;
  OCILobCopy             : _OCILobCopy;
  OCILobOpen             : _OCILobOpen;
  OCILobClose            : _OCILobClose;
  OCILobIsOpen           : _OCILobIsOpen;
  OCILobCreateTemporary  : _OCILobCreateTemporary;
  OCILobFreeTemporary    : _OCILobFreeTemporary;
  OCILobIsTemporary      : _OCILobIsTemporary;
  OCILobDisableBuffering : _OCILobDisableBuffering;
  OCILobEnableBuffering  : _OCILobEnableBuffering;
  OCILobErase            : _OCILobErase;
  OCILobFileClose        : _OCILobFileClose;
  OCILobFileExists       : _OCILobFileExists;
  OCILobFileGetName      : _OCILobFileGetName;
  OCILobFileIsOpen       : _OCILobFileIsOpen;
  OCILobFileOpen         : _OCILobFileOpen;
  OCILobFileSetName      : _OCILobFileSetName;
  OCILobFlushBuffer      : _OCILobFlushBuffer;
  OCILobGetLength        : _OCILobGetLength;
  OCILobIsEqual          : _OCILobIsEqual;
  OCILobLoadFromFile     : _OCILobLoadFromFile;
  OCILobLocatorIsInit    : _OCILobLocatorIsInit;
  OCILobRead             : _OCILobRead;
  OCILobRead2            : _OCILobRead2;
  OCILobTrim             : _OCILobTrim;
  OCILobWrite            : _OCILobWrite;

  OCICacheFlush          : _OCICacheFlush;
  OCICacheFree           : _OCICacheFree;
  OCICacheRefresh        : _OCICacheRefresh;
  OCICacheUnmark         : _OCICacheUnmark;
  OCICacheUnpin          : _OCICacheUnpin;

  OCIObjectCopy          : _OCIObjectCopy;
  OCIObjectExists        : _OCIObjectExists;
  OCIObjectFlush         : _OCIObjectFlush;
  OCIObjectFree          : _OCIObjectFree;
  OCIObjectGetAttr       : _OCIObjectGetAttr;
  OCIObjectGetInd        : _OCIObjectGetInd;
  OCIObjectGetObjectRef  : _OCIObjectGetObjectRef;
  OCIObjectGetProperty   : _OCIObjectGetProperty;
  OCIObjectGetTypeRef    : _OCIObjectGetTypeRef;
  OCIObjectIsDirty       : _OCIObjectIsDirty;
  OCIObjectIsLocked      : _OCIObjectIsLocked;
  OCIObjectLock          : _OCIObjectLock;
  OCIObjectMarkDelete    : _OCIObjectMarkDelete;
  OCIObjectMarkDeleteByRef : _OCIObjectMarkDeleteByRef;
  OCIObjectMarkUpdate    : _OCIObjectMarkUpdate;
  OCIObjectNew           : _OCIObjectNew;
  OCIObjectPin           : _OCIObjectPin;
  OCIObjectPinCountReset : _OCIObjectPinCountReset;
  OCIObjectPinTable      : _OCIObjectPinTable;
  OCIObjectRefresh       : _OCIObjectRefresh;
  OCIObjectSetAttr       : _OCIObjectSetAttr;
  OCIObjectUnmark        : _OCIObjectUnmark;
  OCIObjectUnmarkByRef   : _OCIObjectUnmarkByRef;
  OCIObjectUnpin         : _OCIObjectUnpin;
  OCITypeByName          : _OCITypeByName;
  OCITypeByRef           : _OCITypeByRef;

  OCICollAppend          : _OCICollAppend;
  OCICollAssign          : _OCICollAssign;
  OCICollAssignElem      : _OCICollAssignElem;
  OCICollGetElem         : _OCICollGetElem;
  OCICollMax             : _OCICollMax;
  OCICollSize            : _OCICollSize;
  OCICollTrim            : _OCICollTrim;
  OCIDateAssign          : _OCIDateAssign;
  OCIDateFromText        : _OCIDateFromText;
  OCIDateGetDate         : _OCIDateGetDate;
  OCIDateGetTime         : _OCIDateGetTime;
  OCIDateSetDate         : _OCIDateSetDate;
  OCIDateSetTime         : _OCIDateSetTime;
  OCIDateToText          : _OCIDateToText;
  OCINumberAssign        : _OCINumberAssign;
  OCINumberCmp           : _OCINumberCmp;
  OCINumberFromInt       : _OCINumberFromInt;
  OCINumberFromReal      : _OCINumberFromReal;
  OCINumberFromText      : _OCINumberFromText;
  OCINumberToInt         : _OCINumberToInt;
  OCINumberToReal        : _OCINumberToReal;
  OCINumberToText        : _OCINumberToText;
  OCIRefAssign           : _OCIRefAssign;
  OCIRefClear            : _OCIRefClear;
  OCIRefIsEqual          : _OCIRefIsEqual;
  OCIRefIsNull           : _OCIRefIsNull;
  OCIRefToHex            : _OCIRefToHex;
  OCIStringAllocSize     : _OCIStringAllocSize;
  OCIStringAssign        : _OCIStringAssign;
  OCIStringAssignText    : _OCIStringAssignText;
  OCIStringPtr           : _OCIStringPtr;
  OCIStringResize        : _OCIStringResize;
  OCIStringSize          : _OCIStringSize;
  OCITableDelete         : _OCITableDelete;
  OCITableExists         : _OCITableExists;
  OCITableFirst          : _OCITableFirst;
  OCITableLast           : _OCITableLast;
  OCITableNext           : _OCITableNext;
  OCITablePrev           : _OCITablePrev;
  OCITableSize           : _OCITableSize;

  OCIEnvCreate               : _OCIEnvCreate;

  OCIDirPathAbort            : _OCIDirPathAbort;
  OCIDirPathColArrayEntryGet : _OCIDirPathColArrayEntryGet;
  OCIDirPathColArrayEntrySet : _OCIDirPathColArrayEntrySet;
  OCIDirPathColArrayRowGet   : _OCIDirPathColArrayRowGet;
  OCIDirPathColArrayReset    : _OCIDirPathColArrayReset;
  OCIDirPathColArrayToStream : _OCIDirPathColArrayToStream;
  OCIDirPathFinish           : _OCIDirPathFinish;
  OCIDirPathLoadStream       : _OCIDirPathLoadStream;
  OCIDirPathPrepare          : _OCIDirPathPrepare;
  OCIDirPathStreamReset      : _OCIDirPathStreamReset;

  OCIDateTimeConstruct          : _OCIDateTimeConstruct;
  OCIDateTimeCheck              : _OCIDateTimeCheck;
  OCIDateTimeFromText           : _OCIDateTimeFromText;
  OCIDateTimeToText             : _OCIDateTimeToText;
  OCIDateTimeGetDate            : _OCIDateTimeGetDate;
  OCIDateTimeGetTime            : _OCIDateTimeGetTime;
  OCIDateTimeGetTimeZoneOffset  : _OCIDateTimeGetTimeZoneOffset;
  OCIDateTimeGetTimeZoneName    : _OCIDateTimeGetTimeZoneName;
  OCIDateTimeAssign             : _OCIDateTimeAssign;
  OCIDateTimeCompare            : _OCIDateTimeCompare;
  OCIIntervalFromText           : _OCIIntervalFromText;
  OCIIntervalToText             : _OCIIntervalToText;
  OCIIntervalCheck              : _OCIIntervalCheck;
  OCIIntervalAssign             : _OCIIntervalAssign;
  OCIIntervalCompare            : _OCIIntervalCompare;
  OCIIntervalSetYearMonth       : _OCIIntervalSetYearMonth;
  OCIIntervalGetYearMonth       : _OCIIntervalGetYearMonth;
  OCIIntervalSetDaySecond       : _OCIIntervalSetDaySecond;
  OCIIntervalGetDaySecond       : _OCIIntervalGetDaySecond;
  OCIIntervalFromNumber         : _OCIIntervalFromNumber;

  OCIPing                       : _OCIPing;

  OCIXMLTypeNew                 : _OCIXMLTypeNew;
  OCIXMLTypeCreateFromSrc       : _OCIXMLTypeCreateFromSrc;
  OCIXMLTypeExtract             : _OCIXMLTypeExtract;
  OCIXMLTypeTransform           : _OCIXMLTypeTransform;
  OCIXMLTypeExists              : _OCIXMLTypeExists;
  OCIXMLTypeIsSchemaBased       : _OCIXMLTypeIsSchemaBased;
  OCIXMLTypeGetSchema           : _OCIXMLTypeGetSchema;
  OCIXMLTypeValidate            : _OCIXMLTypeValidate;
  OCIXMLTypeGetDOM              : _OCIXMLTypeGetDOM;
  OCIXMLTypeGetFromDOM          : _OCIXMLTypeGetFromDOM;
  OCIDOMFree                    : _OCIDOMFree;

  OCIPStreamFromXMLType         : _OCIPStreamFromXMLType;
  OCIPStreamRead                : _OCIPStreamRead;
  OCIPStreamClose               : _OCIPStreamClose;

  OCIRowidToChar                : _OCIRowidToChar;

  OCIExtProcGetEnv              : _OCIExtProcGetEnv;
  OCIExtProcAllocCallMemory     : _OCIExtProcAllocCallMemory;
  OCIExtProcRaiseExcpWithMsg    : _OCIExtProcRaiseExcpWithMsg;

  OCISubscriptionRegister       : _OCISubscriptionRegister;
  OCISubscriptionUnRegister     : _OCISubscriptionUnRegister;
  OCISubscriptionEnable         : _OCISubscriptionEnable;
  OCISubscriptionDisable        : _OCISubscriptionDisable;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  OraMTSSvcGet                  : _OraMTSSvcGet;
  OraMTSSvcRel                  : _OraMTSSvcRel;
  OraMTSJoinTxn                 : _OraMTSJoinTxn;
  OraMTSEnlCtxGet               : _OraMTSEnlCtxGet;
  OraMTSEnlCtxRel               : _OraMTSEnlCtxRel;
  OraMTSSvcEnlist               : _OraMTSSvcEnlist;  
{$ENDIF}
{$ENDIF}

const
  MaxOracleHomes = 10;

type
  TOCICallStyle = (None,OCI73,OCI80);
  TOCICallStyleSet = set of TOCICallStyle;
  TOracleHome = (ohDefault,ohHome0,ohHome1,ohHome2,ohHome3,ohHome4,ohHome5);

  EOCIInitError = class(Exception);

{$IFNDEF CLR}
var
  OCIDLL        : string;
{$ELSE}
const
  OCIDLL = 'oci.dll';
  MTSDLL = 'oramts.dll';
var
{$ENDIF}
  OCIClientDLL : string;
  OCIVersionSt  : _string;
  OCIVersion    : word;
  OCICallStyle  : TOCICallStyle;
  PossibleOCICallStyles : TOCICallStyleSet;
  ObjectVersion : boolean;
  ThreadSafety  : boolean;
  OCIThreaded   : boolean;  // OCI_THREADED for OCI80
  OCIMutexed    : boolean;  // OCI_ENV_NO_MUTEX for OCI81 (works with OCI81 only)
  OCIShared     : boolean;  // OCI_SHARED for OCI81
  OCIEvents     : boolean;  // OCI_EVENTS for OCI81
  OCIEventsVersion: word;
  OCIUnicode    : boolean;
  OCIUnicodeAsNational: boolean = False; // bind all Unicode params as NVarchar data type
  OCIInited     : boolean;
  DirectUnicodeFieldSizeInChars: boolean = True; // NLS_LENGTH_SEMANTICS=CHAR can cut values in the Unicode mode for query : select 'ABCDE' from dual, if server has UTF8 charset
  hOCIEnv       : pOCIEnv;
  hOCIError     : pOCIError;
  OCILite       : boolean;  // Oracle Lite
  OracleHomePath  : string;
  OracleHomeName  : string;
  OracleHomeCount : integer;
  DefaultOracleHome : integer;
  OracleHomePaths : array [0..MaxOracleHomes - 1] of string;
  OracleHomeKeys  : array [0..MaxOracleHomes - 1] of string;
  OracleHomeNames : array [0..MaxOracleHomes - 1] of string;
  OracleErrorMaxLength : integer;
  SubscriptionPort: integer;

  procedure DetectOCI;
  procedure LoadOCI;
  procedure FreeOCI;
  function LoadedOCI: boolean;

  procedure InitOCI;
  procedure FinishOCI;

  procedure CheckOCI;
  procedure CheckOCI73;
  procedure CheckOCI80;
  procedure CheckOCI81;
  procedure CheckOCI90;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  procedure InitMTS;
  procedure FreeMTS;
{$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
  procedure OraError(ErrorCode: sword; UnicodeEnv: boolean; hError: pOCIError = nil);
{$ELSE}
  procedure DoOraError(ErrorCode: sword; UnicodeEnv: boolean; hError: pOCIError = nil);
{$ENDIF}
  function GetOraError(ErrorCode: sword; UnicodeEnv: boolean; var ErrorMsg: _string; hError: pOCIError = nil): sword; overload;
  function GetOraError(ErrorCode: sword; UnicodeEnv: boolean; hError: pOCIError = nil): sword; overload;
  procedure Check(Status: sword; hError: pOCIError = nil); overload;
  procedure Check(Status: sword; UnicodeEnv: boolean; hError: pOCIError = nil); overload;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  procedure MTSError(ErrorCode: sword; UnicodeEnv: boolean; hError: pOCIError = nil);
  function GetMTSError(ErrorCode: sword; UnicodeEnv: boolean; var ErrorMsg: _string; hError: pOCIError = nil): sword; overload;
  function GetMTSError(ErrorCode: sword; UnicodeEnv: boolean; hError: pOCIError = nil): sword; overload;
  procedure MTSCheck(Status: sword; UnicodeEnv: boolean; hError: pOCIError = nil);
{$ENDIF}
{$ENDIF}

  function GetHeapAlloc: integer;
  function GetSharedHeapAlloc: integer;

  function GetSubscriptionPort: integer;

  function GetThreadSafety: boolean;
  procedure SetThreadSafety(Value: boolean);

  function VersionStrToWord(VersionSt: _string): word;

  procedure OCINumberFromBCD(rnum: TBytes; rnum_length: uword; number: pOCINumber);
  procedure OCINumberToBCD(Number: pOCINumber; var Bcd{$IFDEF CLR}: TBytes{$ENDIF});

  function IsUnicodeEnv(Env: pOCIEnv; hError: pOCIError = nil): boolean;
  function StringToHGlobalOCI(const S: _string; var Size: integer; UnicodeEnv: boolean): IntPtr;
  procedure FreeStringOCI(P: IntPtr; UnicodeEnv: boolean);
  function PtrToStringOCI(P: IntPtr; UnicodeEnv: boolean): _string; overload;
  function PtrToStringOCI(P: IntPtr; Size: integer; UnicodeEnv: boolean): _string; overload;
  function SizeOfCharOCI(UnicodeEnv: boolean): integer;

const
  DACProductName = 'ODAC';

implementation

uses
{$IFDEF CLR}
  {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF},
{$ENDIF}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  OraError, OraConsts;
{$ELSE}
  OraErrorUni, OraConstsUni;
{$ENDIF}

var
  OCIFunctionsLinked: boolean;
{$IFDEF MSWINDOWS}
  hOraLib: HMODULE;
  hOraLibClient9: HMODULE;
{$IFNDEF LITE}
  MTSInited: boolean;
  hMTSLib: HMODULE;
{$ENDIF}
{$ENDIF}
{$IFDEF LINUX}
  hOraLib: IntPtr;
  hOraLibClient9: IntPtr;
{$ENDIF}
  hOCIInit: TRTLCriticalSection;
  hMTSInit: TRTLCriticalSection;



function IsUnicodeEnv(Env: pOCIEnv; hError: pOCIError = nil): boolean;
var
  BVal: ub1;
  BPtr: IntPtr;
begin
  if OCIVersion < 9000 then begin
    Result := False;
    exit;
  end;

  if hError = nil then
    hError := hOCIError;

  BPtr := OrdinalToPtr(BVal);
  try
    Check(OCIAttrGet1(Env, OCI_HTYPE_ENV, BPtr, nil, OCI_ATTR_ENV_UTF16, hError), False);
  finally
    PtrToOrdinal(BPtr, BVal);
  end;
  Result := BVal <> 0;
end;

function StringToHGlobalOCI(const S: _string; var Size: integer; UnicodeEnv: boolean): IntPtr;
var
{$IFDEF IS_UNICODE}
  sa: AnsiString;
{$ELSE}
  sw: WideString;
{$ENDIF}
begin
  if UnicodeEnv then begin
  {$IFNDEF IS_UNICODE}
    sw := WideString(S);
    Size := Length(sw) * SizeOf(WideChar);
    GetMem(Result, Size + 2);
    Move(PWideChar(sw)^, Result^, Size + 2);
  {$ELSE}
    Size := Length(S) * SizeOf(WideChar);
  {$IFNDEF CLR}
    Result := PWideChar(S);
  {$ELSE}
    Result := Marshal.StringToHGlobalUni(S);
  {$ENDIF}
  {$ENDIF}
  end
  else begin
  {$IFDEF IS_UNICODE}
    sa := AnsiString(S);
    Size := Length(sa);
  {$IFNDEF CLR}
    GetMem(Result, Size + 1);
    Move(PAnsiChar(sa)^, Result^, Size + 1);
  {$ELSE}
    Result := Marshal.StringToHGlobalAnsi(S);
  {$ENDIF}
  {$ELSE}
    Size := Length(S);
    Result := PAnsiChar(S);
  {$ENDIF}
  end;
end;

procedure FreeStringOCI(P: IntPtr; UnicodeEnv: boolean);
begin
{$IFNDEF CLR}
  if {$IFDEF IS_UNICODE} not {$ENDIF} UnicodeEnv then
    FreeMem(P);
{$ELSE}
  Marshal.FreeHGlobal(P);
{$ENDIF}
end;

function PtrToStringOCI(P: IntPtr; UnicodeEnv: boolean): _string;
begin
  if UnicodeEnv then
    Result := _string(Marshal.PtrToStringUni(P))
  else
    Result := _string(Marshal.PtrToStringAnsi(P));
end;

function PtrToStringOCI(P: IntPtr; Size: integer; UnicodeEnv: boolean): _string;
begin
  if UnicodeEnv then
    Result := _string(Marshal.PtrToStringUni(P, Size div 2))
  else
    Result := _string(Marshal.PtrToStringAnsi(P, Size));
end;

function SizeOfCharOCI(UnicodeEnv: boolean): integer;
begin
  if UnicodeEnv then
    Result := 2
  else
    Result := 1;
end;

{$IFDEF CLR}

{PTRD}

function PTRD.GetUB1Property(Index: Integer): ub1;
begin
  Result := Marshal.ReadByte(Ptr, Index);
end;

procedure PTRD.SetUB1Property(Index: Integer; Value: ub1);
begin
  Marshal.WriteByte(Ptr, Index, Value);
end;

function PTRD.GetUB2Property(Index: Integer): ub2;
begin
  Result := ub2(Marshal.ReadInt16(Ptr, Index));
end;

procedure PTRD.SetUB2Property(Index: Integer; Value: ub2);
begin
  Marshal.WriteInt16(Ptr, Index, Value);
end;

function PTRD.GetUB4Property(Index: Integer): ub4;
begin
  Result := ub4(Marshal.ReadInt32(Ptr, Index));
end;

procedure PTRD.SetUB4Property(Index: Integer; Value: ub4);
begin
  Marshal.WriteInt32(Ptr, Index, Value);
end;

class operator PTRD.Implicit(AValue: IntPtr): PTRD;
begin
  Result.Ptr := AValue;
end;

class operator PTRD.Implicit(AValue: PTRD): IntPtr;
begin
  Result := AValue.Ptr;
end;

{PRowId7}

function PRowId7.GetUB4Property(Index: Integer): ub4;
begin
  Result := Marshal.ReadInt32(Ptr, Index);
end;

procedure PRowId7.SetUB4Property(Index: Integer; Value: ub4);
begin
  Marshal.WriteInt16(Ptr, Index, Value);
end;

function PRowId7.GetTRDProperty(Index: Integer): PTRD;
begin
  Result.Ptr := IntPtr(Integer(Ptr) + Index);
end;

class operator PRowId7.Implicit(AValue: IntPtr): PRowId7;
begin
  Result.Ptr := AValue;
end;

class operator PRowId7.Implicit(AValue: PRowId7): IntPtr;
begin
  Result := AValue.Ptr;
end;

{PCDA}

function PCDA.GetSB2Property(Index: Integer): sb2;
begin
  Result := Marshal.ReadInt16(Ptr, Index);
end;

procedure PCDA.SetSB2Property(Index: Integer; Value: sb2);
begin
  Marshal.WriteInt16(Ptr, Index, Value);
end;

function PCDA.GetSB4Property(Index: Integer): sb4;
begin
  Result := Marshal.ReadInt32(Ptr, Index);
end;

procedure PCDA.SetSB4Property(Index: Integer; Value: sb4);
begin
  Marshal.WriteInt32(Ptr, Index, Value);
end;

function PCDA.GetUB1Property(Index: Integer): ub1;
begin
  Result := Marshal.ReadByte(Ptr, Index);
end;

procedure PCDA.SetUB1Property(Index: Integer; Value: ub1);
begin
  Marshal.WriteByte(Ptr, Index, Value);
end;

function PCDA.GetUB2Property(Index: Integer): ub2;
begin
  Result := ub2(Marshal.ReadInt16(Ptr, Index));
end;

procedure PCDA.SetUB2Property(Index: Integer; Value: ub2);
begin
  Marshal.WriteInt16(Ptr, Index, Value);
end;

function PCDA.GetUB4Property(Index: Integer): ub4;
begin
  Result := ub4(Marshal.ReadInt32(Ptr, Index));
end;

procedure PCDA.SetUB4Property(Index: Integer; Value: ub4);
begin
  Marshal.WriteInt32(Ptr, Index, Value);
end;

function PCDA.GetIntPtrProperty(Index: Integer): IntPtr;
begin
  Result := Marshal.ReadIntPtr(Ptr, Index);
end;

procedure PCDA.SetIntPtrProperty(Index: Integer; Value: IntPtr);
begin
  Marshal.WriteIntPtr(Ptr, Index, Value);
end;

function PCDA.GetTRowId7Property(Index: Integer): PRowId7;
begin
  Result.Ptr := IntPtr(Integer(Ptr) + Index);
end;

procedure PCDA.SetArrayProperty(Index: Integer; ArrayInd: integer; Value: ub1);
begin
  Marshal.WriteByte(Ptr, Index + ArrayInd, Value);
end;

function PCDA.GetArrayProperty(Index: Integer; ArrayInd: integer): ub1;
begin
  Result := Marshal.ReadByte(Ptr, Index + ArrayInd);
end;

class operator PCDA.Implicit(AValue: IntPtr): PCDA;
begin
  Result.Ptr := AValue;
end;

class operator PCDA.Implicit(AValue: PCDA): IntPtr;
begin
  Result := AValue.Ptr;
end;

{PRowId8}

function PRowId8.GetUB2Property(Index: Integer): ub2;
begin
  Result := ub2(Marshal.ReadInt16(Ptr, Index));
end;

procedure PRowId8.SetUB2Property(Index: Integer; Value: ub2);
begin
  Marshal.WriteInt16(Ptr, Index, Value);
end;

function PRowId8.GetUB4Property(Index: Integer): ub4;
begin
  Result := ub4(Marshal.ReadInt32(Ptr, Index));
end;

procedure PRowId8.SetUB4Property(Index: Integer; Value: ub4);
begin
  Marshal.WriteInt32(Ptr, Index, Value);
end;

class operator PRowId8.Implicit(AValue: IntPtr): PRowId8;
begin
  Result.Ptr := AValue;
end;

class operator PRowId8.Implicit(AValue: PRowId8): IntPtr;
begin
  Result := AValue.Ptr;
end;

{PRowid81}

function PRowid81.GetUB1Property(Index: Integer): ub1;
begin
  Result := Marshal.ReadByte(Ptr, Index);
end;

procedure PRowid81.SetUB1Property(Index: Integer; Value: ub1);
begin
  Marshal.WriteByte(Ptr, Index, Value);
end;

function PRowid81.GetUB2Property(Index: Integer): ub2;
begin
  Result := ub2(Marshal.ReadInt16(Ptr, Index));
end;

procedure PRowid81.SetUB2Property(Index: Integer; Value: ub2);
begin
  Marshal.WriteInt16(Ptr, Index, Value);
end;

function PRowid81.GetUB4Property(Index: Integer): ub4;
begin
  Result := ub4(Marshal.ReadInt32(Ptr, Index));
end;

procedure PRowid81.SetUB4Property(Index: Integer; Value: ub4);
begin
  Marshal.WriteInt32(Ptr, Index, Value);
end;

class operator PRowid81.Implicit(AValue: IntPtr): PRowid81;
begin
  Result.Ptr := AValue;
end;

class operator PRowid81.Implicit(AValue: PRowid81): IntPtr;
begin
  Result := AValue.Ptr;
end;

{POCIRowid}

function POCIRowid.GetPRowId8Property(Index: Integer): PRowId8;
begin
  Result := IntPtr(Integer(Ptr) + Index);
end;

class operator POCIRowid.Implicit(AValue: IntPtr): POCIRowid;
begin
  Result.Ptr := AValue;
end;

class operator POCIRowid.Implicit(AValue: POCIRowid): IntPtr;
begin
  Result := AValue.Ptr;
end;

{POCIRowid81}

function POCIRowid81.GetPRowId81Property(Index: Integer): PRowId81;
begin
  Result := Marshal.ReadIntPtr(Ptr, Index);
end;

procedure POCIRowid81.SetPRowId81Property(Index: Integer; Value: PRowId81);
begin
  Marshal.WriteIntPtr(Ptr, Index, Value);
end;

class operator POCIRowid81.Implicit(AValue: IntPtr): pOCIRowid81;
begin
  Result.Ptr := AValue;
end;

class operator POCIRowid81.Implicit(AValue: pOCIRowid81): IntPtr;
begin
  Result := AValue.Ptr;
end;

{$ENDIF}

function NotLink: sword;
begin
  raise Exception.Create('OCI function is not linked');
end;

function OCINumberFromBCD_(Value: TBytes; RealLength: uword): TBytes;
var
  i, exponent: Integer;
  IsFirstDigit, IsFirstDigitBCD, IsPositive: Boolean;
  NumberInd, BCDInd: Integer;
  b: byte;
  bytesCount: integer;
  OldPrecision, Precision, Places: integer;
begin
  if (Value[0] = 0) or (Value[0] > 64) or ((Value[1] and $40) > 0) then begin  // Zero
    SetLength(Result, 1);
    Result[0] := Byte(-128);
    Exit;
  end;
  Precision := Value[0];
  IsPositive := (Value[1] and $80) = 0;
  Places := Value[1] and $3f;

  // last nulls
  i := (Precision + 1) div 2 + 1;
  repeat
   if Value[i] <> 0 then begin
     if (Precision mod 2) = 0 then begin
       if (Value[Precision div 2 + 1] and $f) = 0 then begin //last digit is null
         Dec(Precision);
         Dec(Places);
       end;
     end
     else begin
       if (Value[Precision div 2 + 1] and $f0) = 0 then begin //first digit is null
         Dec(Precision);
         Dec(Places);
       end;
     end;
     break;
   end;
   Dec(Precision, 2);
   Dec(Places, 2);
   Dec(i);
  until i <= 1;

  BCDInd := 2;
  IsFirstDigitBCD := true;
  // first nulls
  OldPrecision := (Precision + 1) div 2 + 1;
  for i := 2 to OldPrecision do begin
   if Value[i] <> 0 then begin
     if (Value[i] and $f0) = 0 then begin
       IsFirstDigitBCD := false;
       Dec(Precision);
     end;
     break;
   end;
   Inc(BCDInd);
   Dec(Precision, 2);
  end;

  if Precision = 0 then begin
    SetLength(Result, 1);
    Result[0] := Byte(-128);
    Exit;
  end;

  IsFirstDigit := ((Precision - Places) mod 2) = 0;

  if Precision - Places >= 0 then
    exponent := (Precision - Places + 1) div 2 - 1
  else
    exponent := (Precision - Places) div 2 - 1;

  bytesCount := (Precision + 1 + ((Precision - Places) and 1)) div 2;

  if IsPositive then begin  // positive
    SetLength(Result, bytesCount + 1);
    Result[0] := exponent + 128 + 65;
  end
  else begin
    if bytesCount < 20 then begin
      SetLength(Result, bytesCount + 2);
      Result[bytesCount + 1] := 102;
    end
    else
      SetLength(Result, bytesCount + 1);
    Result[0] := not(exponent + 128 + 65);
  end;

  NumberInd := 1;
  Result[1] := 0;
  for i := 1 to Precision do begin
    if IsFirstDigitBCD then
      b := (Value[BCDInd] and $f0) shr 4
    else begin
      b := Value[BCDInd] and $f;
      Inc(BCDInd);
    end;
    IsFirstDigitBCD := not IsFirstDigitBCD;

    if IsFirstDigit then
      Result[NumberInd] := b * 10
    else begin
      Result[NumberInd] := Result[NumberInd] + b + 1;
      Inc(NumberInd);
    end;
    IsFirstDigit := not IsFirstDigit;
  end;
  {if Result <> 0 then
    raise Exception.Create(Format('''%s'' is not a valid Oracle Number value', [str]));}
  if not IsFirstDigit then
    Result[NumberInd] := Result[NumberInd] + 1;

  if not IsPositive then begin  // positive
    for i := 1 to bytesCount do
      Result[i] := 102 - Result[i];
  end;
end;

const
  SizeOfTBcd = 34;

type
  TNetBCD = {$IFDEF CLR}TBytes{$ELSE}array [0 .. SizeOfTBcd - 1] of byte{$ENDIF};
  TNetNumber = {$IFDEF CLR}TBytes{$ELSE}array [0 .. OCI_NUMBER_SIZE - 1] of byte{$ENDIF};

procedure OCINumberToBCD_(const number: TNetNumber; numberLength: uword; var Result: TNetBCD);
var
  IsPositive, IsFirstDigit, IsFirstDigitBCD: Boolean;
  BCDInd: Integer;
  exponent: integer;
  len, b: byte;
  i, Precision: integer;
begin
{$IFDEF CLR}
  for i := 0 to SizeOfTBcd - 1 do
    Result[i] := 0;
{$ELSE}
  System.FillChar(Result, SizeOfTBcd, 0);
{$ENDIF}

  len := numberLength;
  if (len = 1) and (number[0] = Byte(-128)) then begin
    Result[0] := 1; // "0"
    Exit;
  end;

  if (len = 2) and (number[0] = Byte(-1)) and (number[0] = 101) then begin
    raise Exception.Create('Cannot convert to Number to BCD');
    {Result[0] := 0;
    Result[1] := 0;//1/0;  // +INF
    Exit;}
  end;

  if (len = 1) and (number[0] = 0) then begin
    raise Exception.Create('Cannot convert to Number to BCD');
    {Result[0] := 0;
    Result[1] := 0;//-1/0;  // -INF
    Exit;}
  end;

// mantissa convert

  if (number[0] and $ffffff80) <> 0 then begin  // is positive
    IsPositive := true;
    Result[1] := 0;
    exponent := ShortInt((number[0] and $ffffff7f) - 65);
  end
  else begin
    IsPositive := false;
    Result[1] := $80;
    if (len - 1 <> 20) or (number[len - 1] = 102) then
      Dec(len);
    exponent := ShortInt((not number[0] and $ffffff7f) - 65);
  end;
  exponent := exponent * 2 + 1;

  // copy digits
  if 2 <= len then begin
    if IsPositive then
      IsFirstDigit := ((number[1] - 1) div 10) <> 0
    else
      IsFirstDigit := ((101 - number[1]) div 10) <> 0;
    IsFirstDigitBCD := true;
    BCDInd := 2 ;

    Precision := (len - 1) * 2;
    if not IsFirstDigit then
      Dec(Precision)
    else
      Inc(exponent);

    // correct precision
    if IsPositive then
      b := Byte(number[len - 1] - 1)
    else
      b := Byte(101 - number[len - 1]);
    if (b mod 10) = 0 then
      Dec(Precision);

    // for 0,<00..0>123 numbers
    if exponent < 0 then begin
      if Precision - exponent > 64 then
        raise Exception.Create('Cannot convert to Number to BCD');
      BCDInd := 2 - exponent div 2;
      IsFirstDigitBCD := (exponent mod 2) = 0;
      Precision := Precision - exponent;
      exponent := 0;
      for i := 2 to BCDInd do
        Result[i] := 0;
    end;

    i := 1;
    repeat
      if IsPositive then
        b := Byte(number[i] - 1)
      else
        b := Byte(101 - number[i]);

      // first digit from number
      if IsFirstDigit then begin
        if IsFirstDigitBCD then
          Result[BCDInd] := (b div 10) shl 4
        else begin
          Result[BCDInd] := Result[BCDInd] + (b div 10);
          Inc(BCDInd);
        end;
        IsFirstDigitBCD := not IsFirstDigitBCD;
      end
      else
        IsFirstDigit := true;

      // second digit from number
      if BCDInd <= 33 then begin
        if IsFirstDigitBCD then
          Result[BCDInd] := (b mod 10) shl 4
        else begin
          Result[BCDInd] := Result[BCDInd] + (b mod 10);
          Inc(BCDInd);
        end;
      end;
      IsFirstDigitBCD := not IsFirstDigitBCD;

      Inc(i);
    until i >= len;
    if (b mod 10) = 0 then begin
      if IsFirstDigitBCD then
        Dec(BCDInd);
      IsFirstDigitBCD := not IsFirstDigitBCD;
    end;
  end
  else begin
    Precision := 0;
    IsFirstDigitBCD := false;
    BCDInd := 0;
  end;

  exponent := Precision - exponent;
  while exponent < 0 do begin
    if BCDInd >= 34 then
      raise Exception.Create('Cannot convert to Number to BCD');
    if IsFirstDigitBCD then
      Result[BCDInd] := 0
    else begin
      Inc(BCDInd);
    end;
    IsFirstDigitBCD := not IsFirstDigitBCD;
    Inc(Precision);
    Inc(exponent);
  end;
  Result[0] := Precision;
  Result[1] := Result[1] or exponent;
end;

procedure OCINumberFromBCD(rnum: TBytes; rnum_length: uword; number: pOCINumber);
var
  Num: TBytes;
begin
  Num := OCINumberFromBCD_(rnum, rnum_length);
  Marshal.WriteByte(number, Length(Num));
  Marshal.Copy(Num, 0, IntPtr(Integer(number) + 1), Length(Num));
end;

procedure OCINumberToBCD(Number: pOCINumber; var Bcd{$IFDEF CLR}: TBytes{$ENDIF});
var
{$IFDEF CLR}
  Num: TBytes;
{$ENDIF}
  NumLength: integer;
begin
  NumLength := Marshal.ReadByte(Number);
{$IFDEF CLR}
  SetLength(Num, NumLength);
  Marshal.Copy(IntPtr(Integer(Number) + 1), Num, 0, Length(Num));
  OCINumberToBCD_(Num, NumLength, Bcd);
{$ELSE}
  OCINumberToBCD_(TNetNumber(Pointer(PAnsiChar(Number) + 1)^), NumLength, TNetBCD(Bcd));
{$ENDIF}
end;

function VersionStrToWord(VersionSt: _string): word;
var
  S: _string;
  PosInd: integer;
  i: integer;
begin
  Result := 0;
  S := Trim(VersionSt);
  for i := 1 to 4 do begin
    if S = '' then
      Result := Result * 10
    else begin
      PosInd := Pos('.', S);
      if PosInd > 0 then begin
        Result := Result * 10 + StrToIntDef(Copy(S, 1, PosInd - 1), 0);
        S := Copy(S, PosInd + 1, Length(S) - PosInd);
      end
      else begin
        Result := Result * 10 + StrToIntDef(S, 0);
        S := '';
      end;
    end;
  end;
end;

{$IFDEF MSWINDOWS}

{$IFNDEF CLR}
function GetOraClient9Proc(Name: string): FARPROC;
begin
  Result := GetProcAddress(hOraLibClient9, PChar(Name));
  if Result = nil then
    Result := @NotLink;
end;

function GetEnvironmentPathVariable: string;
{$IFNDEF FPC}
var
  Len: integer;
{$ENDIF}
begin
{$IFDEF FPC}
  Result := GetEnvironmentVariable('PATH');
{$ELSE}
  Result := '';
  Len := GetEnvironmentVariable('PATH', nil, 0);
  if Len > 0 then begin
    SetLength(Result, Len - 1);
    GetEnvironmentVariable('PATH', PChar(Result), Len);
  end;
{$ENDIF}
end;
{$ENDIF}

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
  OCIVersion := 0;

  Len := GetFileVersionInfoSize(PChar(FileName), Handle);
  SetLength(VersionData, Len);
  if GetFileVersionInfo(PChar(FileName), Handle, Len, VersionData) then begin
    if VerQueryValueA(VersionData, '\VarFileInfo\Translation', VersionSt, Len) then begin
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
{$IFNDEF CLR}
function GetProc(Name: string): FARPROC;
begin
  Result := GetProcAddress(hOraLib, PChar(Name));
  if Result = nil then
    Result := @NotLink;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF LINUX}
function GetProc(Name: string): IntPtr;
begin
  Result := dlsym(hOraLib, PChar(Name));
  if Result = nil then
    Result := @NotLink;
end;
{$ENDIF}

function GetOCIClientVersion(const OCIDLL: string): _string;
var
  i: integer;
  OraLibLoaded: boolean;
  VersionAr: array[0..4] of sword;
  OCIClientVersion: _OCIClientVersion;
begin
  Result := '';
{$IFDEF MSWINDOWS}
  if hOraLib <= 0 then
    hOraLib := LoadLibraryEx(PChar(OCIDLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  OraLibLoaded := hOraLib > 0;
{$ENDIF}
{$IFDEF LINUX}
  if hOraLib = nil then
    hOraLib := dlopen(PChar(OCIDLL), RTLD_LAZY);
  OraLibLoaded := hOraLib <> nil;
{$ENDIF}

  if OraLibLoaded then begin
  {$IFDEF CLR}
    OCIClientVersion := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIClientVersion;
  {$ELSE}
    OCIClientVersion := GetProc('OCIClientVersion');
  {$ENDIF}

  {$IFDEF CLR}
    if GetProcAddress(hOraLib, 'OCIClientVersion') <> nil then begin
  {$ELSE}
    if @OCIClientVersion <> @NotLink then begin
  {$ENDIF}
      if OCIClientVersion(VersionAr[0], VersionAr[1], VersionAr[2], VersionAr[3], VersionAr[4]) = OCI_SUCCESS
      then begin
        for i := 0 to High(VersionAr) do begin
          if i > 0 then
            Result := Result + '.';
          Result := Result + IntToStr(VersionAr[i]);
        end;
      end;
    end
  end;
  if Result = '' then begin
  {$IFDEF MSWINDOWS}
    Result := GetFileVersion(OCIDLL);
  {$ENDIF}
  {$IFDEF LINUX}
    if OraLibLoaded then begin
      if GetProc('OCIArrayDescriptorFree') <> @NotLink then
        Result := '11.1.0.0.0'
      else
      if GetProc('OCIClientVersion') <> @NotLink then
        Result := '10.2.0.0.0'
      else
      if GetProc('OCILobWrite2') <> @NotLink then
        Result := '10.1.0.0.0'
      else
      if GetProc('OCISessionPoolCreate') <> @NotLink then
        Result := '9.2.0.0.0'
      else
      if GetProc('OCIStmtFetch2') <> @NotLink then
        Result := '9.0.0.0.0'
      else
      if GetProc('OCIEnvCreate') <> @NotLink then
        Result := '8.1.0.0.0'
      else
        Result := '8.0.0.0.0';
    end
    else
      Result := '8.0.0.0.0';
  {$ENDIF}
  end;
end;

{$IFDEF MSWINDOWS}

procedure DetectOCI;
const
  BufLen = 100;
var
  Buf: array [0..BufLen] of char;
{$IFNDEF CLR}
  hOracleReg: HKEY;
  Len: UINT;
  SearchRec: TSearchRec;
  St1,OldSt: string;
  i: integer;
  NumSubKeys: Integer;
  MaxSubKeyLen: Integer;

  procedure AddToOracleHomes(StrKey: string; IsOracle7: boolean);
  var
    Key: string;
    hOracleReg: HKEY;
    HomeName: array [0..100] of char;
    HomePath: array [0..255] of char;
    Len: UINT;
    i: integer;
    Success, Found: boolean;
  begin
    if OracleHomeCount >= MaxOracleHomes then
      exit;
    if IsOracle7 then
      Key := 'SOFTWARE\ORACLE\ORACLE_HOMES\' + StrKey
    else
      Key := 'SOFTWARE\ORACLE\' + StrKey;

    if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(Key),
        0, KEY_READ, hOracleReg) = ERROR_SUCCESS
    then begin
      if IsOracle7 then begin
        StrCopy(HomeName, PChar(StrKey));
        Success := True;
      end
      else begin
        Len := 100;
        Success := RegQueryValueEx(hOracleReg, 'ORACLE_HOME_NAME', nil, nil, @HomeName, @Len) = ERROR_SUCCESS;
      end;
      if Success then begin
        Len := 255;
        if RegQueryValueEx(hOracleReg, 'ORACLE_HOME', nil, nil, @HomePath, @Len) = ERROR_SUCCESS
        then begin
          Found := False;
          for i := 0 to OracleHomeCount - 1 do
            if AnsiStrIComp(PChar(OracleHomeNames[i]), HomeName) = 0 then begin
              Found := True;
              break;
            end;
          if not Found then begin
            Inc(OracleHomeCount);
            OracleHomePaths[OracleHomeCount - 1] := HomePath;
            OracleHomeKeys[OracleHomeCount - 1] := StrKey;
            OracleHomeNames[OracleHomeCount - 1] := HomeName;
          end;
        end;
      end;
      RegCloseKey(hOracleReg);
    end;
  end;

  procedure GetDefaultHome;
  var
    PathSt, Str: string;
    iPos, i: integer;
  begin
    PathSt := GetEnvironmentPathVariable;
    while True do begin
      iPos := Pos(';', PathSt);
      if iPos > 0 then begin
        Str := Trim(Copy(PathSt, 1, iPos - 1));
        PathSt := Copy(PathSt, iPos + 1, Length(PathSt));;
        if Str = '' then
          continue;
      end
      else begin
        Str := Trim(PathSt);
        PathSt := '';
        if Str = '' then
          break;
      end;
      if Str[Length(Str)] <> '\' then
        Str := Str + '\';
      for i := 0 to OracleHomeCount - 1 do begin
        if (AnsiStrIComp(PChar(OracleHomePaths[i] + '\bin\'), PChar(Str)) = 0) or
          (AnsiStrIComp(PChar(OracleHomePaths[i] + '\'), PChar(Str)) = 0)
        then
          if FileExists(Str + 'oci.dll') or FileExists(Str + 'ociw32.dll') then begin
            DefaultOracleHome := i;
            exit;
          end;
      end;
    end;
  end;
{$ENDIF}

  procedure RaiseError;
  var
    St: string;
  begin
    St := OCIDLL;
{$IFNDEF CLR}
    OCIDLL := '';
{$ENDIF}
    raise Exception.Create('Cannot find OCI DLL: ' + St);
  end;

begin
{$IFNDEF CLR}
  hOracleReg := 0;
  try
  // Read home information
    OracleHomeCount := 0;
    for i := 0 to MaxOracleHomes - 1 do
      OracleHomePaths[i] := '';

    DefaultOracleHome := -1;

  // find Oracle 7 homes
    if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar('SOFTWARE\ORACLE\ORACLE_HOMES'),
      0, KEY_READ, hOracleReg) = ERROR_SUCCESS
    then begin
      if RegQueryInfoKey(hOracleReg, nil, nil, nil, @NumSubKeys,
        @MaxSubKeyLen, nil, nil, nil, nil, nil, nil) = ERROR_SUCCESS
      then begin
        for i := 0 to NumSubKeys - 1 do begin
          Len := MaxSubKeyLen + 1;
          SetLength(St1, Len);
          RegEnumKeyEx(hOracleReg, i, PChar(St1), Len, nil, nil, nil, nil);
          SetLength(St1, Len);
          AddToOracleHomes(St1, True);
        end;
      end;
      RegCloseKey(hOracleReg);
    end;

  // find 10g homes
    if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar('SOFTWARE\ORACLE'),
      0, KEY_READ, hOracleReg) = ERROR_SUCCESS
    then begin
      if RegQueryInfoKey(hOracleReg, nil, nil, nil, @NumSubKeys,
        @MaxSubKeyLen, nil, nil, nil, nil, nil, nil) = ERROR_SUCCESS
      then begin
        for i := 0 to NumSubKeys - 1 do begin
          Len := MaxSubKeyLen + 1;
          SetLength(St1, Len);
          RegEnumKeyEx(hOracleReg, i, PChar(St1), Len, nil, nil, nil, nil);
          if (AnsiStrLIComp(Pchar(St1), 'HOME', 4) = 0) or (AnsiStrLIComp(Pchar(St1), 'KEY_', 4) = 0) then begin
            Setlength(St1, Len);
            AddToOracleHomes(St1, False);
          end;
        end;
      end;
    end;

  // Find default home
    if OracleHomeCount > 0 then
      GetDefaultHome;

  // read OCIDLL registry variable
    if OCIDLL = '' then begin

      Len := BufLen;
      if RegQueryValueEx(hOracleReg, 'OCIDLL', nil, nil, @Buf, @Len) =
        ERROR_SUCCESS
      then
        OCIDLL := Trim(Buf);
    end;

  // extract Oracle home
    if OCIDLL <> '' then begin
      OracleHomePath := ExtractFileDir(OCIDLL);
      if AnsiUpperCase(Copy(OracleHomePath, Length(OracleHomePath) - 3, 4)) = '\BIN' then
        SetLength(OracleHomePath, Length(OracleHomePath) - 4);
    end;

  // get library name from homes
    if OCIDLL = '' then begin
      if OracleHomeName = '' then begin
        if (OracleHomeCount > 0) and (DefaultOracleHome <> -1) then
          OracleHomePath := OracleHomePaths[DefaultOracleHome]
        else
          OracleHomePath := '';
        if OracleHomePath = '' then begin
          Len := BufLen;
          if RegQueryValueEx(hOracleReg, 'ORACLE_HOME', nil, nil, @Buf, @Len) = ERROR_SUCCESS then begin
            Buf[Len - 1] := #0;
            OracleHomePath := Buf;
          end;
        end;
        if OracleHomePath = '' then
          OCIDLL := 'oci.dll';
      end
      else begin
        for i := 0 to OracleHomeCount - 1 do
          if AnsiStrComp(PChar(OracleHomeNames[i]) , PChar(OracleHomeName)) = 0 then begin
            OracleHomePath := OracleHomePaths[i];
            break;
          end;
        if OracleHomePath = '' then begin
          OCIDLL := 'in ' + OracleHomeName;
          RaiseError;
        end;
      end;

    // Find DLL
      if OracleHomePath <> '' then begin
        OldSt := '';
        if OracleHomePath[Length(OracleHomePath)] = '\' then
          SetLength(OracleHomePath, Length(OracleHomePath) - 1);

        if FindFirst(OracleHomePath + '\BIN\' + 'oci.dll', faAnyFile, SearchRec) = 0 then begin
          OCIDLL := OracleHomePath + '\BIN\' + 'oci.dll';
          SysUtils.FindClose(SearchRec);
        end
        else
        if FindFirst(OracleHomePath + '\' + 'oci.dll', faAnyFile, SearchRec) = 0 then begin
          OCIDLL := OracleHomePath + '\' + 'oci.dll';
          SysUtils.FindClose(SearchRec);
        end
        else
          if FindFirst(OracleHomePath + '\BIN\' + 'ora*.dll', faAnyFile, SearchRec) = 0 then begin
            repeat
              St1 := Copy(ChangeFileExt(SearchRec.Name, ''), 4, 6);
              if (St1[1] >= '0') and (St1[1] <= '9') then
                if St1 > OldSt then begin
                  OCIDLL := OracleHomePath + '\BIN\' + SearchRec.Name;
                  OldSt := St1;
                end;
            until FindNext(SearchRec) <> 0;
            SysUtils.FindClose(SearchRec);
          end;

        if OCIDLL = '' then
          if OracleHomeName = '' then
            OCIDLL := 'oci.dll' // try to search in PATH for default home
          else begin
            OCIDLL := OracleHomePath + '\BIN\' + '*.*';
            RaiseError;
          end;
      end;
    end;

    OCILite := (RegQueryValueEx(hOracleReg, 'OLITE', nil, nil, @Buf, @Len) =
      ERROR_SUCCESS) and (StrLIComp(Buf, 'YES', 3) = 0);
  finally
    if hOracleReg <> 0 then
      RegCloseKey(hOracleReg);
  end;
{$ENDIF}

  OCIVersionSt := GetOCIClientVersion(OCIDLL);
  OCIVersion := VersionStrToWord(OCIVersionSt);

  if LowerCase(ExtractFileName(OCIDLL)) = 'ociw32.dll' then begin
    Include(PossibleOCICallStyles, OCI73);
    OCICallStyle := OCI73;
    Exit;
  end;

  if OCIVersionSt = '' then
    RaiseError;

  if OCIVersion div 100 = 73 then begin
    Include(PossibleOCICallStyles, OCI73);
    OCICallStyle := OCI73;
  end
  else
    if OCIVersion >= 8000 then begin
      Include(PossibleOCICallStyles, OCI73);
      Include(PossibleOCICallStyles, OCI80);
      if OCICallStyle = None then
        OCICallStyle := OCI80;
    end
    else
      raise Exception.Create('OCI version ' + OCIVersionSt + ' is not supported');

{$IFDEF CLR}
  if OCIVersion > 10000 then
    OCIClientDLL := OCIDLL
  else
    OCIClientDLL := 'oraclient9.dll';
{$ELSE}
  if OCIVersion > 10000 then
    OCIClientDLL := OCIDLL
  else
  if OCIVersion > 9000 then
    OCIClientDLL := ExtractFilePath(OCIDLL) + 'oraclient9.dll'
  else
    OCIClientDLL := '';
{$ENDIF}
end;

{$ENDIF}
{$IFDEF LINUX}

function GetOraClient9Proc(Name: string): IntPtr;
begin
  Result := @NotLink;
end;

procedure DetectOCI;
begin
  OracleHomePath := getenv('ORACLE_HOME');
  OCIDLL := 'libclntsh.so';
  Include(PossibleOCICallStyles, OCI73);
  Include(PossibleOCICallStyles, OCI80);
  OCIVersionSt := GetOCIClientVersion(OCIDLL);
  OCIVersion := VersionStrToWord(OCIVersionSt);
  OCICallStyle := OCI80;
end;

{$ENDIF}

procedure LoadOCI;
var
  St: string;

  {hOraClient: HMODULE;
  FileName: string;
  Len: integer;}
begin
  DetectOCI;

{$IFDEF MSWINDOWS}
{$IFNDEF CLR}
  if (OracleHomeCount > 1) and (OracleHomePath <> '') then begin // {(OCIVersion = OCI81)}
    St := GetEnvironmentPathVariable;
    St := ExtractFileDir(OCIDLL) + ';' + St;
    SetEnvironmentVariable('PATH', PChar(St));
  end;

{$ENDIF}
  if hOraLib <= 0 then
    hOraLib := LoadLibraryEx(PChar(OCIDLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  if OCIVersion >= 9000 then
    hOraLibClient9 := LoadLibraryEx(PChar(OCIClientDLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
{$ENDIF}
{$IFDEF LINUX}
  if hOraLib = nil then
    hOraLib := dlopen(PChar(OCIDLL), RTLD_LAZY);
{$ENDIF}

{$IFDEF MSWINDOWS}
  if hOraLib = 0 then begin
{$ENDIF}
{$IFDEF LINUX}
  if hOraLib = nil then begin
{$ENDIF}
    St := OCIDLL;
  {$IFNDEF CLR}
    OCIDLL := '';  // for invalid path
  {$ENDIF}
    raise Exception.Create('Cannot load OCI DLL: ' + St);
  end;

{$IFDEF MSWINDOWS}
  if (OCIVersion >= 9000) and (hOraLibClient9 = 0) then begin
    St := OCIClientDLL;
  {$IFNDEF CLR}
    OCIClientDLL := '';  // for invalid path
  {$ENDIF}
    raise Exception.Create('Cannot load ' + St);
  end;
{$ENDIF}

  if OCI73 in PossibleOCICallStyles then begin
  {$IFDEF CLR}
    obindps  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.obindps;
    obndra   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.obndra ;
    obndrn   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.obndrn ;
    obndrv   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.obndrv ;
    obreak   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.obreak ;
    ocan     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ocan   ;
    oclose   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oclose ;
    ocof     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ocof   ;
    ocom     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ocom   ;
    ocon     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ocon   ;
    odefin   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.odefin ;
    odefinps := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.odefinps ;
    odescr   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.odescr ;
    odessp   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.odessp ;
    oerhms   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oerhms ;
    oermsg   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oermsg ;
    oexec    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oexec  ;
    oexfet   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oexfet ;
    oexn     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oexn   ;
    ofen     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ofen   ;
    ofetch   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ofetch ;
    oflng    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oflng  ;
    ognfd    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ognfd  ;  
    olog     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.olog   ;  
    ologof   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ologof ;  
    onbclr   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.onbclr ;  
    onbset   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.onbset ;  
    onbtst   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.onbtst ;  
    oopen    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oopen  ;  
    oopt     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oopt   ;  
    oparse   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oparse ;  
    opinit   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.opinit ;  
    orol     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.orol   ;  
    ogetpi   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ogetpi ;  
    osetpi   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.osetpi ;  
    sqllda   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.sqllda ;
    sqlld2   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.sqlld2 ;
  {$ELSE}
    obindps  := GetProc('obindps');
    obndra   := GetProc('obndra');
    obndrn   := GetProc('obndrn');
    obndrv   := GetProc('obndrv');
    obreak   := GetProc('obreak');
    ocan     := GetProc('ocan');
    oclose   := GetProc('oclose');
    ocof     := GetProc('ocof');
    ocom     := GetProc('ocom');
    ocon     := GetProc('ocon');
    odefin   := GetProc('odefin');
    odefinps := GetProc('odefinps');
    odescr   := GetProc('odescr');
    odessp   := GetProc('odessp');
    oerhms   := GetProc('oerhms');
    oermsg   := GetProc('oermsg');
    oexec    := GetProc('oexec');
    oexfet   := GetProc('oexfet');
    oexn     := GetProc('oexn');
    ofen     := GetProc('ofen');
    ofetch   := GetProc('ofetch');
    oflng    := GetProc('oflng');
    ognfd    := GetProc('ognfd');
    olog     := GetProc('olog');
    ologof   := GetProc('ologof');
    onbclr   := GetProc('onbclr');
    onbset   := GetProc('onbset');
    onbtst   := GetProc('onbtst');
    oopen    := GetProc('oopen');
    oopt     := GetProc('oopt');
    oparse   := GetProc('oparse');
    opinit   := GetProc('opinit');
    orol     := GetProc('orol');
    ogetpi   := GetProc('ogetpi');
    osetpi   := GetProc('osetpi');
    sqllda   := GetProc('sqllda');
    sqlld2   := GetProc('sqlld2');
  {$ENDIF}
  end;

  if OCI80 in PossibleOCICallStyles then begin
  {$IFDEF CLR}
    OCIAttrGet1            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAttrGet1;
    OCIAttrGet2            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAttrGet2;
    OCIAttrSet1            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAttrSet1;
    OCIAttrSet2            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAttrSet2;
    //OCIAttrSet3            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAttrSet3;
    OCIBindArrayOfStruct   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIBindArrayOfStruct;
    OCIBindByName          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIBindByName;
    OCIBindByPos           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIBindByPos;
    OCIBindDynamic         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIBindDynamic;
    OCIBindObject          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIBindObject;
    OCIBreak               := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIBreak;
    OCIDefineArrayOfStruct := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDefineArrayOfStruct;
    OCIDefineByPos         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDefineByPos;
    OCIDefineDynamic       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDefineDynamic;
    OCIDefineObject        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDefineObject;
    OCIDescribeAny         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDescribeAny;
    OCIDescriptorAlloc     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDescriptorAlloc;
    OCIDescriptorFree      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDescriptorFree;
    OCIEnvInit             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIEnvInit;
    OCIErrorGet            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIErrorGet;
    OCIHandleAlloc         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIHandleAlloc;
    OCIHandleFree          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIHandleFree;
    OCIInitialize          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIInitialize;
    OCILdaToSvcCtx         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILdaToSvcCtx;
    OCIParamGet            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIParamGet;
    OCIPasswordChange      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPasswordChange;
    OCIReset               := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIReset;
    OCIServerAttach        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIServerAttach;
    OCIServerDetach        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIServerDetach;
    OCIServerVersion       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIServerVersion;
    OCISessionBegin        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISessionBegin;
    OCISessionEnd          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISessionEnd;
    OCISessionGet          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISessionGet;
    OCISessionRelease      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISessionRelease;
    OCISessionPoolCreate   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISessionPoolCreate;
    OCISessionPoolDestroy  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISessionPoolDestroy;
    OCITransStart          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransStart;
    OCITransRollback       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransRollback;
    OCITransCommit         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransCommit;
    OCITransDetach         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransDetach;
    OCITransPrepare        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransPrepare;
    OCITransForget         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransForget;
    OCIStmtExecute         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtExecute;
    OCIStmtFetch           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtFetch;
    OCIStmtFetch2          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtFetch2;
    OCIStmtGetPieceInfo    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtGetPieceInfo;
    OCIStmtPrepare         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtPrepare;
    OCIStmtPrepare2        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtPrepare2;
    OCIStmtRelease         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtRelease;
    OCIStmtSetPieceInfo    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtSetPieceInfo;
    OCISvcCtxToLda         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISvcCtxToLda;
    OCITransCommit         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransCommit;
    OCITransRollback       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransRollback;

    OCILobAppend           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobAppend;
    OCILobAssign           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobAssign;
    OCILobCharSetForm      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobCharSetForm;
    OCILobCharSetId        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobCharSetId;
    OCILobCopy             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobCopy;
    OCILobOpen             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobOpen;
    OCILobClose            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobClose;
    OCILobIsOpen           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobIsOpen;
    OCILobCreateTemporary  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobCreateTemporary;
    OCILobFreeTemporary    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFreeTemporary;
    OCILobIsTemporary      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobIsTemporary;
    OCILobDisableBuffering := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobDisableBuffering;
    OCILobEnableBuffering  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobEnableBuffering;
    OCILobErase            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobErase;
    OCILobFileClose        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFileClose;
    OCILobFileExists       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFileExists;
    OCILobFileGetName      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFileGetName;
    OCILobFileIsOpen       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFileIsOpen;
    OCILobFileOpen         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFileOpen;
    OCILobFileSetName      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFileSetName;
    OCILobFlushBuffer      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFlushBuffer;
    OCILobGetLength        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobGetLength;
    OCILobIsEqual          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobIsEqual;
    OCILobLoadFromFile     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobLoadFromFile;
    OCILobLocatorIsInit    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobLocatorIsInit;
    OCILobRead             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobRead;
    OCILobTrim             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobTrim;
    OCILobWrite            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobWrite;

    OCISubscriptionRegister   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISubscriptionRegister;
    OCISubscriptionUnRegister := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISubscriptionUnRegister;
    OCISubscriptionEnable     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISubscriptionEnable;
    OCISubscriptionDisable    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISubscriptionDisable;

    if ObjectVersion then begin
    OCICacheFlush          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICacheFlush;
    OCICacheFree           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICacheFree;
    OCICacheRefresh        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICacheRefresh;
    OCICacheUnmark         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICacheUnmark;
    OCICacheUnpin          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICacheUnpin;

    OCIObjectCopy          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectCopy;
    OCIObjectExists        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectExists;
    OCIObjectFlush         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectFlush;
    OCIObjectFree          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectFree;
    OCIObjectGetAttr       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectGetAttr;
    OCIObjectGetInd        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectGetInd;
    OCIObjectGetObjectRef  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectGetObjectRef;
    OCIObjectGetProperty   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectGetProperty;
    OCIObjectGetTypeRef    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectGetTypeRef;
    OCIObjectIsDirty       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectIsDirty;
    OCIObjectIsLocked      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectIsLocked;
    OCIObjectLock          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectLock;
    OCIObjectMarkDelete    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectMarkDelete;
    OCIObjectMarkDeleteByRef :={$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectMarkDeleteByRef;
    OCIObjectMarkUpdate    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectMarkUpdate;
    OCIObjectNew           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectNew;
    OCIObjectPin           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectPin;
    OCIObjectPinCountReset := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectPinCountReset;
    OCIObjectPinTable      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectPinTable;
    OCIObjectRefresh       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectRefresh;
    OCIObjectSetAttr       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectSetAttr;
    OCIObjectUnmark        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectUnmark;
    OCIObjectUnmarkByRef   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectUnmarkByRef;
    OCIObjectUnpin         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectUnpin;
    OCITypeByName          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITypeByName;
    OCITypeByRef           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITypeByRef;

    OCICollAppend          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollAppend;
    OCICollAssign          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollAssign;
    OCICollAssignElem      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollAssignElem;
    OCICollGetElem         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollGetElem;
    OCICollMax             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollMax;
    OCICollSize            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollSize;
    OCICollTrim            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollTrim;
    OCIDateAssign          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateAssign;
    OCIDateFromText        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateFromText;
    OCIDateGetDate         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateGetDate;
    OCIDateGetTime         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateGetTime;
    OCIDateSetDate         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateSetDate;
    OCIDateSetTime         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateSetTime;
    OCIDateToText          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateToText;
    OCINumberAssign        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberAssign;
    OCINumberCmp           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberCmp;
    OCINumberFromInt       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberFromInt;
    OCINumberFromReal      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberFromReal;
    OCINumberFromText      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberFromText;
    OCINumberToInt         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberToInt;
    OCINumberToReal        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberToReal;
    OCINumberToText        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberToText;
    OCIRefAssign           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIRefAssign;
    OCIRefClear            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIRefClear;
    OCIRefIsEqual          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIRefIsEqual;
    OCIRefIsNull           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIRefIsNull;
    OCIRefToHex            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIRefToHex;
    OCIStringAllocSize     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStringAllocSize;
    OCIStringAssign        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStringAssign;
    OCIStringAssignText    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStringAssignText;
    OCIStringPtr           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStringPtr;
    OCIStringResize        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStringResize;
    OCIStringSize          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStringSize;
    OCITableDelete         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITableDelete;
    OCITableExists         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITableExists;
    OCITableFirst          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITableFirst;
    OCITableLast           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITableLast;
    OCITableNext           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITableNext;
    OCITablePrev           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITablePrev;
    OCITableSize           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITableSize;

    end; // ObjectVersion
  {$ELSE}
    OCIAttrGet1            := GetProc('OCIAttrGet');
    OCIAttrGet2            := _OCIAttrGet2(OCIAttrGet1);
    OCIAttrSet1            := GetProc('OCIAttrSet');
    OCIAttrSet2            := _OCIAttrSet2(OCIAttrSet1);
    //OCIAttrSet3            := GetProc('OCIAttrSet');
    OCIBindArrayOfStruct   := GetProc('OCIBindArrayOfStruct');
    OCIBindByName          := GetProc('OCIBindByName');
    OCIBindByPos           := GetProc('OCIBindByPos');
    OCIBindDynamic         := GetProc('OCIBindDynamic');
    OCIBindObject          := GetProc('OCIBindObject');
    OCIBreak               := GetProc('OCIBreak');
    OCIDefineArrayOfStruct := GetProc('OCIDefineArrayOfStruct');
    OCIDefineByPos         := GetProc('OCIDefineByPos');
    OCIDefineDynamic       := GetProc('OCIDefineDynamic');
    OCIDefineObject        := GetProc('OCIDefineObject');
    OCIDescribeAny         := GetProc('OCIDescribeAny');
    OCIDescriptorAlloc     := GetProc('OCIDescriptorAlloc');
    OCIDescriptorFree      := GetProc('OCIDescriptorFree');
    OCIEnvInit             := GetProc('OCIEnvInit');
    OCIErrorGet            := GetProc('OCIErrorGet');
    OCIHandleAlloc         := GetProc('OCIHandleAlloc');
    OCIHandleFree          := GetProc('OCIHandleFree');
    OCIInitialize          := GetProc('OCIInitialize');
    OCILdaToSvcCtx         := GetProc('OCILdaToSvcCtx');
    OCIParamGet            := GetProc('OCIParamGet');
    OCIPasswordChange      := GetProc('OCIPasswordChange');
    OCIReset               := GetProc('OCIReset');
    OCIServerAttach        := GetProc('OCIServerAttach');
    OCIServerDetach        := GetProc('OCIServerDetach');
    OCIServerVersion       := GetProc('OCIServerVersion');
    OCISessionBegin        := GetProc('OCISessionBegin');
    OCISessionEnd          := GetProc('OCISessionEnd');
    OCISessionGet          := GetProc('OCISessionGet');
    OCISessionRelease      := GetProc('OCISessionRelease');
    OCISessionPoolCreate   := GetProc('OCISessionPoolCreate');
    OCISessionPoolDestroy  := GetProc('OCISessionPoolDestroy');
    OCITransStart          := GetProc('OCITransStart');
    OCITransRollback       := GetProc('OCITransRollback');
    OCITransCommit         := GetProc('OCITransCommit');
    OCITransDetach         := GetProc('OCITransDetach');
    OCITransPrepare        := GetProc('OCITransPrepare');
    OCITransForget         := GetProc('OCITransForget');
    OCIStmtExecute         := GetProc('OCIStmtExecute');
    OCIStmtFetch           := GetProc('OCIStmtFetch');
    OCIStmtFetch2          := GetProc('OCIStmtFetch2');
    OCIStmtGetPieceInfo    := GetProc('OCIStmtGetPieceInfo');
    OCIStmtPrepare         := GetProc('OCIStmtPrepare');
    OCIStmtSetPieceInfo    := GetProc('OCIStmtSetPieceInfo');
    OCISvcCtxToLda         := GetProc('OCISvcCtxToLda');

    OCILobAppend           := GetProc('OCILobAppend');
    OCILobAssign           := GetProc('OCILobAssign');
    OCILobCharSetForm      := GetProc('OCILobCharSetForm');
    OCILobCharSetId        := GetProc('OCILobCharSetId');
    OCILobCopy             := GetProc('OCILobCopy');
    OCILobOpen             := GetProc('OCILobOpen');
    OCILobClose            := GetProc('OCILobClose');
    OCILobIsOpen           := GetProc('OCILobIsOpen');
    OCILobCreateTemporary  := GetProc('OCILobCreateTemporary');
    OCILobFreeTemporary    := GetProc('OCILobFreeTemporary');
    OCILobIsTemporary      := GetProc('OCILobIsTemporary');
    OCILobDisableBuffering := GetProc('OCILobDisableBuffering');
    OCILobEnableBuffering  := GetProc('OCILobEnableBuffering');
    OCILobErase            := GetProc('OCILobErase');
    OCILobFileClose        := GetProc('OCILobFileClose');
    OCILobFileExists       := GetProc('OCILobFileExists');
    OCILobFileGetName      := GetProc('OCILobFileGetName');
    OCILobFileIsOpen       := GetProc('OCILobFileIsOpen');
    OCILobFileOpen         := GetProc('OCILobFileOpen');
    OCILobFileSetName      := GetProc('OCILobFileSetName');
    OCILobFlushBuffer      := GetProc('OCILobFlushBuffer');
    OCILobGetLength        := GetProc('OCILobGetLength');
    OCILobIsEqual          := GetProc('OCILobIsEqual');
    OCILobLoadFromFile     := GetProc('OCILobLoadFromFile');
    OCILobLocatorIsInit    := GetProc('OCILobLocatorIsInit');
    OCILobRead             := GetProc('OCILobRead');
    OCILobTrim             := GetProc('OCILobTrim');
    OCILobWrite            := GetProc('OCILobWrite');

    OCIExtProcGetEnv       := GetProc('ociepgoe');
    OCIExtProcAllocCallMemory := GetProc('ociepacm');
    OCIExtProcRaiseExcpWithMsg := GetProc('ociepmsg');

    OCISubscriptionRegister   := GetProc('OCISubscriptionRegister');
    OCISubscriptionUnRegister := GetProc('OCISubscriptionUnRegister');
    OCISubscriptionEnable     := GetProc('OCISubscriptionEnable');
    OCISubscriptionDisable    := GetProc('OCISubscriptionDisable');

    if ObjectVersion then begin
    OCICacheFlush          := GetProc('OCICacheFlush');
    OCICacheFree           := GetProc('OCICacheFree');
    OCICacheRefresh        := GetProc('OCICacheRefresh');
    OCICacheUnmark         := GetProc('OCICacheUnmark');
    OCICacheUnpin          := GetProc('OCICacheUnpin');

    OCIObjectCopy          := GetProc('OCIObjectCopy');
    OCIObjectExists        := GetProc('OCIObjectExists');
    OCIObjectFlush         := GetProc('OCIObjectFlush');
    OCIObjectFree          := GetProc('OCIObjectFree');
    OCIObjectGetAttr       := GetProc('OCIObjectGetAttr');
    OCIObjectGetInd        := GetProc('OCIObjectGetInd');
    OCIObjectGetObjectRef  := GetProc('OCIObjectGetObjectRef');
    OCIObjectGetProperty   := GetProc('OCIObjectGetProperty');
    OCIObjectGetTypeRef    := GetProc('OCIObjectGetTypeRef');
    OCIObjectIsDirty       := GetProc('OCIObjectIsDirty');
    OCIObjectIsLocked      := GetProc('OCIObjectIsLocked');
    OCIObjectLock          := GetProc('OCIObjectLock');
    OCIObjectMarkDelete    := GetProc('OCIObjectMarkDelete');
    OCIObjectMarkDeleteByRef := GetProc('OCIObjectMarkDeleteByRef');
    OCIObjectMarkUpdate    := GetProc('OCIObjectMarkUpdate');
    OCIObjectNew           := GetProc('OCIObjectNew');
    OCIObjectPin           := GetProc('OCIObjectPin');
    OCIObjectPinCountReset := GetProc('OCIObjectPinCountReset');
    OCIObjectPinTable      := GetProc('OCIObjectPinTable');
    OCIObjectRefresh       := GetProc('OCIObjectRefresh');
    OCIObjectSetAttr       := GetProc('OCIObjectSetAttr');
    OCIObjectUnmark        := GetProc('OCIObjectUnmark');
    OCIObjectUnmarkByRef   := GetProc('OCIObjectUnmarkByRef');
    OCIObjectUnpin         := GetProc('OCIObjectUnpin');
    OCITypeByName          := GetProc('OCITypeByName');
    OCITypeByRef           := GetProc('OCITypeByRef');

    OCICollAppend          := GetProc('OCICollAppend');
    OCICollAssign          := GetProc('OCICollAssign');
    OCICollAssignElem      := GetProc('OCICollAssignElem');
    OCICollGetElem         := GetProc('OCICollGetElem');
    OCICollMax             := GetProc('OCICollMax');
    OCICollSize            := GetProc('OCICollSize');
    OCICollTrim            := GetProc('OCICollTrim');
    OCIDateAssign          := GetProc('OCIDateAssign');
    OCIDateFromText        := GetProc('OCIDateFromText');
    OCIDateGetDate         := GetProc('OCIDateGetDate');
    OCIDateGetTime         := GetProc('OCIDateGetTime');
    OCIDateSetDate         := GetProc('OCIDateSetDate');
    OCIDateSetTime         := GetProc('OCIDateSetTime');
    OCIDateToText          := GetProc('OCIDateToText');
    OCINumberAssign        := GetProc('OCINumberAssign');
    OCINumberCmp           := GetProc('OCINumberCmp');
    OCINumberFromInt       := GetProc('OCINumberFromInt');
    OCINumberFromReal      := GetProc('OCINumberFromReal');
    OCINumberFromText      := GetProc('OCINumberFromText');
    OCINumberToInt         := GetProc('OCINumberToInt');
    OCINumberToReal        := GetProc('OCINumberToReal');
    OCINumberToText        := GetProc('OCINumberToText');
    OCIRefAssign           := GetProc('OCIRefAssign');
    OCIRefClear            := GetProc('OCIRefClear');
    OCIRefIsEqual          := GetProc('OCIRefIsEqual');
    OCIRefIsNull           := GetProc('OCIRefIsNull');
    OCIRefToHex            := GetProc('OCIRefToHex');
    OCIStringAllocSize     := GetProc('OCIStringAllocSize');
    OCIStringAssign        := GetProc('OCIStringAssign');
    OCIStringAssignText    := GetProc('OCIStringAssignText');
    OCIStringPtr           := GetProc('OCIStringPtr');
    OCIStringResize        := GetProc('OCIStringResize');
    OCIStringSize          := GetProc('OCIStringSize');
    OCITableDelete         := GetProc('OCITableDelete');
    OCITableExists         := GetProc('OCITableExists');
    OCITableFirst          := GetProc('OCITableFirst');
    OCITableLast           := GetProc('OCITableLast');
    OCITableNext           := GetProc('OCITableNext');
    OCITablePrev           := GetProc('OCITablePrev');
    OCITableSize           := GetProc('OCITableSize');

    end; // ObjectVersion
  {$ENDIF}
  end;

  if OCIVersion >= 8100 then begin
  {$IFDEF CLR}
    OCIEnvCreate                := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIEnvCreate;

    OCIDirPathAbort             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathAbort;
    OCIDirPathColArrayEntryGet  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathColArrayEntryGet;
    OCIDirPathColArrayEntrySet  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathColArrayEntrySet;
    OCIDirPathColArrayRowGet    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathColArrayRowGet;
    OCIDirPathColArrayReset     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathColArrayReset;
    OCIDirPathColArrayToStream  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathColArrayToStream;
    OCIDirPathFinish            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathFinish;
    OCIDirPathLoadStream        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathLoadStream;
    OCIDirPathPrepare           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathPrepare;
    OCIDirPathStreamReset       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathStreamReset;
  {$ELSE}
    OCIEnvCreate                := GetProc('OCIEnvCreate');

    OCIDirPathAbort             := GetProc('OCIDirPathAbort');
    OCIDirPathColArrayEntryGet  := GetProc('OCIDirPathColArrayEntryGet');
    OCIDirPathColArrayEntrySet  := GetProc('OCIDirPathColArrayEntrySet');
    OCIDirPathColArrayRowGet    := GetProc('OCIDirPathColArrayRowGet');
    OCIDirPathColArrayReset     := GetProc('OCIDirPathColArrayReset');
    OCIDirPathColArrayToStream  := GetProc('OCIDirPathColArrayToStream');
    OCIDirPathFinish            := GetProc('OCIDirPathFinish');
    OCIDirPathLoadStream        := GetProc('OCIDirPathLoadStream');
    OCIDirPathPrepare           := GetProc('OCIDirPathPrepare');
    OCIDirPathStreamReset       := GetProc('OCIDirPathStreamReset');
  {$ENDIF}
  end;

  if OCIVersion >= 9000 then begin
  {$IFDEF CLR}
    OCIDateTimeConstruct          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeConstruct;
    OCIDateTimeCheck              := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeCheck;
    OCIDateTimeFromText           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeFromText;
    OCIDateTimeToText             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeToText;
    OCIDateTimeGetDate            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeGetDate;
    OCIDateTimeGetTime            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeGetTime;
    OCIDateTimeGetTimeZoneOffset  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeGetTimeZoneOffset;
    OCIDateTimeGetTimeZoneName    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeGetTimeZoneName;
    OCIDateTimeAssign             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeAssign;
    OCIDateTimeCompare            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeCompare;
    OCIIntervalFromText           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalFromText;
    OCIIntervalToText             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalToText;
    OCIIntervalCheck              := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalCheck;
    OCIIntervalAssign             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalAssign;
    OCIIntervalCompare            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalCompare;
    OCIIntervalSetYearMonth       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalSetYearMonth;
    OCIIntervalGetYearMonth       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalGetYearMonth;
    OCIIntervalSetDaySecond       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalSetDaySecond;
    OCIIntervalGetDaySecond       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalGetDaySecond;
    OCIIntervalFromNumber         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalFromNumber;

    OCIPing                       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPing;

    OCIXMLTypeNew                 := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeNew;
    OCIXMLTypeCreateFromSrc       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeCreateFromSrc;
    OCIXMLTypeExtract             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeExtract;
    OCIXMLTypeTransform           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeTransform;
    OCIXMLTypeExists              := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeExists;
    OCIXMLTypeIsSchemaBased       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeIsSchemaBased;
    OCIXMLTypeGetSchema           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeGetSchema;
    OCIXMLTypeValidate            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeValidate;

    OCIXMLTypeGetDOM              := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeGetDOM;
    OCIXMLTypeGetFromDOM          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeGetFromDOM;
    OCIDOMFree                    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDOMFree;

    OCIRowidToChar                := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIRowidToChar;

    if OCIVersion > 10000 then begin
      OCIPStreamFromXMLType         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPStreamFromXMLType10;
      OCIPStreamRead                := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPStreamRead10;
      OCIPStreamClose               := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPStreamClose10;
    end
    else begin
      OCIPStreamFromXMLType         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPStreamFromXMLType;
      OCIPStreamRead                := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPStreamRead;
      OCIPStreamClose               := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPStreamClose;
    end;
  {$ELSE}
    OCIDateTimeConstruct          := GetProc('OCIDateTimeConstruct');
    OCIDateTimeCheck              := GetProc('OCIDateTimeCheck');
    OCIDateTimeFromText           := GetProc('OCIDateTimeFromText');
    OCIDateTimeToText             := GetProc('OCIDateTimeToText');
    OCIDateTimeGetDate            := GetProc('OCIDateTimeGetDate');
    OCIDateTimeGetTime            := GetProc('OCIDateTimeGetTime');
    OCIDateTimeGetTimeZoneOffset  := GetProc('OCIDateTimeGetTimeZoneOffset');
    OCIDateTimeGetTimeZoneName    := GetProc('OCIDateTimeGetTimeZoneName');
    OCIDateTimeAssign             := GetProc('OCIDateTimeAssign');
    OCIDateTimeCompare            := GetProc('OCIDateTimeCompare');
    OCIIntervalFromText           := GetProc('OCIIntervalFromText');
    OCIIntervalToText             := GetProc('OCIIntervalToText');
    OCIIntervalCheck              := GetProc('OCIIntervalCheck');
    OCIIntervalAssign             := GetProc('OCIIntervalAssign');
    OCIIntervalCompare            := GetProc('OCIIntervalCompare');
    OCIIntervalSetYearMonth       := GetProc('OCIIntervalSetYearMonth');
    OCIIntervalGetYearMonth       := GetProc('OCIIntervalGetYearMonth');
    OCIIntervalSetDaySecond       := GetProc('OCIIntervalSetDaySecond');
    OCIIntervalGetDaySecond       := GetProc('OCIIntervalGetDaySecond');
    OCIIntervalFromNumber         := GetProc('OCIIntervalFromNumber');

    OCIPing                       := GetProc('OCIPing');

    OCIXMLTypeNew                 := GetProc('OCIXMLTypeNew');
    OCIXMLTypeCreateFromSrc       := GetProc('OCIXMLTypeCreateFromSrc');
    OCIXMLTypeExtract             := GetProc('OCIXMLTypeExtract');
    OCIXMLTypeTransform           := GetProc('OCIXMLTypeTransform');
    OCIXMLTypeExists              := GetProc('OCIXMLTypeExists');
    OCIXMLTypeIsSchemaBased       := GetProc('OCIXMLTypeIsSchemaBased');
    OCIXMLTypeGetSchema           := GetProc('OCIXMLTypeGetSchema');
    OCIXMLTypeValidate            := GetProc('OCIXMLTypeValidate');
    OCIXMLTypeGetDOM              := GetProc('OCIXMLTypeGetDOM');
    OCIXMLTypeGetFromDOM          := GetProc('OCIXMLTypeGetFromDOM');
    OCIDOMFree                    := GetProc('OCIDOMFree');

    OCIRowidToChar                := GetProc('OCIRowidToChar');

    OCIPStreamFromXMLType         := GetOraClient9Proc('OCIPStreamFromXMLType');
    OCIPStreamRead                := GetOraClient9Proc('OCIPStreamRead');
    OCIPStreamClose               := GetOraClient9Proc('OCIPStreamClose');

    OCIStmtPrepare2               := GetProc('OCIStmtPrepare2');
    OCIStmtRelease                := GetProc('OCIStmtRelease');
  {$ENDIF}
  end;

  if OCIVersion >= 10000 then begin
  {$IFDEF CLR}
    OCILobRead2 := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobRead2;
  {$ELSE}
    OCILobRead2 := GetProc('OCILobRead2');
  {$ENDIF}
  end;

  OCIFunctionsLinked := True;    
end;

procedure FreeOCI;
{$IFDEF WIN32_64}
var
  Env, Home: string;
  Ind: integer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  FreeMTS;
{$ENDIF}
{$ENDIF}

{$IFNDEF CLR}
  if not System.IsLibrary then  // WAR access violation on free DLL with ODAC
{$ENDIF}
    FinishOCI;

  OCIInited := False;

{$IFDEF WIN32_64}
  if (OracleHomeCount > 1) and (OracleHomePath <> '') then begin
    // remove added home from PATH variable
    Env := GetEnvironmentPathVariable;
    Home := ExtractFileDir(OCIDLL) + ';';
    Ind := Pos(Home, Env);
    if Ind > 0 then begin
      Env := Copy(Env, 1, Ind - 1) + Copy(Env, Ind + Length(Home),
        Length(Env) - Length(Home) - Ind + 1);
      SetEnvironmentVariable('PATH', PChar(Env));
    end;
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  if hOraLib > 0 then begin
    FreeLibrary(hOraLib);  // Returns False on Win95 in DLL
    hOraLib := 0;
  end;
  if hOraLibClient9 > 0 then begin
    FreeLibrary(hOraLibClient9);  // Returns False on Win95 in DLL
    hOraLibClient9 := 0;
  end;
{$ENDIF}
{$IFDEF LINUX}
  if hOraLib <> nil then begin
    //dlclose(hOraLib); // BUG: SIGSEGV on close application
    hOraLib := nil;
  end;
  if hOraLibClient9 <> nil then begin
    //dlclose(hOraLibClient9); // BUG: SIGSEGV on close application
    hOraLibClient9 := nil;
  end;
{$ENDIF}
  OCIFunctionsLinked := False;

{$IFNDEF CLR}
  OCIDLL := '';
  OCIClientDLL := '';
{$ENDIF}
  OCIVersionSt := '';
  OCIVersion := 0;
  OCICallStyle := None;
  PossibleOCICallStyles := [];
  OracleHomePath := '';
end;

function LoadedOCI: boolean;
begin
{$IFDEF MSWINDOWS}
  Result :=
      (hOraLib > 0) and OCIFunctionsLinked and
      ( not (OCIVersion >= 9000) or (hOraLibClient9 > 0) );
{$ENDIF}
{$IFDEF LINUX}
  Result := (hOraLib <> nil) and OCIFunctionsLinked;
{$ENDIF}
end;

procedure InitOCI;
  procedure Error(Res: sword);
  begin
    raise EOCIInitError.CreateFmt(SCannotInitOCI, [Res]);
  end;
var
  InitMode: integer;
  EnvInitMode: integer;
  Res: sword;
begin
  EnterCriticalSection(hOCIInit);
  try
    if not OCIInited then begin
      if not LoadedOCI then
        LoadOCI;

      if OCI80 in PossibleOCICallStyles then begin
        // WAR problem with OCI_THREADED on HandelFree(hStmt... with Oracle 8.0.4
        //                  and create direct path handle
        InitMode := OCI_DEFAULT or OCI_OBJECT;
        if OCIUnicode and (OCIVersion >= 9000) then
          InitMode := InitMode or OCI_UTF16;
        if OCIThreaded and (OCIVersion > 8040) then // supports from OCI 8.0.5
          InitMode := InitMode or OCI_THREADED;
        if OCIShared and (OCIVersion >= 8140) then  // supports from OCI 8.1.5
          InitMode := InitMode or OCI_SHARED;
        if OCIEvents and (OCIVersion >= 8140) and (OCIVersion >= OCIEventsVersion) then
          InitMode := InitMode or OCI_EVENTS;

        if OCIVersion >= 8100 then begin
        // Oracle 8.1.5
          if not OCIMutexed and OCIThreaded {$IFDEF LITE} or (OCIVersion < 9000) {$ENDIF}then
            InitMode := InitMode or OCI_NO_MUTEX;

          Res := OCIEnvCreate(hOCIEnv, InitMode, nil, nil, nil, nil, 0, nil);
          if Res <> OCI_SUCCESS then
            Error(Res);
        end
        else begin
        // Oracle 8.0.x
          EnvInitMode := OCI_DEFAULT;
          if {$IFDEF LITE} not OCIMutexed and{$ENDIF} OCIThreaded and (OCIVersion <= 8140) then // supports from OCI 8.1.5
            EnvInitMode := EnvInitMode or OCI_ENV_NO_MUTEX;

          Res := OCIInitialize(InitMode, nil, nil, nil, nil);
          if Res <> OCI_SUCCESS then
            Error(Res);

          Res := OCIEnvInit(hOCIEnv, EnvInitMode, 0, nil);
          if Res <> OCI_SUCCESS then
            Error(Res);
        end;

        Res := OCIHandleAlloc(hOCIEnv, pOCIHandle(hOCIError), OCI_HTYPE_ERROR, 0, nil);
        if Res <> OCI_SUCCESS then
          Error(Res);

        if (SubscriptionPort <> 0) and (OCIVersion >= 10200) then
          Check(OCIAttrSet2(hOCIEnv, OCI_HTYPE_ENV, SubscriptionPort, 0,
            OCI_ATTR_SUBSCR_PORTNO, hOCIError));

        OCIInited := True;
      end;

      if OCI73 in PossibleOCICallStyles then begin
        if OCIThreaded then
          SetThreadSafety(True);

        OCIInited := True;
      end;
    end;
  finally
    LeaveCriticalSection(hOCIInit);
  end;
end;

procedure FinishOCI;
begin
  if OCIInited then
    if OCI80 in PossibleOCICallStyles then begin
      OCIHandleFree(hOCIError, OCI_HTYPE_ERROR);
      OCIHandleFree(hOCIEnv, OCI_HTYPE_ENV);
      hOCIError := nil;
      hOCIEnv := nil;

      OCIInited := False;
    end
end;

procedure CheckOCI;
begin
  if not ((OCICallStyle = OCI73) or (OCICallStyle = OCI80)) then
    RaiseError(SCheckOCI);
end;

procedure CheckOCI73;
begin
  if not (OCICallStyle = OCI73) then
    RaiseError(SCheckOCI73);
end;

procedure CheckOCI80;
begin
  if not (OCICallStyle = OCI80) then
    RaiseError(SCheckOCI80);
end;

procedure CheckOCI81;
begin
  if OCIVersion < 8100 then
    RaiseError(SCheckOCI81);
end;

procedure CheckOCI90;
begin
  if OCIVersion < 9000 then
    RaiseError(SCheckOCI90);
end;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
procedure InitMTS;
{$IFNDEF CLR}
var
  MTSName: string;
{$ENDIF}
begin
  EnterCriticalSection(hMTSInit);
  try
    if MTSInited then
      Exit;

  {$IFNDEF CLR}
    {oramts}
    MTSName := ExtractFileDir(OCIDLL) + '\oramts.dll';

    hMTSLib := LoadLibraryEx(PChar(MTSName), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
    if hMTSLib <> 0 then begin
      OraMTSSvcGet := GetProcAddress(hMTSLib, PChar('OraMTSSvcGet'));
      OraMTSSvcRel := GetProcAddress(hMTSLib, PChar('OraMTSSvcRel'));
      OraMTSEnlCtxGet := GetProcAddress(hMTSLib, PChar('OraMTSEnlCtxGet'));
      OraMTSJoinTxn := GetProcAddress(hMTSLib, PChar('OraMTSJoinTxn'));
      OraMTSEnlCtxRel := GetProcAddress(hMTSLib, PChar('OraMTSEnlCtxRel'));
      OraMTSSvcEnlist := GetProcAddress(hMTSLib, PChar('OraMTSSvcEnlist'));
    end
    else
      raise Exception.Create('Can not find ' + MTSName);

  {$ELSE}
      OraMTSSvcGet := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OraMTSSvcGet;
      OraMTSSvcRel := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OraMTSSvcRel;
      OraMTSEnlCtxGet := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OraMTSEnlCtxGet;
      OraMTSJoinTxn := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OraMTSJoinTxn;
      OraMTSEnlCtxRel := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OraMTSEnlCtxRel;
      OraMTSSvcEnlist := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OraMTSSvcEnlist;
  {$ENDIF}

    InitMSDTC;

    MTSInited := True;
  finally
    LeaveCriticalSection(hMTSInit);
  end;
end;

procedure FreeMTS;
begin
  if not MTSInited then
    Exit;

{$IFNDEF CLR}
  if hMTSLib <> 0 then begin
    FreeLibrary(hMTSLib);
    hMTSLib := 0;
  end;

  FreeMSDTC;
{$ENDIF}

  MTSInited := False;
end;
{$ENDIF}
{$ENDIF}

{ OCI80 }

procedure {$IFNDEF FPC}OraError{$ELSE}DoOraError{$ENDIF}(ErrorCode: sword; UnicodeEnv: boolean; hError: pOCIError = nil);
var
  Msg: _string;
  OrgErrorCode: sword;
begin
  OrgErrorCode := ErrorCode;
  ErrorCode := GetOraError(ErrorCode, UnicodeEnv, Msg, hError);

  if OrgErrorCode <> OCI_SUCCESS_WITH_INFO then
    raise EOraError.Create(Abs(ErrorCode), TrimRight(Msg));
end;

function GetOraError(ErrorCode: sword; UnicodeEnv: boolean; var ErrorMsg: _string; hError: pOCIError = nil): sword;
var
  hMsg: IntPtr;
  St: _string;
  Res: sword;
  BufSize: integer;
begin
  CheckOCI80;

  if hError = nil then
    hError := hOCIError;

  case ErrorCode of
    OCI_SUCCESS: begin
      Result := ErrorCode;
      Exit;
    end;
    OCI_ERROR,
    OCI_SUCCESS_WITH_INFO,
    OCI_NO_DATA:
    begin
      BufSize := OracleErrorMaxLength * SizeOfCharOCI(UnicodeEnv);
      hMsg := Marshal.AllocHGlobal(BufSize);
      try
        Marshal.WriteInt16(hMsg, 0);
        Res := OCIErrorGet(hError, 1, nil, ErrorCode, hMsg, BufSize, OCI_HTYPE_ERROR);
        ErrorMsg := PtrToStringOCI(hMsg, UnicodeEnv);
      finally
        Marshal.FreeHGlobal(hMsg);
      end;

      case Res of
        OCI_SUCCESS:
          if ErrorMsg = '' then
            ErrorMsg := 'ORA-' + IntToStr(ErrorCode);
        OCI_ERROR: begin // For some errors ORACLE BUG
          St := Copy(ErrorMsg, 1, OracleErrorMaxLength);
          if Pos('ORA-', St) = 1 then begin
            St := Copy(St, 5, 5);
            try
              ErrorCode := StrToInt(St);
            except
            end;
          end;
        end;
        OCI_SUCCESS_WITH_INFO:
          ErrorMsg := 'OCI_SUCCESS_WITH_INFO';
        OCI_NO_DATA:
          ErrorMsg := 'OCI_NO_DATA';
      end;
    end;
    OCI_INVALID_HANDLE:
      ErrorMsg := 'OCI_INVALID_HANDLE';
    OCI_NEED_DATA:
      ErrorMsg := 'OCI_NEED_DATA';
  else
    ErrorMsg := 'Unknown error: ORA-' + IntToStr(ErrorCode);
  end;
  ErrorCode := Abs(ErrorCode);
  Result := ErrorCode;
end;

function GetOraError(ErrorCode: sword; UnicodeEnv: boolean; hError: pOCIError = nil): sword;
var
  Msg: _string;
begin
  Result := GetOraError(ErrorCode, UnicodeEnv, Msg, hError);
end;

procedure Check(Status: sword; hError: pOCIError = nil);
begin
  Check(Status, OCIUnicode, hError);
end;

procedure Check(Status: sword; UnicodeEnv: boolean; hError: pOCIError = nil);
begin
  if Status <> OCI_SUCCESS then
    {$IFDEF CLR}Devart.Odac.{$ENDIF}{$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.
    {$IFNDEF FPC}OraError{$ELSE}DoOraError{$ENDIF}(Status, UnicodeEnv, hError);
end;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
procedure MTSError(ErrorCode: sword; UnicodeEnv: boolean; hError: pOCIError = nil);
var
  Msg: _string;
begin
  ErrorCode := GetMTSError(ErrorCode, UnicodeEnv, Msg, hError);
  raise EOraError.Create(Abs(ErrorCode), TrimRight(Msg));
end;

function GetMTSError(ErrorCode: sword; UnicodeEnv: boolean; var ErrorMsg: _string; hError: pOCIError = nil): sword;
begin
  Result := GetOraError(OCI_ERROR, UnicodeEnv, ErrorMsg, hError);
end;

function GetMTSError(ErrorCode: sword; UnicodeEnv: boolean; hError: pOCIError = nil): sword;
var
  Msg: _string;
begin
  Result := GetMTSError(ErrorCode, UnicodeEnv, Msg, hError);
end;

procedure MTSCheck(Status: sword; UnicodeEnv: boolean; hError: pOCIError = nil);
begin
  if Status <> ORAMTSERR_NOERROR then
    {$IFDEF CLR}Devart.Odac.{$ENDIF}{$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.MTSError(Status, UnicodeEnv, hError);
end;
{$ENDIF}
{$ENDIF}

function GetHeapAlloc: integer;
var
  ValuePtr: Integer;
begin
  Check(OCIAttrGet2(hOCIEnv, OCI_HTYPE_ENV, ValuePtr, nil, OCI_ATTR_HEAPALLOC, hOCIError));
  Result := Integer(ValuePtr);
end;

function GetSharedHeapAlloc: integer;
var
  ValuePtr: Integer;
begin
  Check(OCIAttrGet2(hOCIEnv, OCI_HTYPE_ENV, ValuePtr, nil, OCI_ATTR_SHARED_HEAPALLOC, hOCIError));
  Result := Integer(ValuePtr);
end;

function GetSubscriptionPort: integer;
begin
  if (SubscriptionPort = 0) and (hOCIEnv <> nil) then
    Check(OCIAttrGet2(hOCIEnv, OCI_HTYPE_ENV, Result, nil, OCI_ATTR_SUBSCR_PORTNO, hOCIError))
  else
    Result := SubscriptionPort;
end;

{ OCI73 }

function GetThreadSafety: boolean;
begin
  Result := ThreadSafety
end;

procedure SetThreadSafety(Value: boolean);
begin
  if OCI73 in PossibleOCICallStyles then begin
    ThreadSafety := Value;
  {  if ThreadSafety then
      opinit(OCI_EV_TSF)
    else}
      opinit(OCI_EV_DEF); // for nonblock break
  end;
end;

initialization

{$IFDEF MSWINDOWS}
  hOraLib := 0;
  hOraLibClient9 := 0;
{$ENDIF}
{$IFDEF LINUX}
  hOraLib := nil;
  hOraLibClient9 := nil;
{$ENDIF}

{$IFNDEF CLR}
  OCIDLL := '';
  OCIClientDLL := '';

// Oracle 7
  obindps  := @NotLink;
  obndra   := @NotLink;
  obndrn   := @NotLink;
  obndrv   := @NotLink;
  obreak   := @NotLink;
  ocan     := @NotLink;
  oclose   := @NotLink;
  ocof     := @NotLink;
  ocom     := @NotLink;
  ocon     := @NotLink;
  odefin   := @NotLink;
  odefinps := @NotLink;
  odescr   := @NotLink;
  odessp   := @NotLink;
  oerhms   := @NotLink;
  oermsg   := @NotLink;
  oexec    := @NotLink;
  oexfet   := @NotLink;
  oexn     := @NotLink;
  ofen     := @NotLink;
  ofetch   := @NotLink;
  oflng    := @NotLink;
  ognfd    := @NotLink;
  olog     := @NotLink;
  ologof   := @NotLink;
  onbclr   := @NotLink;
  onbset   := @NotLink;
  onbtst   := @NotLink;
  oopen    := @NotLink;
  oopt     := @NotLink;
  opinit   := @NotLink;
  oparse   := @NotLink;
  orol     := @NotLink;
  ogetpi   := @NotLink;
  osetpi   := @NotLink;
  sqllda   := nil;
  sqlld2   := nil;

// Oracle 8
  OCIAttrGet1            := @NotLink;
  OCIAttrGet2            := @NotLink;
  OCIAttrSet1            := @NotLink;
  OCIAttrSet2            := @NotLink;
  //OCIAttrSet3            := @NotLink;
  OCIBindArrayOfStruct   := @NotLink;
  OCIBindByName          := @NotLink;
  OCIBindByPos           := @NotLink;
  OCIBindDynamic         := @NotLink;
  OCIBindObject          := @NotLink;
  OCIBreak               := @NotLink;
  OCIDefineArrayOfStruct := @NotLink;
  OCIDefineByPos         := @NotLink;
  OCIDefineDynamic       := @NotLink;
  OCIDefineObject        := @NotLink;
  OCIDescribeAny         := @NotLink;
  OCIDescriptorAlloc     := @NotLink;
  OCIDescriptorFree      := @NotLink;
  OCIEnvCreate           := @NotLink;
  OCIEnvInit             := @NotLink;
  OCIErrorGet            := @NotLink;
  OCIHandleAlloc         := @NotLink;
  OCIHandleFree          := @NotLink;
  OCIInitialize          := @NotLink;
  OCILdaToSvcCtx         := @NotLink;
  OCIParamGet            := @NotLink;
  OCIPasswordChange      := @NotLink;
  OCIReset               := @NotLink;
  OCIServerAttach        := @NotLink;
  OCIServerDetach        := @NotLink;
  OCIServerVersion       := @NotLink;
  OCISessionBegin        := @NotLink;
  OCISessionEnd          := @NotLink;
  OCISessionGet          := @NotLink;
  OCISessionRelease      := @NotLink;
  OCISessionPoolCreate   := @NotLink;
  OCISessionPoolDestroy  := @NotLink;
  OCITransStart          := @NotLink;
  OCITransRollback       := @NotLink;
  OCITransCommit         := @NotLink;
  OCITransDetach         := @NotLink;
  OCITransPrepare        := @NotLink;
  OCITransForget         := @NotLink;
  OCIStmtExecute         := @NotLink;
  OCIStmtFetch           := @NotLink;
  OCIStmtFetch2          := @NotLink;
  OCIStmtGetPieceInfo    := @NotLink;
  OCIStmtPrepare         := @NotLink;
  OCIStmtPrepare2        := @NotLink;
  OCIStmtRelease         := @NotLink;
  OCIStmtSetPieceInfo    := @NotLink;
  OCISvcCtxToLda         := @NotLink;

  OCILobAppend           := @NotLink;
  OCILobAssign           := @NotLink;
  OCILobCharSetForm      := @NotLink;
  OCILobCharSetId        := @NotLink;
  OCILobCopy             := @NotLink;
  OCILobOpen             := @NotLink;
  OCILobClose            := @NotLink;
  OCILobIsOpen           := @NotLink;
  OCILobCreateTemporary  := @NotLink;
  OCILobFreeTemporary    := @NotLink;
  OCILobIsTemporary      := @NotLink;
  OCILobDisableBuffering := @NotLink;
  OCILobEnableBuffering  := @NotLink;
  OCILobErase            := @NotLink;
  OCILobFileClose        := @NotLink;
  OCILobFileExists       := @NotLink;
  OCILobFileGetName      := @NotLink;
  OCILobFileIsOpen       := @NotLink;
  OCILobFileOpen         := @NotLink;
  OCILobFileSetName      := @NotLink;
  OCILobFlushBuffer      := @NotLink;
  OCILobGetLength        := @NotLink;
  OCILobIsEqual          := @NotLink;
  OCILobLoadFromFile     := @NotLink;
  OCILobLocatorIsInit    := @NotLink;
  OCILobRead             := @NotLink;
  OCILobRead2            := @NotLink;
  OCILobTrim             := @NotLink;
  OCILobWrite            := @NotLink;

  OCICacheFlush          := @NotLink;
  OCICacheFree           := @NotLink;
  OCICacheRefresh        := @NotLink;
  OCICacheUnmark         := @NotLink;
  OCICacheUnpin          := @NotLink;

  OCIObjectCopy          := @NotLink;
  OCIObjectExists        := @NotLink;
  OCIObjectFlush         := @NotLink;
  OCIObjectFree          := @NotLink;
  OCIObjectGetAttr       := @NotLink;
  OCIObjectGetInd        := @NotLink;
  OCIObjectGetObjectRef  := @NotLink;
  OCIObjectGetProperty   := @NotLink;
  OCIObjectGetTypeRef    := @NotLink;
  OCIObjectIsDirty       := @NotLink;
  OCIObjectIsLocked      := @NotLink;
  OCIObjectLock          := @NotLink;
  OCIObjectMarkDelete    := @NotLink;
  OCIObjectMarkDeleteByRef := @NotLink;
  OCIObjectMarkUpdate    := @NotLink;
  OCIObjectNew           := @NotLink;
  OCIObjectPin           := @NotLink;
  OCIObjectPinCountReset := @NotLink;
  OCIObjectPinTable      := @NotLink;
  OCIObjectRefresh       := @NotLink;
  OCIObjectSetAttr       := @NotLink;
  OCIObjectUnmark        := @NotLink;
  OCIObjectUnmarkByRef   := @NotLink;
  OCIObjectUnpin         := @NotLink;
  OCITypeByName          := @NotLink;
  OCITypeByRef           := @NotLink;

  OCICollAppend          := @NotLink;
  OCICollAssign          := @NotLink;
  OCICollAssignElem      := @NotLink;
  OCICollGetElem         := @NotLink;
  OCICollMax             := @NotLink;
  OCICollSize            := @NotLink;
  OCICollTrim            := @NotLink;
  OCIDateAssign          := @NotLink;
  OCIDateFromText        := @NotLink;
  OCIDateGetDate         := @NotLink;
  OCIDateGetTime         := @NotLink;
  OCIDateSetDate         := @NotLink;
  OCIDateSetTime         := @NotLink;
  OCIDateToText          := @NotLink;
  OCINumberAssign        := @NotLink;
  OCINumberCmp           := @NotLink;
  OCINumberFromInt       := @NotLink;
  OCINumberFromReal      := @NotLink;
  OCINumberFromText      := @NotLink;
  OCINumberToInt         := @NotLink;
  OCINumberToReal        := @NotLink;
  OCINumberToText        := @NotLink;
  OCIRefAssign           := @NotLink;
  OCIRefClear            := @NotLink;
  OCIRefIsEqual          := @NotLink;
  OCIRefIsNull           := @NotLink;
  OCIRefToHex            := @NotLink;
  OCIStringAllocSize     := @NotLink;
  OCIStringAssign        := @NotLink;
  OCIStringAssignText    := @NotLink;
  OCIStringPtr           := @NotLink;
  OCIStringResize        := @NotLink;
  OCIStringSize          := @NotLink;
  OCITableDelete         := @NotLink;
  OCITableExists         := @NotLink;
  OCITableFirst          := @NotLink;
  OCITableLast           := @NotLink;
  OCITableNext           := @NotLink;
  OCITablePrev           := @NotLink;
  OCITableSize           := @NotLink;

  OCIDirPathAbort            := @NotLink;
  OCIDirPathColArrayEntryGet := @NotLink;
  OCIDirPathColArrayEntrySet := @NotLink;
  OCIDirPathColArrayRowGet   := @NotLink;
  OCIDirPathColArrayReset    := @NotLink;
  OCIDirPathColArrayToStream := @NotLink;
  OCIDirPathFinish           := @NotLink;
  OCIDirPathLoadStream       := @NotLink;
  OCIDirPathPrepare          := @NotLink;
  OCIDirPathStreamReset      := @NotLink;

  OCIExtProcGetEnv           := @NotLink;
  OCIExtProcAllocCallMemory  := @NotLink;
  OCIExtProcRaiseExcpWithMsg := @NotLink;

  OCISubscriptionRegister    := @NotLink;
  OCISubscriptionUnRegister  := @NotLink;
  OCISubscriptionEnable      := @NotLink;
  OCISubscriptionDisable     := @NotLink;

// Oracle 9
  OCIDateTimeConstruct          := @NotLink;
  OCIDateTimeFromText           := @NotLink;
  OCIDateTimeToText             := @NotLink;
  OCIDateTimeGetDate            := @NotLink;
  OCIDateTimeGetTime            := @NotLink;
  OCIDateTimeGetTimeZoneOffset  := @NotLink;
  OCIDateTimeGetTimeZoneName    := @NotLink;
  OCIDateTimeAssign             := @NotLink;
  OCIDateTimeCompare            := @NotLink;
  OCIIntervalFromText           := @NotLink;
  OCIIntervalToText             := @NotLink;
  OCIIntervalCheck              := @NotLink;
  OCIIntervalAssign             := @NotLink;
  OCIIntervalCompare            := @NotLink;
  OCIIntervalSetYearMonth       := @NotLink;
  OCIIntervalGetYearMonth       := @NotLink;
  OCIIntervalSetDaySecond       := @NotLink;
  OCIIntervalGetDaySecond       := @NotLink;
  OCIIntervalFromNumber         := @NotLink;
  OCIRowidToChar                := @NotLink;

  OCIPing                       := @NotLink;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  OraMTSJoinTxn                 := @NotLink;
  OraMTSEnlCtxGet               := @NotLink;
{$ENDIF}
{$ENDIF}

{$ENDIF}
  OCIVersionSt := '';
  OCIVersion := 0;
  OCICallStyle := None;
  PossibleOCICallStyles := [];
  ObjectVersion := True;
  OCIInited := False;
  OCIFunctionsLinked := False;
{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  MTSInited := False;
{$ENDIF}
{$ENDIF}
  hOCIEnv := nil;
  hOCIError := nil;
  OCIThreaded := True;
  OCIMutexed := True;
  OCIShared := False;
  OCIEvents := True; // need for using subscription
  OCIEventsVersion := 10200; // for change notification
{$IFDEF UNICODE_BUILD}
  OCIUnicode := True;
{$ELSE}
  OCIUnicode := False;
{$ENDIF}
  OracleHomeName := '';
  DefaultOracleHome := -1;
  OracleErrorMaxLength := 1024;
  SubscriptionPort := 0;

  InitializeCriticalSection(hOCIInit);
  InitializeCriticalSection(hMTSInit);

finalization

  FreeOCI;
  DeleteCriticalSection(hOCIInit);
  DeleteCriticalSection(hMTSInit);  
end.




