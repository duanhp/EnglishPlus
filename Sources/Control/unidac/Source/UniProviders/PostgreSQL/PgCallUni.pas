
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgCallUni;

{$ENDIF}

interface

uses
  SysUtils
{$IFNDEF CLR}
  , CLRClasses
{$ENDIF};

const

{ PostgerSQL datatypes }

  SQL_BIGINT = 20;
  SQL_BIT = 1560;
  SQL_BOOLEAN = 16;
  SQL_BOX = 603;
  SQL_BYTEA = 17;
  SQL_CHAR = 1042;
  SQL_CIRCLE = 718;
  SQL_DATE = 1082;
  SQL_DOUBLE = 701;
  SQL_INT = 23;
  SQL_INTERVAL = 1186;
  SQL_LINE = 628;
  SQL_LSEG = 601;
  SQL_MONEY = 790;
  SQL_NUMERIC = 1700;
  SQL_PATH = 602;
  SQL_POINT = 600;
  SQL_POLYGON = 604;
  SQL_REAL = 700;
  SQL_SMALLINT = 21;
  SQL_TEXT = 25;
  SQL_TIME = 1083;
  SQL_TIMESTAMP = 1114;
  SQL_TIMESTAMPTZ = 1184;
  SQL_TIMETZ = 1266;
  SQL_VARBIT = 1562;
  SQL_VARCHAR = 1043;
  SQL_ROW = 1;
  SQL_REFCURSOR = 1790;

  SQL_INT_ARRAY = 1007;

{ PostgerSQL internal datatypes }

  SQL_OID = 26;
  SQL_CID = 29;
  SQL_CIDR = 650;
  SQL_OIDVECTOR = 30;
  SQL_INT2VECTOR = 22;
  SQL_NAME = 19;
  SQL_MACADDR = 829;
  SQL_INET = 869;
  SQL_FLOAT4 = 188;
  SQL_CHARACTER = 18;
  SQL_INTERVAL12 = 10000;
  SQL_OIDVECTOR801 = 10001;
  SQL_UNKNOWN = 705;
  SQL_VOID = 2278;
  SQL_UUID = 2950;
  PG_TYPE_RELTYPE_OID = 71;
  PG_ATTRIBUTE_RELTYPE_OID = 75;
  PG_PROC_RELTYPE_OID = 81;
  PG_CLASS_RELTYPE_OID = 83;

  PG_DEFAULT_PORT = 5432;

type
  OID = integer;
  LODescriptor = integer;


  TPgSQLItemDesc = record
    FieldName: AnsiString;
    TableOid: Integer;
    TableCol: Smallint;
    TypeOid: Integer;
    TypeSize: Smallint;
    TypeModifier: Integer;
    FormatCode: Smallint; 
  end;

  TPgSQLItemDescs = array of TPgSQLItemDesc;

implementation

end.
