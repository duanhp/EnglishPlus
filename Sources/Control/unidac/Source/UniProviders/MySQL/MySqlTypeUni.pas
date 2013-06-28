
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MySqlTypeUni;
{$ENDIF}

interface

type
  TMySqlType = (
    mtBigInt,
    mtBlob,
    mtChar,
    mtDate,
    mtDateTime,
//    mtDecimal,
    mtDouble,
    mtFloat,
    mtInt,
    mtSmallInt,
    mtText,
    mtTime,
//    mtTimeStamp,
    mtTinyInt,
    mtVarChar,
    mtYear
  );

implementation

end.
