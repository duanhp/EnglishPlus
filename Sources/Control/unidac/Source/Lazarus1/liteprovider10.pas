unit liteprovider10; 

interface

uses
  SQLiteUniProvider, LiteCallUni, LiteClassesUni, LiteConstsUni,
  LiteErrorUni, LiteParserUni, LiteServicesUni, LiteCollationUni, LiteFunctionUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('SQLiteUniProvider', @SQLiteUniProvider.Register); 
end; 

initialization
  RegisterPackage('liteprovider10', @Register); 
end.
