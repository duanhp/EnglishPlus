unit pgprovider10; 

interface

uses
  PostgreSQLUniProvider, PgCallUni, PgClassesUni, PgConnectionPoolUni, 
  PgConstsUni, PgCryptUni, PgErrorUni, PgObjectsUni, PgParserUni, 
  PgScriptProcessorUni, PgServicesUni, PgSQLNetUni, PgSQLProtocolUni, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('PostgreSQLUniProvider', @PostgreSQLUniProvider.Register); 
end; 

initialization
  RegisterPackage('pgprovider10', @Register); 
end.
