unit oraprovider10; 

interface

uses
  OraCallUni, OraParserUni, OraClassesUni, OraConnectionPoolUni, OraObjectsUni, 
    OraConstsUni, OraErrorUni, OraServicesUni, OraScriptProcessorUni, 
    OracleUniProvider, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('OracleUniProvider', @OracleUniProvider.Register); 
end; 

initialization
  RegisterPackage('oraprovider10', @Register); 
end.
