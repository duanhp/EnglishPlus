unit msprovider10; 

interface

uses
  SQLServerUniProvider, OLEDBCUni, MSParserUni, MSConstsUni, OLEDBIntfUni, 
    CRThreadUni, MSUDTUni, MSConnectionPoolUni, MSServicesUni, 
    MSScriptProcessorUni, OLEDBAccessUni, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('SQLServerUniProvider', @SQLServerUniProvider.Register); 
end; 

initialization
  RegisterPackage('msprovider10', @Register); 
end.
