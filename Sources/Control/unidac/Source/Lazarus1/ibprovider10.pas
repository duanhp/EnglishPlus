unit ibprovider10; 

interface

uses
  IBCParserUni, IBCConstsUni, IBCClassesUni, IBCCallUni, IBCErrorUni, 
    IBCArrayUni, IBCConnectionPoolUni, IBCServicesUni, IBCScriptProcessorUni, 
    InterBaseUniProvider, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('InterBaseUniProvider', @InterBaseUniProvider.Register); 
end; 

initialization
  RegisterPackage('ibprovider10', @Register); 
end.
