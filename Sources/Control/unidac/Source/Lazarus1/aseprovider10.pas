unit aseprovider10; 

interface

uses
  ASEUniProvider, ASEParserUni, ASEClassesUni, ASEServicesUni,
  ASEConnectionPoolUni, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ASEUniProvider', @ASEUniProvider.Register); 
end; 

initialization
  RegisterPackage('aseprovider10', @Register); 
end.
