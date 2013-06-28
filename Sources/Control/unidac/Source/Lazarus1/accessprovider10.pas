unit accessprovider10; 

interface

uses
  AccessUniProvider, AccessClassesUni, AccessServicesUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('AccessUniProvider', @AccessUniProvider.Register); 
end; 

initialization
  RegisterPackage('accessprovider10', @Register); 
end.
