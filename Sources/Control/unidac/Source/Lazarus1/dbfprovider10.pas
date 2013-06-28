unit dbfprovider10; 

interface

uses
  DBFUniProvider, DBFClassesUni, DBFServicesUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DBFUniProvider', @DBFUniProvider.Register); 
end; 

initialization
  RegisterPackage('DBFprovider10', @Register); 
end.
