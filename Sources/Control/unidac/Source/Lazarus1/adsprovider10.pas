unit adsprovider10; 

interface

uses
  AdvantageUniProvider, ADSClassesUni, ADSServicesUni,
  ADSConnectionPoolUni, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('AdvantageUniProvider', @AdvantageUniProvider.Register); 
end; 

initialization
  RegisterPackage('adsprovider10', @Register); 
end.
