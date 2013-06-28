unit unidacvcl10; 

interface

uses
  UniDacVcl, UniConnectForm, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('unidacvcl10', @Register); 
end.
