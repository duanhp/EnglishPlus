unit dacvcl10; 

interface

uses
  DacVcl, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('dacvcl10', @Register); 
end.
