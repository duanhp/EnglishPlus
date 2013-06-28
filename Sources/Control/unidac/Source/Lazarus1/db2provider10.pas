unit db2provider10; 

interface

uses
  DB2UniProvider, DB2CallUni, DB2ClassesUni, DB2ServicesUni,
  DB2ConnectionPoolUni, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DB2UniProvider', @DB2UniProvider.Register); 
end; 

initialization
  RegisterPackage('db2provider10', @Register); 
end.
