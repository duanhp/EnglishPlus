unit odbcprovider10; 

interface

uses
  ODBCUniProvider, ODBCCallUni, ODBCClassesUni, ODBCConstsUni,
  ODBCErrorUni, ODBCParserUni, ODBCServicesUni, ODBCConnectionPoolUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ODBCUniProvider', @ODBCUniProvider.Register); 
end; 

initialization
  RegisterPackage('odbcprovider10', @Register); 
end.
