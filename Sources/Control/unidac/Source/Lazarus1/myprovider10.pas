unit myprovider10; 

interface

uses
  MySQLUniProvider, MyClassesUni, MyConnectionPoolUni, MyConstsUni, 
    MyParserUni, MyScriptProcessorUni, MyServicesUni, MySqlApiDirectUni, 
    MySqlApiUni, MySqlBindUni, MySqlErrorsUni, MySqlNetUni, MySqlResultSetUni, 
    MySqlSessionUni, MySqlStmtUni, MySqlTypeUni, MySqlVioPipeUni, 
    MyCallUni, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('MySQLUniProvider', @MySQLUniProvider.Register); 
end; 

initialization
  RegisterPackage('myprovider10', @Register); 
end.
