{$I ..\UniDac.inc}

unit Devart.UniDac.Design.UniReg;

{$R ..\..\Images\Devart.UniDac.Design.CRUniDacHelpItem.bmp}
{$R ..\..\Images\Devart.UniDac.Design.CRUniDacFAQItem.bmp}
{$R ..\..\Images\Devart.UniDac.Design.CRUniDacHomePageItem.bmp}
{$R ..\..\Images\Devart.UniDac.Design.CRUniDacPageItem.bmp}
{$R ..\..\Images\Devart.UniDac.Design.UniDacDBMonitorItem.bmp}

{$IFDEF VER10P}

{$IFNDEF STD}
{$R ..\..\Images\Devart.Dac.CRBatchMove.TCRBatchMove.bmp}
{$R ..\..\Images\Devart.Dac.CRBatchMove.TCRBatchMove16.bmp}
{$R ..\..\Images\Devart.Dac.CRBatchMove.TCRBatchMove32.bmp}
{$ENDIF}

{$R ..\..\Images\Devart.UniDac.Uni.TUniConnection.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniQuery.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniSQL.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniTable.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniStoredProc.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniUpdateSQL.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniTransaction.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniDataSource.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniMetaData.bmp}
{$R ..\..\Images\Devart.UniDac.UniScript.TUniScript.bmp}
{$R ..\..\Images\Devart.UniDac.UniLoader.TUniLoader.bmp}
{$R ..\..\Images\Devart.UniDac.UniDump.TUniDump.bmp}
{$R ..\..\Images\Devart.UniDac.UniAlerter.TUniAlerter.bmp}
{$R ..\..\Images\Devart.UniDac.UniSQLMonitor.TUniSQLMonitor.bmp}
{$R ..\..\Images\Devart.UniDac.UniDacVcl.TUniConnectDialog.bmp}

{$R ..\..\Images\Devart.UniDac.Access.AccessUniProvider.TAccessUniProvider.bmp}
{$R ..\..\Images\Devart.UniDac.Advantage.AdvantageUniProvider.TAdvantageUniProvider.bmp}
{$R ..\..\Images\Devart.UniDac.ASE.ASEUniProvider.TASEUniProvider.bmp}
{$R ..\..\Images\Devart.UniDac.DB2.DB2UniProvider.TDB2UniProvider.bmp}
{$R ..\..\Images\Devart.UniDac.DBF.DBFUniProvider.TDBFUniProvider.bmp}
{$R ..\..\Images\Devart.UniDac.InterBase.InterBaseUniProvider.TInterBaseUniProvider.bmp}
{$R ..\..\Images\Devart.UniDac.MySQL.MySQLUniProvider.TMySQLUniProvider.bmp}
{$R ..\..\Images\Devart.UniDac.ODBC.ODBCUniProvider.TODBCUniProvider.bmp}
{$R ..\..\Images\Devart.UniDac.Oracle.OracleUniProvider.TOracleUniProvider.bmp}
{$R ..\..\Images\Devart.UniDac.PostgreSQL.PostgreSQLUniProvider.TPostgreSQLUniProvider.bmp}
{$R ..\..\Images\Devart.UniDac.SQLite.SQLiteUniProvider.TSQLiteUniProvider.bmp}
{$R ..\..\Images\Devart.UniDac.SQLServer.SQLServerUniProvider.TSQLServerUniProvider.bmp}

{$R ..\..\Images\Devart.UniDac.Uni.TUniConnection16.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniQuery16.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniSQL16.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniTable16.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniStoredProc16.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniUpdateSQL16.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniTransaction16.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniDataSource16.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniMetaData16.bmp}
{$R ..\..\Images\Devart.UniDac.UniScript.TUniScript16.bmp}
{$R ..\..\Images\Devart.UniDac.UniLoader.TUniLoader16.bmp}
{$R ..\..\Images\Devart.UniDac.UniDump.TUniDump16.bmp}
{$R ..\..\Images\Devart.UniDac.UniAlerter.TUniAlerter16.bmp}
{$R ..\..\Images\Devart.UniDac.UniSQLMonitor.TUniSQLMonitor16.bmp}
{$R ..\..\Images\Devart.UniDac.UniDacVcl.TUniConnectDialog16.bmp}

{$R ..\..\Images\Devart.UniDac.Access.AccessUniProvider.TAccessUniProvider16.bmp}
{$R ..\..\Images\Devart.UniDac.Advantage.AdvantageUniProvider.TAdvantageUniProvider16.bmp}
{$R ..\..\Images\Devart.UniDac.ASE.ASEUniProvider.TASEUniProvider16.bmp}
{$R ..\..\Images\Devart.UniDac.DB2.DB2UniProvider.TDB2UniProvider16.bmp}
{$R ..\..\Images\Devart.UniDac.DBF.DBFUniProvider.TDBFUniProvider16.bmp}
{$R ..\..\Images\Devart.UniDac.InterBase.InterBaseUniProvider.TInterBaseUniProvider16.bmp}
{$R ..\..\Images\Devart.UniDac.MySQL.MySQLUniProvider.TMySQLUniProvider16.bmp}
{$R ..\..\Images\Devart.UniDac.ODBC.ODBCUniProvider.TODBCUniProvider16.bmp}
{$R ..\..\Images\Devart.UniDac.Oracle.OracleUniProvider.TOracleUniProvider16.bmp}
{$R ..\..\Images\Devart.UniDac.PostgreSQL.PostgreSQLUniProvider.TPostgreSQLUniProvider16.bmp}
{$R ..\..\Images\Devart.UniDac.SQLite.SQLiteUniProvider.TSQLiteUniProvider16.bmp}
{$R ..\..\Images\Devart.UniDac.SQLServer.SQLServerUniProvider.TSQLServerUniProvider16.bmp}

{$R ..\..\Images\Devart.UniDac.Uni.TUniConnection32.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniQuery32.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniSQL32.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniTable32.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniStoredProc32.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniUpdateSQL32.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniTransaction32.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniDataSource32.bmp}
{$R ..\..\Images\Devart.UniDac.Uni.TUniMetaData32.bmp}
{$R ..\..\Images\Devart.UniDac.UniScript.TUniScript32.bmp}
{$R ..\..\Images\Devart.UniDac.UniLoader.TUniLoader32.bmp}
{$R ..\..\Images\Devart.UniDac.UniDump.TUniDump32.bmp}
{$R ..\..\Images\Devart.UniDac.UniAlerter.TUniAlerter32.bmp}
{$R ..\..\Images\Devart.UniDac.UniSQLMonitor.TUniSQLMonitor32.bmp}
{$R ..\..\Images\Devart.UniDac.UniDacVcl.TUniConnectDialog32.bmp}

{$R ..\..\Images\Devart.UniDac.Access.AccessUniProvider.TAccessUniProvider32.bmp}
{$R ..\..\Images\Devart.UniDac.Advantage.AdvantageUniProvider.TAdvantageUniProvider32.bmp}
{$R ..\..\Images\Devart.UniDac.ASE.ASEUniProvider.TASEUniProvider32.bmp}
{$R ..\..\Images\Devart.UniDac.DB2.DB2UniProvider.TDB2UniProvider32.bmp}
{$R ..\..\Images\Devart.UniDac.DBF.DBFUniProvider.TDBFUniProvider32.bmp}
{$R ..\..\Images\Devart.UniDac.InterBase.InterBaseUniProvider.TInterBaseUniProvider32.bmp}
{$R ..\..\Images\Devart.UniDac.MySQL.MySQLUniProvider.TMySQLUniProvider32.bmp}
{$R ..\..\Images\Devart.UniDac.ODBC.ODBCUniProvider.TODBCUniProvider32.bmp}
{$R ..\..\Images\Devart.UniDac.Oracle.OracleUniProvider.TOracleUniProvider32.bmp}
{$R ..\..\Images\Devart.UniDac.PostgreSQL.PostgreSQLUniProvider.TPostgreSQLUniProvider32.bmp}
{$R ..\..\Images\Devart.UniDac.SQLite.SQLiteUniProvider.TSQLiteUniProvider32.bmp}
{$R ..\..\Images\Devart.UniDac.SQLServer.SQLServerUniProvider.TSQLServerUniProvider32.bmp}

{$ELSE}

{$IFNDEF STD}
{$R ..\..\Images\Delphi8\Devart.Dac.CRBatchMove.TCRBatchMove.bmp}
{$ENDIF}

{$R ..\..\Images\Delphi8\Devart.UniDac.Uni.TUniConnection.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.Uni.TUniQuery.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.Uni.TUniSQL.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.Uni.TUniTable.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.Uni.TUniStoredProc.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.Uni.TUniUpdateSQL.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.Uni.TUniTransaction.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.Uni.TUniDataSource.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.Uni.TUniMetaData.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.UniScript.TUniScript.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.UniLoader.TUniLoader.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.UniDump.TUniDump.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.UniAlerter.TUniAlerter.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.UniSQLMonitor.TUniSQLMonitor.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.UniDacVcl.TUniConnectDialog.bmp}

{$R ..\..\Images\Delphi8\Devart.UniDac.Access.AccessUniProvider.TAccessUniProvider.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.Advantage.AdvantageUniProvider.TAdvantageUniProvider.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.ASE.ASEUniProvider.TASEUniProvider.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.DB2.DB2UniProvider.TDB2UniProvider.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.DBF.DBFUniProvider.TDBFUniProvider.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.InterBase.InterBaseUniProvider.TInterBaseUniProvider.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.MySQL.MySQLUniProvider.TMySQLUniProvider.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.ODBC.ODBCUniProvider.TODBCUniProvider.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.Oracle.OracleUniProvider.TOracleUniProvider.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.PostgreSQL.PostgreSQLUniProvider.TPostgreSQLUniProvider.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.SQLite.SQLiteUniProvider.TSQLiteUniProvider.bmp}
{$R ..\..\Images\Delphi8\Devart.UniDac.SQLServer.SQLServerUniProvider.TSQLServerUniProvider.bmp}

{$ENDIF}

{$I ..\Design\UniReg.pas}
