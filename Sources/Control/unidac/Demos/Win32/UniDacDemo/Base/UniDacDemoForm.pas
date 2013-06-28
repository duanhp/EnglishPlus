{$I DacDemo.inc}

unit UniDacDemoForm;

interface

uses
{$IFNDEF LINUX}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DemoForm, Menus, ImgList, StdCtrls, Buttons, ExtCtrls, ComCtrls, ToolWin,
  DBAccess, UniDacVcl, Db, Uni, DAScript, UniScript,
{$IFDEF FPC}
  LResources,
{$ENDIF}
{$IFNDEF CLR}
  OracleUniProvider,
{$IFNDEF LINUX}
  SQLServerUniProvider,
{$ENDIF}
  InterBaseUniProvider,
  MySQLUniProvider,
  PostgreSQLUniProvider,
  SQLiteUniProvider
{$IFNDEF LINUX}
  ,ODBCUniProvider,
  DB2UniProvider,
  AccessUniProvider,
  AdvantageUniProvider,
  ASEUniProvider
{$ENDIF}
{$ELSE}
  System.ComponentModel,
  Devart.UniDac.Oracle.OracleUniProvider,
  Devart.UniDac.SQLServer.SQLServerUniProvider,
  Devart.UniDac.InterBase.InterBaseUniProvider,
  Devart.UniDac.MySQL.MySQLUniProvider,
  Devart.UniDac.PostgreSQL.PostgreSQLUniProvider,
  Devart.UniDac.SQLite.SQLiteUniProvider,
  Devart.UniDac.ODBC.ODBCUniProvider,
  Devart.UniDac.DB2.DB2UniProvider,
  Devart.UniDac.Access.AccessUniProvider,
  Devart.UniDac.Advantage.AdvantageUniProvider,
  Devart.UniDac.ASE.ASEUniProvider
{$ENDIF}
  ;

type
  TUniDACForm = class(TDemoForm)
    UniConnection: TUniConnection;
    UniConnectDialog: TUniConnectDialog;
    scCreate_InterBase: TUniScript;
    scCreate_Oracle: TUniScript;
    scDrop: TUniScript;
    scCreate_SQLServer: TUniScript;
    scDrop_InterBase: TUniScript;
    scCreate_MySQL: TUniScript;
    scCreate_PgSQL: TUniScript;
    scDrop_PgSQL: TUniScript;
    scCreate_SQLite: TUniScript;
    scDrop_SQLite: TUniScript;
    scCreate_DB2: TUniScript;
    scCreate_Access: TUniScript;
    scCreate_Adv: TUniScript;
    scCreate_ASE: TUniScript;
    scDrop_ASE: TUniScript;
    procedure cbDebugClick(Sender: TObject);
    procedure lbAboutClick(Sender: TObject); override;
  private
    { Private declarations }
  protected
    function GetConnection: TCustomDAConnection; override;
    function ApplicationTitle: string; override;
    function ProductName: string; override;
    procedure RegisterDemos; override;
  public
    function ProductColor: TColor; override;
    procedure ExecCreateScript; override;
    procedure ExecDropScript; override;
  end;

var
  UniDACForm: TUniDACForm;

implementation

uses
  UniDacAbout,
{$IFDEF CRDBGRID}
  CRDBGrid,
{$ENDIF}
{$IFNDEF FPC}
  Macros, Pictures,
{$ENDIF}
  CachedUpdates, ConnectDialog, Dump, FilterAndIndex, Loader, MasterDetail,
  Query, Sql, StoredProc, Table, Text, UpdateSQL, VTable;

{$IFNDEF FPC}
{$IFDEF CLR}
{$R *.nfm}
{$ENDIF}
{$IFDEF WIN32}
{$R *.dfm}
{$ENDIF}
{$IFDEF WIN64}
{$R *.dfm}
{$ENDIF}
{$ENDIF}

function TUniDACForm.GetConnection: TCustomDAConnection;
begin
  Result := UniConnection;
end;

function TUniDACForm.ProductColor: TColor;
begin
  Result := $00FF0000;
end;

procedure TUniDACForm.ExecCreateScript;
begin
  if UniConnection.ProviderName = 'Oracle' then
    scCreate_Oracle.Execute
  else
  if UniConnection.ProviderName = 'InterBase' then
    scCreate_InterBase.Execute
  else
  if UniConnection.ProviderName = 'SQL Server' then
    scCreate_SQLServer.Execute
  else
  if UniConnection.ProviderName = 'MySQL' then
    scCreate_MySQL.Execute
  else
  if UniConnection.ProviderName = 'PostgreSQL' then
    scCreate_PgSQL.Execute
  else
  if (UniConnection.ProviderName = 'SQLite') or (UniConnection.ProviderName = 'ODBC') then
    scCreate_SQLite.Execute
  else
  if UniConnection.ProviderName = 'DB2' then
    scCreate_DB2.Execute
  else
  if UniConnection.ProviderName = 'Access' then
    scCreate_Access.Execute
  else
  if UniConnection.ProviderName = 'Advantage' then
    scCreate_Adv.Execute
  else
  if UniConnection.ProviderName = 'ASE' then
    scCreate_ASE.Execute
end;

procedure TUniDACForm.ExecDropScript;
begin
  if UniConnection.ProviderName = 'InterBase' then
    scDrop_InterBase.Execute
  else
  if UniConnection.ProviderName = 'PostgreSQL' then
    scDrop_PgSQL.Execute
  else
  if (UniConnection.ProviderName = 'SQLite') or (UniConnection.ProviderName = 'ODBC')
    or (UniConnection.ProviderName = 'Access')
  then
    scDrop_SQLite.Execute
  else
  if UniConnection.ProviderName = 'ASE' then
    scDrop_ASE.Execute
  else
    scDrop.Execute
end;

function TUniDACForm.ApplicationTitle: string;
begin
  Result := 'Universal Data Access Components demos';
end;

function TUniDACForm.ProductName: string;
begin
  Result := 'UniDAC';
end;

procedure TUniDACForm.RegisterDemos;
begin
  Demos.RegisterCategory('UniDAC Demo', 'UniDAC Demo');

  Demos.RegisterCategory('Working with components', 'Working with components'); // peculiarities of TreeView under Kilyx
  Demos.RegisterDemo('ConnectDialog', 'Customizing login dialog', 'Demonstrates how to customize the UniDAC connect dialog. Changes the standard UniDAC connect dialog to two custom connect dialogs.  ' + ' The first customized sample dialog is inherited from the TForm class, and the second one is inherited from the default UniDAC connect dialog class.', 'Working with components', TConnectDialogFrame, 8);
{$IFDEF CRDBGRID}
  Demos.RegisterDemo('CRDBGrid', 'Using TCRDBGrid component', 'Demonstrates how to work with the TCRDBGrid component. Shows off the main TCRDBGrid features, like filtering, searching, stretching, using compound headers, and more.', 'Working with components',  TCRDBGridFrame, 1);
{$ENDIF}
  Demos.RegisterDemo('Dump', 'Using TUniDump component', 'Demonstrates how to backup data from tables with the TUniDump component. Shows how to use scripts created during back up to restore table data. ' + 'This demo lets you back up a table either by specifying the table name or by writing a SELECT query.', 'Working with components', TDumpFrame, 9);
  Demos.RegisterDemo('Loader', 'Using TUniLoader component', 'Uses the TUniLoader component to quickly load data into a server table. This demo also compares the two TUniLoader data loading handlers: GetColumnData and PutData.', 'Working with components', TLoaderFrame, 10);
  Demos.RegisterDemo('Query', 'Using TUniQuery component', 'Demonstrates working with TUniQuery, which is one of the most useful UniDAC components. Includes many TUniQuery usage scenarios. Demonstrates how to execute queries in both standard and NonBlocking mode' +
    ' and how to edit data and export it to XML files. Note: This is a very good introductory demo. We recommend starting here when first becoming familiar with UniDAC.', 'Working with components', TQueryFrame, 2);
  Demos.RegisterDemo('Sql', 'Using TUniSQL component', 'Uses TUniSQL to execute SQL statements, and generate stored procedures calls and execute them. Demonstrates how to work with parameters in SQL.', 'Working with components', TSqlFrame, 3);
  Demos.RegisterDemo('StoredProc', 'Using TUniStoredProc component', 'Demonstrates working with the TUniStoredProc component. Lets the user generate and invoke different types of stored procedure calls. Users can choose the type of procedure to be generated, ' + 'specify parameter types, and execute the procedure with different parameter values. Shows how to use the TUniStoredProc object to get a DataSet from a stored procedure that returns a record set.', 'Working with components', TStoredProcFrame, 4);
  Demos.RegisterDemo('Table', 'Using TUniTable component', 'Demonstrates how to use TUniTable to work with data from a single table on the server without manually writing any SQL queries. Performs server-side data sorting and filtering and retrieves results for browsing and editing.', 'Working with components', TTableFrame, 5);
  Demos.RegisterDemo('UpdateSQL', 'Using TUniUpdateSQL component', 'Demonstrates using the TUniUpdateSQL component to customize update commands. Lets you optionally use TUniQuery objects for carrying out insert, delete, query, and update commands.', 'Working with components', TUpdateSQLFrame, 6);
  Demos.RegisterDemo('VirtualTable', 'Using TVirtualTable component', 'Demonstrates working with the TVirtualTable component. This sample shows how to fill virtual dataset with data from other datasets, filter data by a given criteria, locate specified records, perform file operations, and change data and table structure.', 'Working with components', TVirtualTableFrame, 7, 'VTable');

  Demos.RegisterCategory('General demos', 'General demos');
  Demos.RegisterDemo('CachedUpdates', 'Cached updates, transaction control', 'Demonstrates how to perform the most important tasks of working with data in CachedUpdates mode, including highlighting uncommitted changes, managing transactions, and committing changes in a batch.', 'General demos', TCachedUpdatesFrame, 1);
  Demos.RegisterDemo('FilterAndIndex', 'Using Filter and IndexFieldNames', 'Demonstrates UniDAC''s local storage functionality. This sample shows how to perform local filtering, sorting and locating by multiple fields, including by calculated and lookup fields.', 'General demos', TFilterAndIndexFrame, 1);
{$IFNDEF FPC}
  Demos.RegisterDemo('Macros', 'Working with universal macros', 'Demonstrates how to use universal macros to write SQL statements that adapts to any database server.', 'General demos', TMacrosFrame, 1);
{$ENDIF}
  Demos.RegisterDemo('MasterDetail', 'Master/detail relationship', 'Uses UniDAC functionality to work with master/detail relationships. This sample shows how to use local master/detail functionality. Demonstrates different kinds of master/detail linking, inluding linking by SQL, by simple fields, and by calculated fields.', 'General demos', TMasterDetailFrame, 1);
{$IFNDEF FPC}
  Demos.RegisterDemo('Pictures', 'Working with BLOB field', 'Uses UniDAC functionality to work with graphics. The sample demonstrates how to retrieve binary data from database and display it on visual components. Sample also shows how to load and save pictures to files and to the database.', 'General demos', TPicturesFrame, 1);
{$ENDIF}
  Demos.RegisterDemo('Text', 'Working with the Text fields', 'Uses UniDAC functionality to work with text. The sample demonstrates how to retrieve text data from database and display it on visual components. Sample also shows how to load and save text to files and to the database.', 'General demos', TTextFrame, 1);

// Registering Supplementary Demo Projects

  Demos.RegisterCategory('Miscellaneous', '', -1, True);
{$IFDEF CLR}
  Demos.RegisterDemo('AspNet', 'An AspNet application', 'Uses UniDataAdapter to create a simple ASP .NET application.  This demo creates an ASP.NET application that lets you connect to a database and execute queries.' + '  Shows how to display query results in a DataGrid and send user changes back to the database.', 'Miscellaneous', nil, 1, '', True);
  Demos.RegisterDemo('WinForms', 'A WinForms application', 'Shows how to use UniDAC to create a  WinForm application.  This demo project creates a simple WinForms application and fills a data grid from an UniDataAdapter data source.', 'Miscellaneous', nil, 1, '', True);
{$ELSE}
  Demos.RegisterDemo('CBuilder', 'A demo for C++Builder', 'General demo project that shows how to create UniDAC-based applications with C++Builder. Lets you execute SQL scripts and work with result sets in a grid. This is one of the two UniDAC demos for C++Builder.', 'Miscellaneous', nil, 1, '', True);
  Demos.RegisterDemo('Dll', 'Working with DLLs', 'Demonstrates creating and loading DLLs for UniDAC-based projects. This demo project consists of two parts - an UniDll project that creates a DLL of a form that sends a query to the server and displays ' + 'its results, and an UniExe project that can be executed to display a form for loading and running this DLL.  Allows you to build a dll for one UniDAC-based project and load and test it from a separate application.', 'Miscellaneous', nil, 1, '', True);
  Demos.RegisterDemo('VirtualTableCB', 'Using VirtualTable in C++Builder', 'Demonstrates working with the TVirtualTable component. This sample shows how to fill virtual dataset with data from other datasets, filter data by a given criteria, locate specified ' + 'records, perform file operations, and change data and table structure. This is one of the two demo projects for C++Builder', 'Miscellaneous', nil, 1, '', True);
  Demos.RegisterCategory('', '', -1, True);
{$ENDIF}


(*
  Demos.RegisterCategory('TechnologySpecific', '', -1, True);
{$IFNDEF CLR}
  Demos.RegisterCategory('', '', -1, True);
{$ENDIF}
*)

{$IFNDEF CLR}
  Demos.RegisterCategory('ThirdParty', '', -1, True);
  Demos.RegisterDemo('FastReport', 'FastReport hint', 'Demonstrates how UniDAC can be used with FastReport components. This project consists of two parts. The first part is several packages that integrate UniDAC components into the FastReport editor.' + ' The second part is a demo application that lets you design and preview reports with UniDAC technology in the FastReport editor.', 'ThirdParty', nil, 1, '', True);
  Demos.RegisterCategory('', '', -1, True);
{$ENDIF}
end;

procedure TUniDACForm.cbDebugClick(Sender: TObject);
begin
  inherited;
  scCreate_Oracle.Debug := cbDebug.Checked;
  scCreate_InterBase.Debug := cbDebug.Checked;
  scCreate_SQLServer.Debug := cbDebug.Checked;
  scCreate_MySQL.Debug := cbDebug.Checked;
  scCreate_PgSQL.Debug := cbDebug.Checked;
  scCreate_SQLite.Debug := cbDebug.Checked;
  scCreate_DB2.Debug := cbDebug.Checked;
  scCreate_Access.Debug := cbDebug.Checked;
  scCreate_Adv.Debug := cbDebug.Checked;
  scCreate_ASE.Debug := cbDebug.Checked;

  scDrop.Debug := cbDebug.Checked;
  scDrop_InterBase.Debug := cbDebug.Checked;
  scDrop_PgSQL.Debug := cbDebug.Checked;
  scDrop_SQLite.Debug := cbDebug.Checked;
  scDrop_ASE.Debug := cbDebug.Checked;
end;

procedure TUniDACForm.lbAboutClick(Sender: TObject);
begin
  inherited;
  UniDacAboutForm.ShowModal;
end;

{$IFDEF FPC}
initialization
  {$i UniDacDemoForm.lrs}
{$ENDIF}

end.
