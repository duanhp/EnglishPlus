program UniDacDemo_VclNet;

{$I Base\DacDemo.inc}

{%DelphiDotNetAssemblyCompiler 'System.Drawing.dll'}
{%DelphiDotNetAssemblyCompiler 'Borland.VclDbRtl.dll'}
{%DelphiDotNetAssemblyCompiler 'Borland.Delphi.dll'}
{%DelphiDotNetAssemblyCompiler 'Borland.VclRtl.dll'}
{%DelphiDotNetAssemblyCompiler 'Borland.Vcl.dll'}
{%DelphiDotNetAssemblyCompiler 'Devart.Dac.dll'}
{%DelphiDotNetAssemblyCompiler 'Devart.UniDac.dll'}

uses
  Forms,
  CategoryFrame in 'Base\CategoryFrame.pas' {CategoryFrame},
  DemoBase in 'Base\DemoBase.pas',
  DemoForm in 'Base\DemoForm.pas' {DemoForm},
  DemoFrame in 'Base\DemoFrame.pas' {DemoFrame},
  HtmlConsts in 'Base\HtmlConsts.pas',
  UniDacDemoForm in 'Base\UniDacDemoForm.pas' {UniDACForm},
  UniDacAbout in 'Base\UniDacAbout.pas' {UniDacAboutForm},
  ParamType in 'Base\ParamType.pas' {ParamTypeForm},
  CachedUpdates in 'CachedUpdates\CachedUpdates.pas' {CachedUpdatesFrame: TFrame},
  ConnectDialog in 'ConnectDialog\ConnectDialog.pas' {ConnectDialogFrame: TFrame},
  CRDBGrid in 'CRDBGrid\CRDBGrid.pas' {CRDBGridFrame: TFrame},
  Dump in 'Dump\Dump.pas' {DumpFrame: TFrame},
  Fetch in 'Loader\Fetch.pas' {TFetchForm},
  FilterAndIndex in 'FilterAndIndex\FilterAndIndex.pas' {FilterAndIndexFrame: TFrame},
  Loader in 'Loader\Loader.pas' {LoaderFrame: TFrame},
  Macros in 'Macros\Macros.pas' {MacrosFrame: TFrame},
  MasterDetail in 'MasterDetail\MasterDetail.pas' {MasterDetailFrame: TFrame},
  Query in 'Query\Query.pas' {QueryFrame: TFrame},
  Pictures in 'Pictures\Pictures.pas' {PicturesFrame: TFrame},
  Sql in 'Sql\Sql.pas' {SqlFrame: TFrame},
  StoredProc in 'StoredProc\StoredProc.pas' {StoredProcFrame: TFrame},
  Table in 'Table\Table.pas' {TableFrame: TFrame},
  Text in 'Text\Text.pas' {TextFrame: TFrame},
  UpdateSQL in 'UpdateSQL\UpdateSQL.pas' {UpdateSQLFrame: TFrame},
  UpdateAction in 'CachedUpdates\UpdateAction.pas' {UpdateActionForm},
  VTable in 'VirtualTable\VTable.pas';

{$R *.RES}

[STAThread]
begin
  Application.Initialize;
  Application.CreateForm(TUniDACForm, UniDACForm);
  Application.CreateForm(TUpdateActionForm, UpdateActionForm);
  Application.CreateForm(TUniDacAboutForm, UniDacAboutForm);
  Application.Run;
end.
