program UniDacDemo;

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  CategoryFrame in '..\..\..\..\Common\Demos\Win32\DacDemo\Base\CategoryFrame.pas' {CategoryFrame},
  DemoBase in '..\..\..\..\Common\Demos\Win32\DacDemo\Base\DemoBase.pas',
  DemoForm in '..\..\..\..\Common\Demos\Win32\DacDemo\Base\DemoForm.pas' {DemoForm},
  DemoFrame in '..\..\..\..\Common\Demos\Win32\DacDemo\Base\DemoFrame.pas' {DemoFrame},
  HtmlConsts in '..\..\..\..\Common\Demos\Win32\DacDemo\Base\HtmlConsts.pas',
  UniDacDemoForm in 'Base\UniDacDemoForm.pas' {UniDACForm},
  UniDacAbout in 'Base\UniDacAbout.pas' {UniDacAboutForm},
  ParamType in 'Base\ParamType.pas' {ParamTypeForm},
  CachedUpdates in 'CachedUpdates\CachedUpdates.pas' {CachedUpdatesFrame: TFrame},
  ConnectDialog in 'ConnectDialog\ConnectDialog.pas' {ConnectDialogFrame: TFrame},
  Dump in 'Dump\Dump.pas' {DumpFrame: TFrame},
  Fetch in 'Loader\Fetch.pas' {TFetchForm},
  FilterAndIndex in 'FilterAndIndex\FilterAndIndex.pas' {FilterAndIndexFrame: TFrame},
  Loader in 'Loader\Loader.pas' {LoaderFrame: TFrame},
  MasterDetail in 'MasterDetail\MasterDetail.pas' {MasterDetailFrame: TFrame},
  Query in 'Query\Query.pas' {QueryFrame: TFrame},
  Sql in 'Sql\Sql.pas' {SqlFrame: TFrame},
  StoredProc in 'StoredProc\StoredProc.pas' {StoredProcFrame: TFrame},
  Table in 'Table\Table.pas' {TableFrame: TFrame},
  Text in 'Text\Text.pas' {TextFrame: TFrame},
  UpdateSQL in 'UpdateSQL\UpdateSQL.pas' {UpdateSQLFrame: TFrame},
  UpdateAction in 'CachedUpdates\UpdateAction.pas' {UpdateActionForm},
  VTable in '..\..\..\..\Common\Demos\Win32\DacDemo\VirtualTable\VTable.pas';

begin
  Application.Initialize;
  Application.CreateForm(TUniDACForm, UniDACForm);
  Application.CreateForm(TUpdateActionForm, UpdateActionForm);
  Application.CreateForm(TUniDacAboutForm, UniDacAboutForm);
  Application.Run;
end.
