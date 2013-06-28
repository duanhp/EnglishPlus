
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  UniDAC Design
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}

unit UniDesign;
{$ENDIF}
interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, TypInfo,
{$IFDEF CLR}
  Borland.Vcl.Design.DesignEditors, Borland.Vcl.Design.DesignIntf,
  Borland.Vcl.Design.FldLinks, WinUtils,
{$ELSE}
{$IFDEF FPC}
  PropEdits, ComponentEditors,
{$ELSE}
  DesignIntf, DesignEditors,
  {$IFNDEF BCB}FldLinks, ColnEdit,{$ENDIF}
{$ENDIF}
{$ENDIF}
  MemUtils, DBAccess,
  UniProvider, Uni, UniScript, UniDump, UniLoader, UniAlerter,
  CRDesign, DADesign;


procedure Register;

type
{$IFNDEF FPC}
  TUniConnectionList = class (TDAConnectionList)
  protected
    function GetConnectionType: TCustomDAConnectionClass; override;
  end;

  TUniDesignNotification = class(TDADesignNotification)
  public
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent); override;
    function CreateConnectionList: TDAConnectionList; override;
    function GetConnectionPropertyName: string; override;
  end;
{$ENDIF}

  TUniConnectionEditor = class (TDAConnectionEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniQueryEditor = class (TDAComponentEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniTableEditor = class (TDAComponentEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniStoredProcEditor = class (TDAComponentEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniSQLEditor = class (TDASQLEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniScriptEditor = class (TDAScriptEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniUpdateSQLEditor = class (TDAUpdateSQLEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniConnectionProviderNameEditor = class (TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TSpecificOptionsEditor = class (TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

implementation

uses
{$IFNDEF FPC}
  UniMenu,
{$ENDIF}
  UniDesignUtils, UniConnectionEditor, UniQueryEditor,
  UniSQLEditor, UniTableEditor, UniStoredProcEditor, UniScriptEditor,
  UniUpdateSQLEditor, UniSpecificOptionsEditor;

{$IFNDEF FPC}
var
  Notificator: TUniDesignNotification;
{$ENDIF}

procedure Register;
begin
// Register property editors
  RegisterPropertyEditor(TypeInfo(String), TUniConnection, 'ProviderName',
    TUniConnectionProviderNameEditor);
  RegisterPropertyEditor(TypeInfo(TUniMacros), TUniConnection, 'Macros', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(_TStrings), TUniQuery, 'SQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniQuery, 'SQLDelete', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniQuery, 'SQLInsert', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniQuery, 'SQLLock', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniQuery, 'SQLRefresh', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniQuery, 'SQLUpdate', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TDAParams), TUniTable, 'Params', nil);

  RegisterPropertyEditor(TypeInfo(_TStrings), TUniSQL, 'SQL', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(_TStrings), TUniScript, 'SQL', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(_TStrings), TUniStoredProc, 'SQL', nil);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniStoredProc, 'SQLDelete', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniStoredProc, 'SQLInsert', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniStoredProc, 'SQLRefresh', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniStoredProc, 'SQLUpdate', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniStoredProc, 'SQLLock', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(_TStrings), TUniUpdateSQL, 'InsertSQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniUpdateSQL, 'ModifySQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniUpdateSQL, 'DeleteSQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniUpdateSQL, 'RefreshSQL', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(_TStrings), TUniConnection, 'SpecificOptions', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TCustomUniDataSet, 'SpecificOptions', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniSQL, 'SpecificOptions', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniScript, 'SpecificOptions', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniLoader, 'SpecificOptions', TSpecificOptionsEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniDump, 'SpecificOptions', TSpecificOptionsEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TUniAlerter, 'SpecificOptions', TSpecificOptionsEditor);

// Register component editors
  DARegisterComponentEditor(TUniConnection, TUniConnectionEditor, TUniConnectionEditorForm, TUniDesignUtils);
  // DAComponentEditor is needed to display connection select dialog with Delphi 5
  DARegisterComponentEditor(TUniTransaction, TDAComponentEditor, nil, TUniDesignUtils);
  DARegisterComponentEditor(TUniQuery, TUniQueryEditor, TUniQueryEditorForm, TUniDesignUtils);
  DARegisterComponentEditor(TUniSQL, TUniSQLEditor, TUniSQLEditorForm, TUniDesignUtils);
  DARegisterComponentEditor(TUniTable, TUniTableEditor, TUniTableEditorForm, TUniDesignUtils);
  DARegisterComponentEditor(TUniStoredProc, TUniStoredProcEditor, TUniStoredProcEditorForm, TUniDesignUtils);
  DARegisterComponentEditor(TUniScript, TUniScriptEditor, TUniScriptEditorForm, TUniDesignUtils);
  DARegisterComponentEditor(TUniMetaData, TDAComponentEditor, nil, TUniDesignUtils);
  DARegisterComponentEditor(TUniLoader, TDALoaderEditor, nil, TUniDesignUtils);
  DARegisterComponentEditor(TUniDump, TDAComponentEditor, nil, TUniDesignUtils);
  DARegisterComponentEditor(TUniAlerter, TDAComponentEditor, nil, TUniDesignUtils);
  DARegisterComponentEditor(TUniUpdateSQL, TUniUpdateSQLEditor, TUniUpdateSQLEditorForm, TUniDesignUtils);
{$IFNDEF FPC}
  DARegisterComponentEditor(TUniDataSource, TCRDataSourceEditor, nil, TUniDesignUtils);
{$ENDIF}

{$IFNDEF LINUX}
{$IFNDEF FPC}
  Menu.AddItems({$IFDEF CLR}WinUtils{$ELSE}SysInit{$ENDIF}.HInstance);
{$ENDIF}
{$ENDIF}
end;

{$IFNDEF FPC}

{ TUniConnectionList }

function TUniConnectionList.GetConnectionType: TCustomDAConnectionClass;
begin
  Result := TUniConnection;
end;

{ TUniDesignNotification }

procedure TUniDesignNotification.ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  if (AItem <> nil) and ((AItem is TCustomUniDataSet)
    or (AItem is TUniScript)
    or (AItem is TUniDataSource)
    or (AItem is TUniTransaction)
    or (AItem is TUniSQL)
    or (AItem is TUniMetaData)
    or (AItem is TUniLoader)
    or (AItem is TUniDump)
    or (AItem is TUniAlerter)
    )
  then
    FItem := AItem;
end;

function TUniDesignNotification.CreateConnectionList: TDAConnectionList;
begin
  Result := TUniConnectionList.Create;
end;

function TUniDesignNotification.GetConnectionPropertyName: string;
begin
  if FItem is TUniTransaction then
    Result := 'DefaultConnection'
  else
    Result := 'Connection'
end;
{$ENDIF}

{ Component editors }

procedure TUniConnectionEditor.InitVerbs;
begin
  inherited;

  AddVerb('Connection Editor...', TUniConnectionEditorForm, TUniDesignUtils);
end;

procedure TUniQueryEditor.InitVerbs;
begin
  AddVerb('Fields &Editor...', ShowFieldsEditor);
  AddVerb('UniQuery E&ditor...', TUniQueryEditorForm, TUniDesignUtils);
  AddVerb('Data Editor...', ShowDataEditor);
  inherited;
end;

procedure TUniTableEditor.InitVerbs;
begin
  AddVerb('Fields &Editor...', ShowFieldsEditor);
  AddVerb('UniTable E&ditor...', TUniTableEditorForm, TUniDesignUtils);
  AddVerb('Data Editor...', ShowDataEditor);

  inherited;
end;

procedure TUniStoredProcEditor.InitVerbs;
begin
  AddVerb('Fields &Editor...', ShowFieldsEditor);
  AddVerb('StoredProc E&ditor...', TUniStoredProcEditorForm, TUniDesignUtils);

  inherited;
end;

procedure TUniSQLEditor.InitVerbs;
begin
  inherited;

  AddVerb('UniSQL E&ditor...', TUniSQLEditorForm, TUniDesignUtils);
end;

procedure TUniScriptEditor.InitVerbs;
begin
  inherited;

  AddVerb('UniScript E&ditor...', TUniScriptEditorForm, TUniDesignUtils);
end;

procedure TUniUpdateSQLEditor.InitVerbs;
begin
  inherited;
  AddVerb('UniUpdateSQL E&ditor...', TUniUpdateSQLEditorForm, TUniDesignUtils);
end;

{ TUniConnectionProviderNameEditor }

function TUniConnectionProviderNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TUniConnectionProviderNameEditor.GetValues(Proc: TGetStrProc);
var
  List: TStrings;
  i: integer;
begin
  List := TStringList.Create;
  try
    UniProviders.GetProviderNames(List);
    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;

{ TSpecificOptionsEditor }

function TSpecificOptionsEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TSpecificOptionsEditor.GetValue: string;
begin
  Result := '(TStrings)';
end;

procedure TSpecificOptionsEditor.Edit;
var
  Component: TComponent;
begin
  Component := GetComponent(0) as TComponent;

  TDAComponentEditor.ShowEditorEx(TUniSpecificOptionsEditorForm, TUniDesignUtils, Component, {$IFNDEF FPC}Designer{$ELSE}FindRootDesigner(Component){$ENDIF}, GetName)
end;

initialization
{$IFNDEF FPC}
  Notificator := TUniDesignNotification.Create;
  RegisterDesignNotification(Notificator);
{$ENDIF}

{$IFNDEF FPC}
  UnRegisterDesignNotification(Notificator);
{$ENDIF}

end.
