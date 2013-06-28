{$I DacDemo.inc}

unit DemoForm;

interface

uses
{$IFNDEF WEB}
  Types,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows, Messages, ShellAPI,
{$ENDIF}
  SysUtils, Classes, DB,
{$IFDEF KYLIX}
  QControls, QStdCtrls, QComCtrls, QGraphics, QMenus, QTypes, QImgList, QForms,
  QButtons, QExtCtrls, Qt, QDialogs,
{$ELSE}
  Forms, Controls, StdCtrls, Graphics, ImgList, ToolWin, ComCtrls, Dialogs,
  ExtCtrls, Menus, DBCtrls, Buttons,
  {$IFNDEF FPC}Tabs,{$ENDIF}
{$IFNDEF VER130}
  Variants,
{$ENDIF}
{$ENDIF}
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  MemUtils, DBAccess,
  DAScript,
  DemoBase,
  DemoFrame,
  CategoryFrame
  {$IFDEF XPMAN}{$IFNDEF FPC}, UxTheme{$ELSE}, Win32UxTheme{$ENDIF}{$ENDIF}
  {$IFDEF USE_SYNEDIT}, SynMemo, SynEdit, SynEditHighlighter, SynHighlighterPas{$ENDIF}
  ;
const
  MAX_HISTORY_SIZE = 6;

type
  TDemoForm = class(TForm)
    StatusBar: TStatusBar;
    ImageList1: TImageList;
    PanelUnderTree: TPanel;
    TreeView: TTreeView;
    MainPanel: TPanel;
    Shape1: TShape;
    TVSplitter: TSplitter;
    pnTopLabel: TPanel;
    lbTitle: TLabel;
    lbAbout: TLabel;
    Panel2: TPanel;
    Panel1: TPanel;
    sbConnect: TSpeedButton;
    sbDisconnect: TSpeedButton;
    btCreate: TSpeedButton;
    btDrop: TSpeedButton;
    cbDebug: TCheckBox;
    pnSource: TPanel;
    pnDemo: TPanel;
    sbDemo: TSpeedButton;
    pnShowSource: TPanel;
    ToolBar: TToolBar;
    tbBrowseBack: TToolButton;
    tbBrowseForward: TToolButton;
    BackHistoryPopup: TPopupMenu;
    ForwardHistoryPopup: TPopupMenu;
    ilButtons: TImageList;
    ilDisabledButtons: TImageList;
    sbSource: TSpeedButton;
    sbFormText: TSpeedButton;
    pnOpenDemoDir: TPanel;
    sbOpenDemoDir: TSpeedButton;

    procedure FormCreate(Sender: TObject); virtual;
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TVSplitterMoved(Sender: TObject);
    procedure TVSplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure lbAboutClick(Sender: TObject); virtual;
    procedure lbAboutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbTitleMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure cbDebugClick(Sender: TObject);
    procedure sbOpenDemoDirClick(Sender: TObject);
    procedure sbConnectClick(Sender: TObject);
    procedure sbDisconnectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HistoryItemClick(Sender: TObject);
    procedure sbDemoClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BackHistoryPopupPopup(Sender: TObject);
    procedure ForwardHistoryPopupPopup(Sender: TObject);
    procedure tbBrowseBackClick(Sender: TObject);
    procedure tbBrowseForwardClick(Sender: TObject);
    procedure btScriptClick(Sender: TObject);
    procedure OnScriptError(Sender: TObject; E: Exception; SQL: _string;
      var Action: TErrorAction);
    procedure sbSourceClick(Sender: TObject);
    procedure sbFormTextClick(Sender: TObject);
  protected
    Demos: TDemos;
    ActiveNode: TTreeNode;
    //History
    History: Array of integer;                                          //Absolute indexes in TreeView
    HistoryIndex: integer;                                              //Current History index
    HistoryEnd: integer;
    IgnoreScriptErrors: boolean;
    DropScriptActive: boolean;
    DemoSourceLoaded, FormSourceLoaded: boolean;

    //Product customization
    function GetConnection: TCustomDAConnection; virtual; abstract; //This function should return DAC product specific connection (i.e. OraSession, MyConnection)
    function ApplicationTitle: string; virtual; abstract; //This function should return DAC product specific title
    function ProductName: string; virtual; abstract; ////This function should return product name
    procedure RegisterDemos; virtual; abstract;          //This procedure should regiter DAC product specific demos
    //XP manifest
  {$IFDEF XPMAN}
    procedure ReplaceFlatStyle(Control: TWinControl; Flat: boolean);
  {$ENDIF}
    //Demo selection
    procedure InitializeDemoFrame(Frame: TDemoFrame; DemoType: TDemoType); virtual;
    procedure UpdateDemo;
    procedure ShowDemo;
    procedure ShowDemoSource;
    procedure ShowFormSource;
  {$IFNDEF WEB}
    procedure OnNavigate(DemoDescription: string);
  {$ELSE}
    procedure OnNavigate(Index: integer);
  {$ENDIF}
    //History
    procedure SelectDemo;
    procedure NavigateHistory(Offset: integer);
    procedure GetBackHistory(BackList: TStrings);
    procedure GetForwardHistory(ForwardList: TStrings);
    procedure DisableBrowse(Back, Forward: boolean);
    procedure FillHistoryPopup(BackHistory: boolean);
    //Connection
    procedure AfterConnect(Sender: TObject);
    procedure AfterDisconnect(Sender: TObject);
  public
{$IFDEF USE_SYNEDIT}
    SourceBrowser: TSynMemo;
{$ELSE}
    SourceBrowser: TMemo;
{$ENDIF}
    function GetIsXPMan: boolean;
    function ProductColor: TColor; virtual; abstract;     //This function should return DAC product specific color
    procedure ExecCreateScript; virtual; abstract;
    procedure ExecDropScript; virtual; abstract;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF CLR}
{$R *.nfm}
{$ENDIF}
{$IFDEF WIN32_64}
{$R *.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}
{$ENDIF}

{$IFDEF XPMAN}
  {$R WindowsXP.res}
{$ENDIF}

procedure TDemoForm.FormCreate(Sender: TObject);
begin
  SetLength(History, MAX_HISTORY_SIZE);
  HistoryIndex := -1;
  Demos := TDemos.Create(TreeView.Items);
  RegisterDemos;
{$IFDEF XPMAN}
  if GetIsXPMan then begin
    ReplaceFlatStyle(Self, False);
    pnTopLabel.Color := ProductColor;
  end;
{$ENDIF}

{$IFDEF USE_SYNEDIT}
  SourceBrowser := TSynMemo.Create(pnSource);

  SourceBrowser.Highlighter := TSynPasSyn.Create(SourceBrowser);
  SourceBrowser.Options := [eoAltSetsColumnMode, eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDisableScrollArrows, eoDragDropEditing, eoDropFiles, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces];
  with SourceBrowser.Gutter do begin
    Visible := True;
    AutoSize := True;
    DigitCount := 3;
    LeftOffset := 0;
    RightOffset := 0;
    ShowLineNumbers := True;
    Width := 1;
 end;
{$ELSE}
  SourceBrowser := TMemo.Create(pnSource);
  SourceBrowser.ScrollBars := ssVertical;
  SourceBrowser.Font.Name := 'Courier New';
  SourceBrowser.Font.Size := 10;
{$ENDIF}
  with SourceBrowser do begin
    Parent := pnSource;
    Align := alClient;
    ReadOnly := True;
  end;
  Resize;
  TreeView.Items[0].Expand(True);
  TreeView.Items[0].Selected := True;
  with GetConnection do begin
    AfterConnect := Self.AfterConnect;
    AfterDisconnect :=  Self.AfterDisconnect;
  end;
  SelectDemo;
end;

procedure TDemoForm.FormDestroy(Sender: TObject);
begin
  with GetConnection do begin
    AfterConnect := nil;
    AfterDisconnect :=  nil;
  end;
  SetLength(History, 0);
  Demos.Free;
end;

//TreeView routines
procedure TDemoForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if (TreeView.Selected.Data <> nil) then
    StatusBar.Panels[0].Text := TDemo(TreeView.Selected.Data).Hint
  else
    StatusBar.Panels[0].Text := TreeView.Selected.Text;
  StatusBar.Repaint;
end;

procedure TDemoForm.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = {$IFDEF LINUX}4100{$ELSE}13{$ENDIF} then
    SelectDemo;
end;

procedure TDemoForm.TreeViewClick(Sender: TObject);
begin
  SelectDemo;
end;

//TreeView Hints
procedure TDemoForm.TreeViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  Node := TreeView.GetNodeAt(X, Y);
  if (Node <> nil) and (Node.Data <> nil) then
    TreeView.Hint := TDemo(Node.Data).Hint
  else
    TreeView.Hint := '';
end;

//Sizing constraints
procedure TDemoForm.TVSplitterMoved(Sender: TObject);
begin
  if PanelUnderTree.ClientWidth = 0 then
    PanelUnderTree.ClientWidth := 150;
end;

procedure TDemoForm.TVSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := (NewSize >= PanelUnderTree.Constraints.MinWidth) and
    ((ClientWidth - MainPanel.Constraints.MinWidth - NewSize) > 0);
end;

function TDemoForm.GetIsXPMan: boolean;
begin
  Result := {$IFDEF XPMAN}UseThemes; {$ELSE} False;{$ENDIF}
end;

{$IFDEF XPMAN}
procedure TDemoForm.ReplaceFlatStyle(Control: TWinControl; Flat: boolean);
var
  i: integer;
begin
  for i := 0 to Control.ControlCount - 1 do
    if Control.Controls[i] is TSpeedButton then
      TSpeedButton(Control.Controls[i]).Flat := Flat
    else
    if Control.Controls[i] is TDBNavigator then
      TDBNavigator(Control.Controls[i]).Flat := Flat
    else
      if Control.Controls[i] is TWinControl then begin
        if Control.Controls[i] is TPanel then begin
        {$IFNDEF FPC}
          TPanel(Control.Controls[i]).ParentBackground := False;
        {$ENDIF}
          TPanel(Control.Controls[i]).Color := clBtnFace;
        end;
        ReplaceFlatStyle(TWinControl(Control.Controls[i]), Flat);
      end;
end;
{$ENDIF}

//Demo Change
procedure TDemoForm.InitializeDemoFrame(Frame: TDemoFrame; DemoType: TDemoType);
begin
  Frame.Connection := GetConnection;
  Frame.SetDebug(cbDebug.Checked);
  Frame.Parent := pnDemo;
  if DemoType = dtCategory then                       //Attach browser event handlers
    TCategoryFrame(Frame).OnNavigate := OnNavigate;
{$IFDEF XPMAN}
  if GetIsXPMan then
    ReplaceFlatStyle(Frame, False);
{$ENDIF}
  Frame.Initialize;
end;

procedure TDemoForm.UpdateDemo;
var
  i: integer;
begin
  for i := 1 to StatusBar.Panels.Count - 1 do
    StatusBar.Panels[i].Text := '';
  ActiveNode := TreeView.Selected;
  if (ActiveNode <> nil) then
    with Demos.SelectDemo(ActiveNode.AbsoluteIndex) do begin
      InitializeDemoFrame(Frame, DemoType);
      DemoSourceLoaded := False;
      FormSourceLoaded := False;
      if DemoType = dtCategory then begin
        pnShowSource.Visible := False;
        pnOpenDemoDir.Visible := False;
        ShowDemo;
      end
      else begin
        pnShowSource.Visible := True;
      {$IFNDEF LINUX}
        pnOpenDemoDir.Visible := True;
      {$ELSE}
        pnOpenDemoDir.Visible := False;
      {$ENDIF}
        if sbDemo.Down then
          ShowDemo
        else
          if sbSource.Down then
            ShowDemoSource
          else
            ShowFormSource;
      end;
      Self.Caption := ApplicationTitle + ' - ' + Name;
      Application.Title := ApplicationTitle;
    end;
  StatusBar.Repaint;
end;

procedure TDemoForm.ShowDemo;
begin
  pnSource.Visible := False;
  pnDemo.Visible := True;
end;

procedure TDemoForm.ShowDemoSource;
begin
  if not DemoSourceLoaded then begin
    Demos.SelectedDemo.LoadDemoCode(SourceBrowser.Lines);
    DemoSourceLoaded := True;
    FormSourceLoaded := False;
  end;
  pnSource.Visible := True;
  pnDemo.Visible := False;
end;

procedure TDemoForm.ShowFormSource;
begin
  if not FormSourceLoaded then begin
    Demos.SelectedDemo.LoadFormCode(SourceBrowser.Lines);
    FormSourceLoaded := True;
    DemoSourceLoaded := False;
  end;
  pnSource.Visible := True;
  pnDemo.Visible := False;
end;

//User control
procedure TDemoForm.sbOpenDemoDirClick(Sender: TObject);
begin
  Demos.SelectedDemo.OpenDemoFolder;
end;

procedure TDemoForm.cbDebugClick(Sender: TObject);
begin
  Demos.SelectedDemo.Frame.SetDebug(cbDebug.Checked);
end;

{$IFNDEF WEB}
procedure TDemoForm.OnNavigate(DemoDescription: string);
var
   Node: TTreeNode;
   DemoName, CategoryName, FolderName: string;
   ListBox: TListBox;
   i: integer;
begin
  Node := TreeView.Items.GetFirstNode;
  DemoName := Trim(Copy(DemoDescription, 1, pos('-', DemoDescription) - 1));
  while Node <> nil do begin
    if TDemo(Node.Data).Name = DemoName then begin
      TreeView.Selected := Node;
      SelectDemo;
      break;
    end;
    Node := Node.GetNext;
  end;
  // Demo was not found in the tree. This is supplementary demo.

  if (Demos.SelectedDemo.DemoType = dtCategory) and (DemoName <> '') then begin
    ListBox := TCategoryFrame(Demos.SelectedDemo.Frame).DemosDescription;
    for i := ListBox.ItemIndex downto 0 do
      if (ListBox.Items[i] <> '') and (ListBox.Items[i][1] = ' ') then begin
        CategoryName := Trim(ListBox.Items[i]);
        Break;
      end;
  end;
{$IFNDEF LINUX}
  FolderName := ExtractFilePath(ExtractFileDir(Application.ExeName)) + CategoryName + '\' + DemoName;
  ShellExecute(0, 'open', {$IFNDEF CLR}PChar{$ENDIF}(FolderName), '', '.', SW_SHOW);
{$ENDIF}
end;

{$ELSE}
procedure TDemoForm.OnNavigate(Index: integer);
begin
  TreeView.Items[Index].Selected := True;
  SelectDemo;
end;
{$ENDIF}

procedure TDemoForm.lbAboutClick(Sender: TObject);
begin
  lbAbout.Font.Color := $FFFFFF;
  lbAbout.Cursor := crDefault;
end;

//About highlite
procedure TDemoForm.lbAboutMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbAbout.Font.Color := TColor($FF00001A); //clHotLight
  lbAbout.Cursor := crHandPoint;
end;

procedure TDemoForm.lbTitleMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbAbout.Font.Color := $FFFFFF;
  lbAbout.Cursor := crDefault;
end;

procedure TDemoForm.sbConnectClick(Sender: TObject);
begin
  GetConnection.Connect;
end;

procedure TDemoForm.sbDisconnectClick(Sender: TObject);
begin
  GetConnection.Disconnect;
end;

procedure TDemoForm.AfterConnect(Sender: TObject);
begin
  sbDisconnect.Enabled := True;
  sbConnect.Enabled := False;
end;

procedure TDemoForm.AfterDisconnect(Sender: TObject);
begin
  sbDisconnect.Enabled := False;
  sbConnect.Enabled := True;
end;

//History
procedure TDemoForm.SelectDemo;
var
  i: integer;
begin
  if TreeView.Selected = ActiveNode then              //Same demo selected
    Exit;
  UpdateDemo;
  if HistoryIndex = (MAX_HISTORY_SIZE - 1) then
    for i := 0 to MAX_HISTORY_SIZE - 2 do
      History[i] := History[i + 1]
  else
    Inc(HistoryIndex);
  History[HistoryIndex] := ActiveNode.AbsoluteIndex;
  HistoryEnd := HistoryIndex;
  DisableBrowse(HistoryIndex = 0, True);
end;

procedure TDemoForm.NavigateHistory(Offset: integer);
begin
  if ((HistoryIndex + Offset) < 0) or ((HistoryIndex + Offset) >= MAX_HISTORY_SIZE) then
    raise Exception.Create('Wrong history index');
  HistoryIndex := HistoryIndex + Offset;
  TreeView.Items[Demos.GetDemoIndex(History[HistoryIndex])].Selected := True;
  DisableBrowse(HistoryIndex = 0, HistoryIndex = HistoryEnd);
  UpdateDemo;
end;

procedure TDemoForm.GetBackHistory(BackList: TStrings);
var
  i: integer;
begin
  BackList.Clear;
  for i := HistoryIndex - 1 downto 0 do
    BackList.Add(Demos[History[i]].Name);
end;

procedure TDemoForm.GetForwardHistory(ForwardList: TStrings);
var
  i: integer;
begin
  ForwardList.Clear;
  for i := HistoryIndex + 1 to HistoryEnd do
    ForwardList.Add(Demos[History[i]].Name);
end;

procedure TDemoForm.DisableBrowse(Back, Forward: boolean);
begin
  tbBrowseBack.Enabled := not Back;
  tbBrowseForward.Enabled := not Forward;
end;

procedure TDemoForm.FillHistoryPopup(BackHistory: boolean);
var
  NewItem: TMenuItem;
  List: TStrings;
  i: integer;
  HistoryPopup: TPopupMenu;
begin
  if BackHistory then
    HistoryPopup := BackHistoryPopup
  else
    HistoryPopup := ForwardHistoryPopup;
  HistoryPopup.Items.Clear;
  List := TStringList.Create;
  if BackHistory then
    GetBackHistory(List)
  else
    GetForwardHistory(List);
  for i := 1 to List.Count do begin
    NewItem := TMenuItem.Create(HistoryPopup);
    HistoryPopup.Items.Add(NewItem);
    NewItem.Caption := List[i - 1];
    if BackHistory then
      NewItem.Tag := -i
    else
      NewItem.Tag := i;
    NewItem.OnClick := HistoryItemClick;
  end;
  List.Free;
end;

procedure TDemoForm.HistoryItemClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    NavigateHistory(TMenuItem(Sender).Tag);
end;

procedure TDemoForm.BackHistoryPopupPopup(Sender: TObject);
begin
  FillHistoryPopup(True);
end;

procedure TDemoForm.ForwardHistoryPopupPopup(Sender: TObject);
begin
  FillHistoryPopup(False);
end;

procedure TDemoForm.tbBrowseBackClick(Sender: TObject);
begin
  NavigateHistory(-1);
end;

procedure TDemoForm.tbBrowseForwardClick(Sender: TObject);
begin
  NavigateHistory(1);
end;

procedure TDemoForm.sbDemoClick(Sender: TObject);
begin
  ShowDemo;
end;

procedure TDemoForm.sbSourceClick(Sender: TObject);
begin
  ShowDemoSource;
end;

procedure TDemoForm.sbFormTextClick(Sender: TObject);
begin
  ShowFormSource;
end;

procedure TDemoForm.FormResize(Sender: TObject);
begin
  lbAbout.Left := lbAbout.Parent.ClientWidth - 100;
  cbDebug.Left := cbDebug.Parent.ClientWidth - 100;
  pnOpenDemoDir.Left := cbDebug.Left - (pnOpenDemoDir.Width + 15);
  pnShowSource.Left := pnOpenDemoDir.Left - (pnShowSource.Width + 15);
end;

procedure TDemoForm.OnScriptError(Sender: TObject; E: Exception;
  SQL: _string; var Action: TErrorAction);
var
  OperationStr,
  ScriptFileStr,
  MessageStr: string;
begin
  if DropScriptActive then begin
    OperationStr := 'drop';
    ScriptFileStr := 'UninstallDemoObjects.sql';
  end
  else begin
    OperationStr := 'create';
    ScriptFileStr := 'InstallDemoObjects.sql';
  end;
  MessageStr := Format('An error has been occured: %s' +
    #$d#$d'You can manually %s objects required for demo by using the ' +
    'following file: %%%s%%\Demos\%s' +
    #$d'%%%s%% is the %s installation path on your computer.' + #13#10 + 'Ignore this exception?',
    [E.Message, OperationStr, ProductName, ScriptFileStr, ProductName, ProductName]);
  Action := eaContinue;
  if not IgnoreScriptErrors then
    case MessageDlg(MessageStr, mtError, [mbYes, mbNo{$IFNDEF LINUX}, mbYesToAll{$ENDIF}], 0) of
      mrNo:
        Action := eaAbort;
    {$IFNDEF LINUX}
      mrYesToAll:
        IgnoreScriptErrors := True;
    {$ENDIF}
    end;
end;

procedure TDemoForm.btScriptClick(Sender: TObject);
var
  s: string;
begin
  DropScriptActive := Sender = btDrop;
  if DropScriptActive then
    s := 'removed from database'
  else
    s := 'created in database';
  if MessageDlg(Format('Objects required for the demo will be %s. Continue?', [s]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    IgnoreScriptErrors := False;
    GetConnection.Connect;
    if Sender = btCreate then
      ExecCreateScript
    else
      ExecDropScript;
  end;
end;

{$IFDEF FPC}
initialization
  {$i DemoForm.lrs}
{$ENDIF}

end.
