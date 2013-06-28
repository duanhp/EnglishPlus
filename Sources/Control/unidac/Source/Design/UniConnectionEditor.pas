{$IFNDEF CLR}

{$I UniDac.inc}

unit UniConnectionEditor;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, DacVcl, UniDacVcl, Buttons, Grids,
{$IFNDEF FPC}
  Mask, ValEdit,
{$ENDIF}
{$IFDEF FPC}
  LResources, LCLType,
{$ENDIF}
  CREditor, DAConnectionEditor, MemUtils, DBAccess, Uni, UniSpecificOptionsFrame;

type
  TUniConnectionEditorForm = class(TDAConnectionEditorForm)
    lbProvider: TLabel;
    edProvider: TComboBox;
    lbPort: TLabel;
    lbDatabase: TLabel;
    edDatabase: TComboBox;
    shOptions: TTabSheet;
    shMacros: TTabSheet;
    edPort: TEdit;
    pnOptions: TPanel;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    sgMacros: TDrawGrid;
    btClearAllMacros: TButton;
    btDeleteMacro: TButton;
    btReplaceMacro: TButton;
    btAddMacro: TButton;
    Label8: TLabel;
    Label7: TLabel;
    Label4: TLabel;
    edMacroValue: TEdit;
    edMacroName: TEdit;
    edMacroCondition: TComboBox;
    procedure edProviderChange(Sender: TObject);
    procedure edDatabaseExit(Sender: TObject);
    procedure edPortExit(Sender: TObject);
    procedure edPortChange(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure edDatabaseDropDown(Sender: TObject);
    procedure edDatabaseKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure btAddMacroClick(Sender: TObject);
    procedure btDeleteMacroClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure sgMacrosDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure btReplaceMacroClick(Sender: TObject);
    procedure btClearAllMacrosClick(Sender: TObject);
    procedure sgMacrosSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure edMacroConditionDropDown(Sender: TObject);
  protected
    FOptionsFrame: TUniSpecificOptionsFrame;

    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);

    procedure DoInit; override;
    procedure FillInfo; override;
  {$IFDEF MSWINDOWS}
    procedure AddServerToList; override;
  {$ENDIF}

    procedure ConnToControls; override;
    procedure ControlsToConn; override;
    procedure ShowState(Yellow: boolean = False); override;
    procedure SetupForSpecificOptions;
    procedure EnableMacroButtons;

  {$IFNDEF CLR}
    procedure ReplaceEdit(var Edit: TWinControl);
    procedure SpinEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  {$ENDIF}

  public
    property Connection: TUniConnection read GetConnection write SetConnection;
  end;

var
  UniConnectionEditorForm: TUniConnectionEditorForm;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniConnectionEditor.dfm}
{$ENDIF}
{$ENDIF}

uses
  TypInfo, UniProvider;

{$I UniDacVer.inc}

{ TUniSessionEditorForm }

function TUniConnectionEditorForm.GetConnection: TUniConnection;
begin
  Result := FConnection as TUniConnection;
end;

procedure TUniConnectionEditorForm.SetConnection(Value: TUniConnection);
begin
  FConnection := Value;
end;

procedure TUniConnectionEditorForm.DoInit;
var
  Index: integer;
{$IFDEF MSWINDOWS}
{$IFNDEF CLR}
  WinControl: TWinControl;
{$ENDIF}
{$ENDIF}
begin
  UniProviders.GetProviderNames(edProvider.Items);
{$IFDEF MSWINDOWS}
{$IFNDEF CLR}
  WinControl := edPort;
  ReplaceEdit(WinControl);
{$ENDIF}
{$ENDIF}

  FOptionsFrame := TUniSpecificOptionsFrame.Create(Self);
  FOptionsFrame.Parent := pnOptions;

  inherited;

  lbVersion.Caption := UniDACVersion +' ';
  UpdateVersionPosition;

  Index := edProvider.Items.IndexOf(Connection.ProviderName);
  if Index < 0 then
    ActiveControl := edProvider
  else
  if edUsername.Enabled then
    ActiveControl := edUsername
  else
  if edPassword.Enabled then
    ActiveControl := edPassword
  else
  if edServer.Enabled then
    ActiveControl := edServer
  else
  if edDatabase.Enabled then
    ActiveControl := edDatabase;

  if InitialProperty = 'SpecificOptions' then
    PageControl.ActivePage := shOptions
  else
  if InitialProperty = 'Macros' then
    PageControl.ActivePage := shMacros;
end;

procedure TUniConnectionEditorForm.ConnToControls;
begin
  edProvider.Text := Connection.ProviderName;

  inherited;

  edProviderChange(nil);

  sgMacros.RowCount := Connection.Macros.Count + 2;
  EnableMacroButtons;
end;

procedure TUniConnectionEditorForm.ControlsToConn;
begin
  Connection.Database := edDatabase.Text; // OnExit event is not generated on Kylix when dialog is closing
  // all other parameters are set in controls OnChange event handlers
end;

procedure TUniConnectionEditorForm.ShowState(Yellow: boolean = False);
begin
  inherited;

end;

procedure TUniConnectionEditorForm.SetupForSpecificOptions;
begin
  if TUniUtils.CanGetProvider(Connection) then begin
    edUsername.Enabled :=  TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog)).UsernameEnabled;
    edPassword.Enabled :=  TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog)).PasswordEnabled;
    edServer.Enabled :=  TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog)).ServerEnabled;
    edDatabase.Enabled := TUniUtils.GetProvider(Connection).IsDatabaseSupported and
      TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog)).DatabaseEnabled;
    edPort.Enabled := TUniUtils.GetProvider(Connection).IsPortSupported and
      TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog)).PortEnabled;
  end
  else begin
    edUsername.Enabled := True;
    edPassword.Enabled := True;
    edServer.Enabled := True;
    edPort.Enabled := False;
    edDatabase.Enabled := False;
  end;

  lbUsername.Enabled := edUsername.Enabled;
  lbPassword.Enabled := edPassword.Enabled;
  lbServer.Enabled := edServer.Enabled;
  lbDatabase.Enabled := edDatabase.Enabled;
  lbPort.Enabled := edPort.Enabled;

  if edUsername.Enabled then
    edUsername.Text := Connection.Username
  else
    edUsername.Text := '';

  if edPassword.Enabled then
    edPassword.Text := Connection.Password
  else
    edPassword.Text := '';

  if edServer.Enabled then
    edServer.Text := Connection.Server
  else
    edServer.Text := '';

  if edDatabase.Enabled then
    edDatabase.Text := Connection.Database
  else
    edDatabase.Text := '';

  if edPort.Enabled then
    edPort.Text := IntToStr(Connection.Port)
  else
    edPort.Text := '';
end;

procedure TUniConnectionEditorForm.EnableMacroButtons;
var
  i: integer;
begin
  i := sgMacros.Selection.Top;

  btDeleteMacro.Enabled := Connection.Macros.Count >= i;
  UpBtn.Enabled := (i > 1) and (Connection.Macros.Count >= i);
  DownBtn.Enabled := i < Connection.Macros.Count;
end;

procedure TUniConnectionEditorForm.FillInfo;
begin
  if not Connection.Connected then
    meInfo.Lines.Clear
  else begin
    meInfo.Lines.BeginUpdate;
    try
      meInfo.Lines.Text := 'Server version: ' + Connection.ServerVersionFull + #13#10#13#10 +
        'Client version: ' + Connection.ClientVersion + #13#10;
    finally
      meInfo.Lines.EndUpdate;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TUniConnectionEditorForm.AddServerToList;
begin
  inherited;

  if TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog)).UseDatabaseHistory then
    TUniConnectDialogUtils.SaveDatabaseListToRegistry(TUniConnectDialog(FConnectDialog));
end;
{$ENDIF}

procedure TUniConnectionEditorForm.edProviderChange(Sender: TObject);
begin
  try
    if Connection.ProviderName <> edProvider.Text then begin
      Connection.Disconnect;
      Connection.ProviderName := Trim(edProvider.Text);
    end;
    SetupForSpecificOptions;
    FOptionsFrame.LoadOptions(Connection.ProviderName, otConnection, Connection.SpecificOptions);
  finally
    ShowState;
  end;
end;

procedure TUniConnectionEditorForm.edDatabaseExit(Sender: TObject);
begin
  try
    Connection.Database := edDatabase.Text;
  finally
    ShowState;
  end;
end;

procedure TUniConnectionEditorForm.edDatabaseDropDown(Sender: TObject);
var
  DialogService: TConnectDialogService;
  OldLoginPrompt: Boolean;
  OldConnected: boolean;
  OldDatabase: _string;
  List: _TStringList;
begin
  if not TUniUtils.CanGetProvider(Connection) then
    exit;

  StartWait;
  try
    DialogService := TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog));
    if DialogService.UseDatabaseHistory then
    {$IFDEF MSWINDOWS}
      TUniConnectDialogUtils.LoadDatabaseListFromRegistry(TUniConnectDialog(FConnectDialog), edDatabase.Items)
    {$ENDIF}
    else begin
      OldLoginPrompt := Connection.LoginPrompt;
      Connection.LoginPrompt := False;
      OldConnected := Connection.Connected;
      OldDatabase := Connection.Database;
      if not OldConnected then
        Connection.Database := DialogService.GetDefaultDatabase;
      edDatabase.Items.Clear; // for case when GetDatabaseNames raises an exception
      List := _TStringList.Create;
      try
        Connection.GetDatabaseNames(List);
        AssignStrings(List, edDatabase.Items);
      finally
        Connection.Connected := OldConnected;
        if not OldConnected then
          Connection.Database := OldDatabase;
        Connection.LoginPrompt := OldLoginPrompt;
        List.Free;
      end;

      if edDatabase.Items.Count < 20 then
        edDatabase.DropDownCount := edDatabase.Items.Count
      else
        edDatabase.DropDownCount := 20;
    end;
  finally
    StopWait;
  end;
end;

procedure TUniConnectionEditorForm.edDatabaseKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  // Connection.Database is not assigned in OnChange event.
  // We need assign it before default button ('Connect') will act.
  if Key = VK_RETURN then
    edDatabaseExit(Sender);
end;

procedure TUniConnectionEditorForm.edPortExit(Sender: TObject);
begin
  if FInDoInit then
    Exit;

  try
    try
      Connection.Port := StrToInt(edPort.Text);
    except
      PageControl.ActivePage := shConnect;
      edPort.SetFocus;
      edPort.SelectAll;
      raise;
    end;
  finally
    ShowState;
  end;
end;

procedure TUniConnectionEditorForm.edPortChange(Sender: TObject);
begin
  if FInDoInit or (edPort.Text = '') then
    Exit;

  try
    Connection.Port := StrToInt(edPort.Text);
  finally
    ShowState;
  end;
end;

{$IFNDEF CLR}
procedure TUniConnectionEditorForm.ReplaceEdit(var Edit: TWinControl);
type
  TSetProc = procedure (Self: TObject; Ptr: pointer);
var
  EditClass: string;
  NewEdit: TCustomControl;
  OldName: string;
  TypeInfo: PTypeInfo;
begin
  if GetClass('TSpinEdit') <> nil then
    EditClass := 'TSpinEdit'
  else
{$IFDEF BCB}
  if GetClass('TCSpinEdit') <> nil then
    EditClass := 'TCSpinEdit'
  else
{$ENDIF}
    EditClass := '';

  if EditClass <> '' then begin
    NewEdit := TCustomControl(GetClass(EditClass).NewInstance);
    NewEdit.Create(Edit.Owner);

    with NewEdit do begin
      Parent := Edit.Parent;
      Left := Edit.Left;
      Top := Edit.Top;
      Width := Edit.Width;
      Height := Edit.Height;
      Align := Edit.Align;
      TabOrder := Edit.TabOrder;
      Anchors := Edit.Anchors;
      //Constraints := Edit.Constraints;
      TypeInfo := GetClass(EditClass).ClassInfo;
      HelpContext := Edit.HelpContext;

      if Edit is TEdit then begin
        SetReadOnly(NewEdit, TEdit(Edit).ReadOnly);
        SetOrdProp(NewEdit, 'Color', Longint(TEdit(Edit).Color));
      end;

      OnKeyDown := SpinEditKeyDown;
      if GetPropInfo(Edit.ClassInfo, 'OnChange') <> nil then
        SetMethodProp(NewEdit, GetPropInfo(TypeInfo, 'OnChange'),
          GetMethodProp(Edit, GetPropInfo(Edit.ClassInfo, 'OnChange')));
      SetMethodProp(NewEdit, GetPropInfo(TypeInfo, 'OnExit'),
        GetMethodProp(Edit, GetPropInfo(Edit.ClassInfo, 'OnExit')));
      SetMethodProp(NewEdit, GetPropInfo(TypeInfo, 'OnKeyDown'),
        GetMethodProp(Edit, GetPropInfo(Edit.ClassInfo, 'OnKeyDown')));
      SetMethodProp(NewEdit, GetPropInfo(TypeInfo, 'OnKeyPress'),
        GetMethodProp(Edit, GetPropInfo(Edit.ClassInfo, 'OnKeyPress')));
    end;

    if (Edit.Owner <> nil) and (TForm(Edit.Owner).ActiveControl = Edit) then
      TForm(Edit.Owner).ActiveControl := NewEdit;

    OldName := Edit.Name;
    Edit.Free;
    Edit := TEdit(NewEdit);
    NewEdit.Name := OldName;

    if (EditClass = 'TSpinEdit') {$IFDEF BCB} or (EditClass = 'TCSpinEdit' ){$ENDIF} then begin
      SetOrdProp(NewEdit, 'MaxValue', 65535);
      SetOrdProp(NewEdit, 'MinValue', 0);
    end;
  end;
end;

procedure TUniConnectionEditorForm.SpinEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then
    btConnectClick(self);
end;
{$ENDIF}

procedure TUniConnectionEditorForm.PageControlChange(Sender: TObject);
begin
  inherited;

  if PageControl.ActivePage = shConnect then begin
    SetupForSpecificOptions;
    ShowState;
  end;
end;

procedure TUniConnectionEditorForm.FormShow(Sender: TObject);
begin
  inherited;
  TDBAccessUtils.SetLockLoginPrompt(Connection, True);
end;

procedure TUniConnectionEditorForm.btAddMacroClick(Sender: TObject);
var
  Name, Value, Cond: string;
  Macro: TUniMacro;
  Sel: TGridRect;
  i: integer;
begin
  Name := Trim(edMacroName.Text);
  Value := Trim(edMacroValue.Text);
  Cond := Trim(edMacroCondition.Text);
  if (Name <> '') or (Value <> '') or (Cond <> '') then begin
    Sel := sgMacros.Selection;
    i := Sel.Top;

    Macro := TUniMacro(Connection.Macros.Insert(i - 1));
    Macro.Name := Name;
    Macro.Value := Value;
    Macro.Condition := Cond;

    sgMacros.RowCount := Connection.Macros.Count + 2;
    Sel.Top := i + 1;
    Sel.Bottom := i + 1;
    sgMacros.Selection := Sel;
    sgMacros.Invalidate;
    EnableMacroButtons;

    edMacroName.Text := '';
    edMacroValue.Text := '';
    edMacroCondition.Text := '';
    edMacroName.SetFocus;
  end;
end;

procedure TUniConnectionEditorForm.btReplaceMacroClick(Sender: TObject);
var
  Name, Value, Cond: string;
  Macro: TUniMacro;
  i: integer;
begin
  i := sgMacros.Selection.Top;
  if i > Connection.Macros.Count then begin
    btAddMacroClick(nil);
    exit;
  end;

  Name := Trim(edMacroName.Text);
  Value := Trim(edMacroValue.Text);
  Cond := Trim(edMacroCondition.Text);
  if (Name <> '') or (Value <> '') or (Cond <> '') then begin
    Macro := Connection.Macros[i - 1];
    Macro.Name := Name;
    Macro.Value := Value;
    Macro.Condition := Cond;
    sgMacros.Invalidate;
  end;
end;

procedure TUniConnectionEditorForm.btDeleteMacroClick(Sender: TObject);
var
  i: integer;
begin
  i := sgMacros.Selection.Top;
  if Connection.Macros.Count < i then
    exit;

  Connection.Macros[i - 1].Free;
  sgMacros.RowCount := Connection.Macros.Count + 2;
  sgMacros.Invalidate;
  EnableMacroButtons;
end;

procedure TUniConnectionEditorForm.btClearAllMacrosClick(Sender: TObject);
begin
  Connection.Macros.Clear;
  sgMacros.RowCount := Connection.Macros.Count + 2;
  sgMacros.Invalidate;
  EnableMacroButtons;
end;

procedure TUniConnectionEditorForm.UpBtnClick(Sender: TObject);
var
  i: integer;
  Sel: TGridRect;
begin
  Sel := sgMacros.Selection;
  i := Sel.Top;
  if (Connection.Macros.Count < i) or (i <= 1) then
    exit;

  Connection.Macros[i - 1].Index := i - 2;
  Sel.Top := i - 1;
  Sel.Bottom := i - 1;
  sgMacros.Selection := Sel;
  sgMacros.Invalidate;
  EnableMacroButtons;
end;

procedure TUniConnectionEditorForm.DownBtnClick(Sender: TObject);
var
  i: integer;
  Sel: TGridRect;
begin
  Sel := sgMacros.Selection;
  i := Sel.Top;
  if i >= Connection.Macros.Count then
    exit;

  Connection.Macros[i - 1].Index := i;
  Sel.Top := i + 1;
  Sel.Bottom := i + 1;
  sgMacros.Selection := Sel;
  sgMacros.Invalidate;
  EnableMacroButtons;
end;

procedure TUniConnectionEditorForm.sgMacrosDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  s: string;
begin
  if ARow = 0 then begin
    case ACol of
      0: s := 'Name';
      1: s := 'Value';
      2: s := 'Condition';
    end;
  end
  else
  if ARow <= Connection.Macros.Count then begin
    case ACol of
      0: s := Connection.Macros[ARow - 1].Name;
      1: s := Connection.Macros[ARow - 1].Value;
      2: s := Connection.Macros[ARow - 1].Condition;
    end;
  end;

  sgMacros.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, s);
end;

procedure TUniConnectionEditorForm.sgMacrosSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  Name, Value, Cond: string;
  Macro: TUniMacro;
begin
  if (ARow > 0) and (ARow <= Connection.Macros.Count) then begin
    Macro := Connection.Macros[ARow - 1];
    Name := Macro.Name;
    Value := Macro.Value;
    Cond := Macro.Condition;
  end
  else begin
    Name := '';
    Value := '';
    Cond := '';
  end;

  edMacroName.Text := Name;
  edMacroValue.Text := Value;
  edMacroCondition.Text := Cond;

  EnableMacroButtons;
end;

procedure TUniConnectionEditorForm.edMacroConditionDropDown(
  Sender: TObject);
var
  List: TStringList;
  i: integer;
begin
  List := TStringList.Create;
  try
    UniProviders.GetProviderNames(List);
    for i := 0 to List.Count - 1 do
      List[i] := StringReplace(List[i], ' ', '', [rfReplaceAll]);

    edMacroCondition.Items.Assign(List);

    List.Clear;
    List.Sorted := True;
    List.Duplicates := dupIgnore;
    List.CaseSensitive := False;
    for i := 0 to Connection.Macros.Count - 1 do
      List.Add(Connection.Macros[i].Name);

    for i := 0 to List.Count - 1 do
      edMacroCondition.Items.Add(List[i]);
  finally
    List.Free;
  end;
end;

initialization
{$IFDEF FPC}
{$I UniConnectionEditor.lrs}
{$ENDIF}

end.
