
procedure SetCursor(Value: integer);
procedure ShowDebugForm(DASQLMonitorClass: TDASQLMonitorClass;
  Component: TComponent; SQL: string; Params: TDAParams; Caption: string);
function ShowConnectForm(ConnectDialog: TCustomConnectDialog): boolean;
procedure StartWait;
procedure StopWait;
function ApplicationTitle: string;

{$IFDEF MSWINDOWS}
function GetHelpFileName(const ProjectName: string; UseCHM: boolean = False): string;
{$ENDIF}

implementation

var
  SQLDelay: longword = 30;

{$IFDEF WIN32_64}
var
  TimerID: word = 0;
  StartTime: longword = 0;

procedure FreeTimer(ForceKill: boolean = False);
begin
  if (TimerID <> 0) and (ForceKill or (GetTickCount - StartTime > SQLDelay)) then begin
    KillTimer(0, TimerID);
    TimerID := 0;
    StartTime := 0;
    SetCursor(crDefault);
  end;
end;

procedure TimerCallBack(hWnd: HWND; Message: Word; TimerID: word;
  SysTime: longint); stdcall;
begin
  FreeTimer;
end;
{$ELSE}
type
  TCursorTimer = class (TTimer)
  private
    procedure TimerHandler(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Timer: TTimer;

{ TCursorTimer }

constructor TCursorTimer.Create(AOwner: TComponent);
begin
  inherited;

  OnTimer := TimerHandler;
end;

procedure TCursorTimer.TimerHandler(Sender: TObject);
begin
  Enabled := False;
  SetCursor(crDefault);
end;
{$ENDIF}

const
{$IFDEF MSWINDOWS}
  KeyPath = '\SOFTWARE\Devart\DBAccess\';
{$ENDIF}
  SQLArrowCount: integer = 0;

procedure SetCursor(Value: integer);
begin
  if ChangeCursor
    {$IFDEF WIN32_64}and (GetCurrentThreadID = MainThreadID){$ENDIF}
  then
    case Value of
      crDefault: begin
        if SQLArrowCount > 0 then begin
          if Screen.Cursor <> crSQLWait then
            Dec(SQLArrowCount);
          if SQLArrowCount > 0 then
            Screen.Cursor := crSQLArrow
          else
            Screen.Cursor := crDefault;
        end
        else
          Screen.Cursor := crDefault;
      end;
      crSQLWait: begin
      {$IFDEF WIN32_64}
        if TimerID = 0 then
          TimerID := SetTimer(0, 0, SQLDelay, @TimerCallBack);

        if Screen.Cursor <> crSQLWait then
          Screen.Cursor := crSQLWait;

        StartTime := GetTickCount;
      {$ELSE}
        if Timer = nil then begin
          Timer := TCursorTimer.Create(nil);
          Timer.Enabled := False;
        end;
        if not Timer.Enabled then begin
          Timer.Interval := SQLDelay;
          Timer.Enabled := True;
        end;
      {$IFNDEF K3}
        // strange AV when freeing cursor on exit (perhaps beta problem)
        if Screen.Cursor <> crSQLWait then
          Screen.Cursor := crSQLWait;
      {$ENDIF}
      {$ENDIF}
      end;
      crSQLArrow: begin
        Inc(SQLArrowCount);
        Screen.Cursor:= crSQLArrow;
      end;
    end;
end;

type
  TDebugForm = class (TForm)
  public
    constructor CreateNew(Owner: TComponent; Dummy: integer = 0); override;

    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure ButtonClick(Sender: TObject);
  end;

{ TDebugForm }

constructor TDebugForm.CreateNew(Owner: TComponent; Dummy: integer = 0);
begin
  inherited CreateNew(Owner{$IFDEF FPC}, Dummy{$ENDIF});

  KeyPreview := True;
  OnKeyPress := FormKeyPress;
end;

procedure TDebugForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) or (Key = #27) then
    Close;
end;

procedure TDebugForm.ButtonClick(Sender: TObject);
begin
  Close;
end;

procedure ShowDebugForm(DASQLMonitorClass: TDASQLMonitorClass;
  Component: TComponent; SQL: string; Params: TDAParams; Caption: string);
var
  Form: TDebugForm;
  Memo: TMemo;
  St: string;
  Panel: TPanel;
{$IFDEF MSWINDOWS}
  Registry: TRegistry;
{$ENDIF}
begin
  Form := TDebugForm.CreateNew(nil);

{$IFDEF MSWINDOWS}
  Registry := TRegistry.Create;
{$ENDIF}

  try
    St := string(DASQLMonitorClass.GetCaption) + ' Debug: ';
    if Component.Owner <> nil then
      St := St + Component.Owner.Name + '.';
    St := St + Component.Name;
    if Caption <> '' then
      St := St + '  [' + Caption + ']';

    Form.Caption := St;

    Panel := TPanel.Create(Form);
    with Panel do begin
      Parent := Form;
      Align := alTop;
      Height := 24;
      BevelInner := bvNone;
      BevelOuter := bvNone;
    end;

    with TButton.Create(Form) do begin
      Parent := Panel;
      Caption := 'Close';
      Font.Style := [fsBold];
      Top := 1;
      Left := 0;
      Height := 22;
      Width := 150;
      OnClick := Form.ButtonClick;
    end;

    with TLabel.Create(Form) do begin
      Parent := Panel;
      Caption := 'Before execution';
      Font.Style := [fsBold];
      Font.Color := clNavy;
      Top := 5;
      Left := 175;
    end;

    with TPanel.Create(Form) do begin
      Parent := Form;
      Align := alLeft;
      Width := 15;
      BevelInner := bvLowered;
      BevelOuter := bvNone;
    end;

    Memo := TMemo.Create(Form);
    with Memo do begin
      Parent := Form;
      Align := alClient;
      ReadOnly := True;
      Color := clBtnFace;
      Font.Name := 'Courier New';
    {$IFDEF MSWINDOWS}
    {$IFNDEF CLX}
      Font.Charset := GetDefFontCharSet;
    {$ENDIF}
    {$ENDIF}
      ScrollBars := ssBoth;
      WordWrap := False;
    end;

    Form.ActiveControl := Memo;

    Memo.Lines.Text := SQL + #13#10 + #13#10 + DASQLMonitorClass.GetParams(Params);
    Memo.SelStart := 0;

  {$IFDEF MSWINDOWS}
    if Registry.OpenKey(KeyPath + 'Debug', False) then begin
      if Registry.ValueExists('Left') then
        Form.Left := Registry.ReadInteger('Left');
      if Registry.ValueExists('Top') then
        Form.Top := Registry.ReadInteger('Top');
      if Registry.ValueExists('Width') then
        Form.Width := Registry.ReadInteger('Width');
      if Registry.ValueExists('Height') then
        Form.Height := Registry.ReadInteger('Height');

      Registry.CloseKey;
    end
    else
  {$ENDIF}
    begin
      Form.Width := (Screen.Width) div 2;
      Form.Left := (Screen.Width - Form.Width) div 2;
      Form.Top := (Screen.Height - Form.Height) div 2;
    end;

    Form.ShowModal;

  {$IFDEF MSWINDOWS}
    if Registry.OpenKey(KeyPath + 'Debug', True) then begin
      Registry.WriteInteger('Left', Form.Left);
      Registry.WriteInteger('Top', Form.Top);
      Registry.WriteInteger('Width', Form.Width);
      Registry.WriteInteger('Height', Form.Height);
      Registry.CloseKey;
    end;
  {$ENDIF}

  finally
  {$IFDEF MSWINDOWS}
    Registry.Free;
  {$ENDIF}
    Form.Free;
  end;
end;

type
  _TCustomConnectDialog = class(TCustomConnectDialog)
  end;

function ShowConnectForm(ConnectDialog: TCustomConnectDialog): boolean;
var
  FormClass: TFormClass;
  ConnectForm: TForm;
  PropInfo: PPropInfo;
begin
  if (ConnectDialog.DialogClass = '')
    or (csDesigning in ConnectDialog.ComponentState)
  then
    FormClass := TFormClass(_TCustomConnectDialog(ConnectDialog).DefDialogClass)
  else
    FormClass := TFormClass(FindClass(ConnectDialog.DialogClass));

  ConnectForm := FormClass.Create(nil);
  try
    PropInfo := GetPropInfo(FormClass.ClassInfo, 'ConnectDialog');
    if PropInfo <> nil then
      SetObjectProp(ConnectForm, PropInfo, ConnectDialog);

    Result := (ConnectForm.ShowModal = mrOk) and
      (not TDBAccessUtils.GetNeedConnect(ConnectDialog) or ConnectDialog.Connection.Connected);
  finally
    ConnectForm.Free;
  end;
end;

procedure StartWait;
begin
  SetCursor(crSQLWait);
end;

procedure StopWait;
begin
  SetCursor(crDefault);
end;

function ApplicationTitle: string;
begin
  Result := Application.Title;
end;

{$IFDEF MSWINDOWS}
{$IFDEF VER8P}
function GetHelpFileName(const ProjectName: string; UseCHM: boolean): string;
var
  Path: string;
  Ind: integer;

  Registry: TRegistry;
  sl: TStringList;
  i: integer;
  UCProjectName: string;
{$IFDEF VER9P}
  ValName: string;
{$ENDIF}
begin
  Result := '';
  Registry := nil;
  sl := nil;
  try
    Registry := TRegistry.Create;
  {$IFDEF VER9P}
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKey('Software\Devart\' + ProjectName, False) then begin
      try
      {$IFDEF VER9}
        ValName := 'D9';
      {$ELSE}
      {$IFDEF VER10}
        ValName := 'D10';
      {$ELSE}
      {$IFDEF VER11}
        ValName := 'D11';
      {$ELSE}
      {$IFDEF VER12}
        ValName := 'D12';
      {$ELSE}
      {$IFDEF VER14}
        ValName := 'D14';
      {$ELSE}
      {$IFDEF VER15}
        ValName := 'D15';
      {$ELSE}
      {$IFDEF VER16}
        ValName := 'D16';
      {$ELSE}
        error
      {$ENDIF}
      {$ENDIF}
      {$ENDIF}
      {$ENDIF}
      {$ENDIF}
      {$ENDIF}
      {$ENDIF}
        Path := Registry.ReadString(ValName);
        Result := IncludeTrailingPathDelimiter(Path) + 'Doc\' + ProjectName + '.hxs';
        Exit;
      except
        // silent
        Registry.CloseKey;
      end;
    end;
  {$ENDIF}

    Registry.RootKey := HKEY_CURRENT_USER;
    sl := TStringList.Create;
    if not Registry.OpenKey('Software\' +
      {$IFDEF VER8}'Borland\BDS\2.0'{$ENDIF}
      {$IFDEF VER9}'Borland\BDS\3.0'{$ENDIF}
      {$IFDEF VER10}'Borland\BDS\4.0'{$ENDIF}
      {$IFDEF VER11}'Borland\BDS\5.0'{$ENDIF}
      {$IFDEF VER12}'CodeGear\BDS\6.0'{$ENDIF}
      {$IFDEF VER14}'CodeGear\BDS\7.0'{$ENDIF}
      {$IFDEF VER15}'Embarcadero\BDS\8.0'{$ENDIF}
      {$IFDEF VER16}'Embarcadero\BDS\9.0'{$ENDIF}
      + '\Known Assemblies', False) then begin
      Result := ProjectName + '.hxs';
      Exit;
    end;

    Registry.GetValueNames(sl);
    UCProjectName := UpperCase('Devart.' + ProjectName + '.Design');
    for i := 0 to sl.Count - 1 do
      if (Registry.GetDataType(sl[i]) in [rdString, rdExpandString])
        and (UpperCase(Registry.ReadString(sl[i])) = UCProjectName) then begin
        Path := sl[i];
        Break;
      end;

    if Path = '' then begin
      Result := ProjectName + '.hxs';
      Exit;
    end;

  finally
    sl.Free;
    Registry.Free;
  end;

  Ind := LastDelimiter('\', Path);
  Path := Copy(Path, 1, Ind - 1);
  Ind := LastDelimiter('\', Path);
  Path := Copy(Path, 1, Ind);
  Result := Path + 'Doc\' + ProjectName + '.hxs';
end;
{$ELSE}
function GetHelpFileName(const ProjectName: string; UseCHM: boolean = False): string;
var
  Path: string;
{$IFNDEF VER7}
  Ind: integer;
{$ENDIF}
{$IFDEF VER5P}
  Registry: TRegistry;
  ValName: string;
{$ENDIF}
begin
{$IFDEF VER5P}
  if UseCHM then begin
    Result := '';
    Registry := TRegistry.Create;
    try
      Registry.RootKey := HKEY_LOCAL_MACHINE;
      if Registry.OpenKey('Software\Devart\' + ProjectName, False) then begin
        try
        {$IFDEF BCB}
          ValName := 'C6';
        {$ELSE}
        {$IFDEF VER5}
          ValName := 'D5';
        {$ELSE}
        {$IFDEF VER6}
          ValName := 'D6';
        {$ELSE}
        {$IFDEF VER7}
          ValName := 'D7';
        {$ENDIF}
        {$ENDIF}
        {$ENDIF}
        {$ENDIF}
          Path := Registry.ReadString(ValName);
        {$IFDEF VER5}
          Result := IncludeTrailingBackslash(Path) + 'Doc\' + ProjectName + '.chm';          
        {$ELSE}
          Result := IncludeTrailingPathDelimiter(Path) + 'Doc\' + ProjectName + '.chm';
        {$ENDIF}
          Exit;
        except
          // silent
        end;
      end;
    finally
      Registry.Free;
    end;
  end
  else
{$ENDIF}
  begin
  {$IFDEF VER7}
    Result := ProjectName + '.hlp';
  {$ELSE}
    Path := Application.ExeName;
    Ind := LastDelimiter('\', Path);
    Path := Copy(Path, 1, Ind - 1);
    Ind := LastDelimiter('\', Path);
    Path := Copy(Path, 1, Ind);
    Result := Path + 'help\' + ProjectName + '.hlp';
  {$ENDIF}
  end;
end;
{$ENDIF}
{$ENDIF}

initialization
{$IFDEF WIN32_64}
{$IFNDEF CLX}
  Screen.Cursors[crSQLArrow] := LoadCursor(HInstance, 'SQLARROW');
{$ENDIF}
{$ELSE}
  Timer := nil;
{$ENDIF}

  SetCursorProc := SetCursor;
  ShowConnectFormProc := ShowConnectForm;
  ShowDebugFormProc := ShowDebugForm;
  StartWaitProc := StartWait;
  StopWaitProc := StopWait;
  ApplicationTitleProc := ApplicationTitle;
  {$IFNDEF VER6P}
  ApplicationHandleException := Application.HandleException;
  {$ENDIF};

finalization
{$IFDEF WIN32_64}
{$IFNDEF CLX}
  DestroyCursor(Screen.Cursors[crSQLArrow]);
{$ENDIF}
  FreeTimer(True);
{$ELSE}
  if Timer <> nil then
    Timer.Free;
{$ENDIF}

end.

