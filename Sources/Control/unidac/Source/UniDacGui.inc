type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniConnectDialog = class(TCustomConnectDialog)
  private
    function GetConnection: TUniConnection;

  protected
    FDatabaseLabel: string;
    FPortLabel: string;
    FProviderLabel: string;
    FConnectDialogService: TConnectDialogService;

    function GetServerEnumeratorClass: TCRServerEnumeratorClass; override;
    procedure SetServerEnumerator(Value: TCRServerEnumerator); override;

    procedure CreateConnectDialogService;
    procedure FreeConnectDialogService;
    procedure CheckConnectDialogService;

    procedure SetLabelSet(Value: TLabelSet); override;

    function DefDialogClass: TClass; override;

  {$IFDEF MSWINDOWS}
    function GetKeyPath: string; override;
    function GetServerListKeyPath: string; override;
    function GetServerStoreName: string; override;
    procedure SaveInfoToRegistry(Registry: TRegistry); override;
    procedure LoadInfoFromRegistry(Registry: TRegistry); overload; override;
    procedure LoadInfoFromRegistry(Registry: TRegistry; LoadProviderName: boolean); reintroduce; overload;
    procedure ReloadInfoFromRegistry;
    procedure SaveDatabaseListToRegistry;
    procedure LoadDatabaseListFromRegistry(List: TStrings);
  {$ENDIF}

  public
    destructor Destroy; override;

    function Execute: Boolean; override;

    property Connection: TUniConnection read GetConnection;

  published
    property DatabaseLabel: string read FDatabaseLabel write FDatabaseLabel;
    property PortLabel: string read FPortLabel write FPortLabel;
    property ProviderLabel: string read FProviderLabel write FProviderLabel;

    property Retries;
    property SavePassword;
    property DialogClass;

    property Caption;
    property UsernameLabel;
    property PasswordLabel;
    property ServerLabel;
    property ConnectButton;
    property CancelButton;

    property LabelSet;
    property StoreLogInfo;
  end;

  TUniConnectDialogUtils = class
    class function GetConnectDialogService(Obj: TUniConnectDialog): TConnectDialogService;
  {$IFDEF MSWINDOWS}
    class procedure ReloadInfoFromRegistry(Obj: TUniConnectDialog);
    class procedure SaveDatabaseListToRegistry(Obj: TUniConnectDialog);
    class procedure LoadDatabaseListFromRegistry(Obj: TUniConnectDialog; List: TStrings);
  {$ENDIF}
  end;

function DefConnectDialogClass: TClass;

implementation

uses
  DAConsts;
  
const
  KeyPath = '\SOFTWARE\Devart\UniDAC\';

{ TUniConnectionDialog }

destructor TUniConnectDialog.Destroy;
begin
  FreeConnectDialogService;

  inherited;
end;

function TUniConnectDialog.DefDialogClass: TClass;
begin
  Result := TUniConnectForm;
end;

function TUniConnectDialog.GetConnection: TUniConnection;
begin
  Result := (inherited Connection) as TUniConnection;
end;

function TUniConnectDialog.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  if Connection = nil then
    DatabaseError(SConnectionNotDefined);
    
  if TUniUtils.CanGetProvider(Connection) then
    Result := TUniUtils.GetProvider(Connection).GetServerEnumeratorClass
  else
    Result := TCRServerEnumerator;
end;

procedure TUniConnectDialog.SetServerEnumerator(Value: TCRServerEnumerator);
begin
  inherited;

  Assert(Connection <> nil);
  if (FServerEnumerator <> nil) and TUniUtils.CanGetProvider(Connection) then
    TUniUtils.GetProvider(Connection).SetObjectProps(FServerEnumerator,
      Connection.SpecificOptions, True);
end;

procedure TUniConnectDialog.CreateConnectDialogService;
begin
  FConnectDialogService := TUniUtils.GetProvider(Connection).GetConnectDialogServiceClass.Create;
end;

procedure TUniConnectDialog.FreeConnectDialogService;
begin
  FConnectDialogService.Free;
  FConnectDialogService := nil;
end;

procedure TUniConnectDialog.CheckConnectDialogService;
begin
  if not (FConnectDialogService is TUniUtils.GetProvider(Connection).GetConnectDialogServiceClass) then begin
    FreeConnectDialogService;
    CreateConnectDialogService;
  end;
  TUniUtils.GetProvider(Connection).SetObjectProps(FConnectDialogService,
    Connection.SpecificOptions, True);
end;

{$IFDEF MSWINDOWS}
function TUniConnectDialog.GetKeyPath: string;
begin
  Result := KeyPath;
end;

function TUniConnectDialog.GetServerListKeyPath: string;
begin
  if Connection = nil then
    DatabaseError(SConnectionNotDefined);

  Result := GetKeyPath + 'Connect\Providers\' + Connection.ProviderName;
end;

procedure TUniConnectDialog.SaveInfoToRegistry(Registry: TRegistry);
var
  Provider: TUniProvider;
begin
  Provider := UniProviders.GetProvider(Connection.ProviderName);
  if Assigned(Provider) then begin
    inherited;

    Registry.WriteString('Provider', Connection.ProviderName);
    if Provider.IsPortSupported then
      Registry.WriteInteger('Port', Connection.Port);
    if Provider.IsDatabaseSupported then
      Registry.WriteString('Database', Connection.DataBase);
  end;
end;

procedure TUniConnectDialog.LoadInfoFromRegistry(Registry: TRegistry);
begin
  LoadInfoFromRegistry(Registry, True);
end;

procedure TUniConnectDialog.LoadInfoFromRegistry(Registry: TRegistry; LoadProviderName: boolean);
var
  Provider: TUniProvider;
  ProviderName: string;
begin
  if LoadProviderName and Registry.ValueExists('Provider') then
    ProviderName := Registry.ReadString('Provider')
  else
    ProviderName := Connection.ProviderName;

  Provider := UniProviders.GetProvider(ProviderName);

  if LoadProviderName and (Provider <> nil) then
    Connection.ProviderName := ProviderName;

  inherited LoadInfoFromRegistry(Registry);

  if Provider <> nil then begin
    if Provider.IsPortSupported and Registry.ValueExists('Port') then
      Connection.Port := Registry.ReadInteger('Port');
    if Provider.IsDatabaseSupported and Registry.ValueExists('Database') then
      Connection.Database := Registry.ReadString('Database');
  end;
end;

procedure TUniConnectDialog.ReloadInfoFromRegistry;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    if Registry.OpenKey(GetApplicationKeyPath, False) then
      LoadInfoFromRegistry(Registry, False);
  finally
    Registry.Free;
  end;
end;

function TUniConnectDialog.GetServerStoreName: string;
var
  Mode: integer;
begin
  if TUniUtils.CanGetProvider(Connection) then begin
    CheckConnectDialogService;
    Mode := FConnectDialogService.GetConnectMode;
  end
  else
    Mode := 1;

  Result := 'ServerC' + IntToStr(Mode);
end;

procedure TUniConnectDialog.LoadDatabaseListFromRegistry(List: TStrings);
var
  Registry: TRegistry;
  i: integer;
begin
  Registry := TRegistry.Create;
  try
    if Registry.OpenKey(GetServerListKeyPath, False) then begin
      List.Clear;
      i := 1;
      while Registry.ValueExists(Format('Database %d', [i])) do begin
        List.Add(Registry.ReadString(Format('Database %d', [i])));
        Inc(i);
      end;
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TUniConnectDialog.SaveDatabaseListToRegistry;
var
  Registry: TRegistry;
  List: TStrings;
  i,j: integer;
begin
  Registry := TRegistry.Create;
  try
    if Registry.OpenKey(GetServerListKeyPath, True) then begin
      List := TStringList.Create;
      try
        LoadDatabaseListFromRegistry(List);
        Registry.WriteString(Format('Database %d', [1]), Connection.Database);
        i := 2;
        for j := 0 to List.Count - 1 do
          if AnsiCompareText(List[j], Connection.Database) <> 0 then begin
            Registry.WriteString(Format('Database %d', [i]), List[j]);
            Inc(i);
          end;
        Registry.CloseKey;
      finally
        List.Free;
      end;
    end;
  finally
    Registry.Free;
  end;
end;
{$ENDIF}

procedure TUniConnectDialog.SetLabelSet(Value: TLabelSet);
begin
  inherited;
  
{$IFDEF WIN32}
  if LabelSet <> lsCustom then begin
    FDatabaseLabel := GetString(6);
    FPortLabel := GetString(7);
    FProviderLabel := GetString(9);
  end;
{$ELSE}
  FDatabaseLabel := 'Database';
  FPortLabel := 'Port';
  FProviderLabel := 'Provider';
{$ENDIF}
end;

function DefConnectDialogClass: TClass;
begin
  Result := TUniConnectDialog;
end;

function TUniConnectDialog.Execute: Boolean;
var
  OldProviderName: string;
begin
  if Connection = nil then
    DatabaseError(SConnectionNotDefined);

  Result := False;
  OldProviderName := (Connection as TUniConnection).ProviderName;
  try
    Result := inherited Execute;

    if Result then begin
    {$IFDEF MSWINDOWS}
      if StoreLogInfo and TUniUtils.CanGetProvider(Connection) then begin
        CheckConnectDialogService;
        if FConnectDialogService.UseDatabaseHistory then
          SaveDatabaseListToRegistry;
      end;
    {$ENDIF}
    end;
  finally
    if not Result then
      (Connection as TUniConnection).ProviderName := OldProviderName;
  end;
end;

{ TUniConnectDialogUtils }

class function TUniConnectDialogUtils.GetConnectDialogService(Obj: TUniConnectDialog): TConnectDialogService;
begin
  Obj.CheckConnectDialogService;
  Result := Obj.FConnectDialogService;
end;

{$IFDEF MSWINDOWS}
class procedure TUniConnectDialogUtils.ReloadInfoFromRegistry(
  Obj: TUniConnectDialog);
begin
  Obj.ReloadInfoFromRegistry;
end;

class procedure TUniConnectDialogUtils.SaveDatabaseListToRegistry(Obj: TUniConnectDialog);
begin
  Obj.SaveDatabaseListToRegistry;
end;

class procedure TUniConnectDialogUtils.LoadDatabaseListFromRegistry(Obj: TUniConnectDialog; List: TStrings);
begin
  Obj.LoadDatabaseListFromRegistry(List);
end;
{$ENDIF}

initialization
  DefConnectDialogClassProc := DefConnectDialogClass;

end.
