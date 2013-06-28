unit MyConnectForm;

interface

uses
{$IFNDEF LINUX}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, SysUtils, MemUtils, DBAccess, UniProvider, Uni;

type
  TfmMyConnect = class(TForm)
    Panel: TPanel;
    lbUsername: TLabel;
    lbPassword: TLabel;
    lbServer: TLabel;
    lbPort: TLabel;
    edUserName: TEdit;
    edPassword: TEdit;
    edServer: TComboBox;
    edProvider: TComboBox;
    lbProvider: TLabel;
    lbDatabase: TLabel;
    edDatabase: TComboBox;
    edPort: TEdit;
    btConnect: TBitBtn;
    btCancel: TBitBtn;
    procedure btConnectClick(Sender: TObject);
    procedure edProviderChange(Sender: TObject);
    procedure edServerDropDown(Sender: TObject);
    procedure edDatabaseDropDown(Sender: TObject);

  private
    FConnectDialog: TCustomConnectDialog;
    FRetries: integer;
    FRetry: boolean;
    FProviderGot: boolean;
    procedure SetConnectDialog(Value: TCustomConnectDialog);

  protected
    procedure DoInit; virtual;
    procedure DoConnect; virtual;

  published
    property ConnectDialog: TCustomConnectDialog read FConnectDialog write SetConnectDialog;
  end;

implementation

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

uses
  UniDacVcl;

{ TfmMyConnect }

procedure TfmMyConnect.DoConnect;
begin
  with FConnectDialog.Connection as TUniConnection do begin
    ProviderName := edProvider.Text;
    UserName := edUserName.Text;
    Password := edPassword.Text;
    Server := edServer.Text;

    Server := edServer.Text;

    if edPort.Enabled then
      try
        Port := StrToInt(edPort.Text)
      except
        ActiveControl := edPort;
        raise;
      end;
    if edDatabase.Enabled then
      Database := edDatabase.Text;
  end;

  try
    FConnectDialog.Connection.PerformConnect(FRetry);
    ModalResult := mrOk;
  except
    on E: EUniError do begin
      Dec(FRetries);
      FRetry := True;
      if FRetries = 0 then
        ModalResult := mrCancel;
      raise;
    end
    else
      raise;
  end;
end;

procedure TfmMyConnect.DoInit;
var
  ProviderList: TStringList;
  Index: Integer;
  Provider: TUniProvider;
  ConDialog: TUniConnectDialog;
  Connection: TUniConnection;
begin
  ConDialog := FConnectDialog as TUniConnectDialog;
  Connection := ConDialog.Connection;

  FRetry := False;
  FRetries := ConDialog.Retries;
  Caption := ConDialog.Caption;

  // fill the providers list
  if not FProviderGot then begin
    ProviderList := TStringList.Create;
    try
      UniProviders.GetProviderNames(ProviderList);
      edProvider.Items.Clear;
      edProvider.Items.Assign(ProviderList);
      Index := edProvider.Items.IndexOf(Connection.ProviderName);
      if Index <> -1 then begin
        edProvider.ItemIndex := Index;
//        edProvider.Text := edProvider.Items[Index];
        ActiveControl := edUsername;
      end
      else
        ActiveControl := edProvider;
    finally
      ProviderList.Free;
    end;
  end;

  lbUsername.Caption := ConDialog.UsernameLabel;
  lbPassword.Caption := ConDialog.PasswordLabel;
  lbServer.Caption := ConDialog.ServerLabel;
  lbPort.Caption := ConDialog.PortLabel;
  lbDatabase.Caption := ConDialog.DatabaseLabel;
  lbProvider.Caption := ConDialog.ProviderLabel;

  btConnect.Caption := ConDialog.ConnectButton;
  btCancel.Caption := ConDialog.CancelButton;

  if TUniUtils.CanGetProvider(Connection) then begin
    Provider := TUniUtils.GetProvider(Connection);
    edUsername.Enabled := TUniConnectDialogUtils.GetConnectDialogService(ConDialog).UsernameEnabled;
    edPassword.Enabled := TUniConnectDialogUtils.GetConnectDialogService(ConDialog).PasswordEnabled;
    edServer.Enabled := TUniConnectDialogUtils.GetConnectDialogService(ConDialog).ServerEnabled;
    edDataBase.Enabled := Provider.IsDatabaseSupported and
      TUniConnectDialogUtils.GetConnectDialogService(ConDialog).DatabaseEnabled;
    edPort.Enabled := Provider.IsPortSupported and
      TUniConnectDialogUtils.GetConnectDialogService(ConDialog).PortEnabled;
  end
  else begin
    edUsername.Enabled := True;
    edPassword.Enabled := True;
    edServer.Enabled := True;
    edDataBase.Enabled := False;
    edPort.Enabled := False;
  end;

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

  if edDataBase.Enabled then
    edDataBase.Text := Connection.Database
  else
    edDataBase.Text := '';

  if edPort.Enabled and (Connection.Port <> -1) then
    edPort.Text := IntToStr(Connection.Port)
  else
    edPort.Text := '';

  lbUsername.Enabled := edUsername.Enabled;
  lbPassword.Enabled := edPassword.Enabled;
  lbServer.Enabled := edServer.Enabled;
  lbDatabase.Enabled := edDatabase.Enabled;
  lbPort.Enabled := edPort.Enabled;

  if (edUsername.Text <> '') and (edPassword.Text = '') and (ActiveControl <> edProvider) then
    ActiveControl := edPassword;
end;

procedure TfmMyConnect.SetConnectDialog(Value: TCustomConnectDialog);
begin
  FConnectDialog := Value;
  FProviderGot := False;
  DoInit;
end;

procedure TfmMyConnect.btConnectClick(Sender: TObject);
begin
  DoConnect;
end;             

procedure TfmMyConnect.edProviderChange(Sender: TObject);
begin
  FProviderGot := True;
  try
    (FConnectDialog.Connection as TUniConnection).ProviderName := edProvider.Text;
  {$IFNDEF LINUX}
    TUniConnectDialogUtils.ReloadInfoFromRegistry(FConnectDialog as TUniConnectDialog);
  {$ENDIF}
    DoInit;
  finally
    FProviderGot := False;
  end;
end;

procedure TfmMyConnect.edServerDropDown(Sender: TObject);
var
  List: _TStringList;
begin
  List := _TStringList.Create;
  try
    ConnectDialog.GetServerList(List);
    AssignStrings(List, edServer.Items);
  finally
    List.Free;
  end;
end;

procedure TfmMyConnect.edDatabaseDropDown(Sender: TObject);
var
  Connection: TUniConnection;
  DialogService: TConnectDialogService;
  OldCursor: TCursor;
  OldLoginPrompt: boolean;
  OldConnected: boolean;
  OldDatabase: _string;
  List: _TStringList;
begin
  Connection := FConnectDialog.Connection as TUniConnection;
  if not TUniUtils.CanGetProvider(Connection) then
    exit;

  DialogService := TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog));
  if DialogService.UseDatabaseHistory then
  {$IFNDEF LINUX}
    TUniConnectDialogUtils.LoadDatabaseListFromRegistry(TUniConnectDialog(FConnectDialog), edDatabase.Items)
  {$ENDIF}
  else begin
    OldLoginPrompt := Connection.LoginPrompt;
    OldCursor := Screen.Cursor;
    OldConnected := Connection.Connected;
    OldDatabase := Connection.Database;

    Connection.UserName := edUsername.Text;
    Connection.Password := edPassword.Text;
    Connection.Server := edServer.Text;
    if not OldConnected then
      Connection.Database := DialogService.GetDefaultDatabase;
    if edPort.Enabled then
      Connection.Port := StrToInt(edPort.Text);

    Connection.LoginPrompt := False;
    Screen.Cursor := crSQLWait;

    List := _TStringList.Create;

    try
      Connection.GetDatabaseNames(List);
      AssignStrings(List, edDatabase.Items);
    finally
      Connection.Connected := OldConnected;
      Connection.LoginPrompt := OldLoginPrompt;
      if not OldConnected then
        Connection.Database := OldDatabase;
      Screen.Cursor := OldCursor;
      List.Free;
    end;
  end;
end;

initialization
  if GetClass('TfmMyConnect') = nil then
    Classes.RegisterClass(TfmMyConnect);

{$IFDEF FPC}
{$I MyConnectForm.lrs}
{$ENDIF}
end.

