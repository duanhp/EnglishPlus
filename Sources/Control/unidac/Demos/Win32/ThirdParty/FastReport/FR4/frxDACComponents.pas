
{******************************************}
{                                          }
{             FastReport v4.0             }
{         DAC enduser components           }
{                                          }

// Created by: Devart
// E-mail: support@devart.com

{                                          }
{******************************************}

unit frxDACComponents;

interface

{$I frx.inc}

uses
  Windows, SysUtils, Classes, frxClass, frxCustomDB, DB, DBAccess, Graphics
{$IFDEF Delphi6}
, Variants
{$ENDIF}
 {$IFDEF QBUILDER}
, fqbClass
 {$ENDIF}
;

type

  TfrxDACComponentsClass = class of TfrxDACComponents;
  TfrxDACDatabaseClass = class of TfrxDACDatabase;
  TfrxDACTableClass = class of TfrxDACTable;
  TfrxDACQueryClass = class of TfrxDACQuery;

  TfrxDACComponents = class(TfrxDBComponents)
  protected
    FDefaultDatabase: TCustomDAConnection;
  public
    function GetDescription: string; override;

    class function GetComponentsBitmap: TBitmap; virtual;
    class function GetComponentsName: string; virtual;
    class function ResourceName: string; virtual;
    class function GetDatabaseClass: TfrxDACDatabaseClass; virtual;
    class function GetTableClass: TfrxDACTableClass; virtual;
    class function GetQueryClass: TfrxDACQueryClass; virtual;

    property DefaultDatabase: TCustomDAConnection read FDefaultDatabase write FDefaultDatabase;
  end;

  TfrxDACDatabase = class(TfrxCustomDatabase)
  protected
    FDatabase: TCustomDAConnection;
    FParams: Tstrings;
    FStreamedConnected: Boolean;
    function GetLoginPrompt: Boolean; override;
    procedure SetLoginPrompt(Value: Boolean); override;
    function GetUsername: string;
    procedure SetUsername(const Value: string);
    function GetPassword: string;
    procedure SetPassword(const Value: string);
    function GetServer: string;
    procedure SetServer(const Value: string);
    function GetConnected: Boolean; override;
    procedure SetConnected(Value: Boolean); override;
    function GetParams: Tstrings; override;
    procedure SetParams(Value: Tstrings); override;
  protected
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetDescription: string; override;
    procedure SetLogin(const Login, Password: string); override;
    property Database: TCustomDAConnection read FDatabase write FDatabase;
    property Username: string read GetUsername write SetUsername;
    property Password: string read GetPassword write SetPassword;
    property Server: string read GetServer write SetServer;
    Property Params: Tstrings read GetParams write SetParams;
  end;

  TfrxDACTable = class(TfrxCustomTable)
  private
    FDatabase: TfrxDACDatabase;
  protected
    procedure SetDatabase(const Value: TfrxDACDatabase); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    constructor DesignCreate(AOwner: TComponent; Flags: Word); override;
    class function GetDescription: string; override;
    procedure BeforeStartReport; override;
    property Database: TfrxDACDatabase read FDatabase write SetDatabase;
  end;

  TfrxDACQuery = class(TfrxCustomQuery)
  private
    FDatabase: TfrxDACDatabase;
  protected
    FQuery: TCustomDADataSet;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetMaster(const Value: TDataSource); override;
    procedure SetSQL(Value: Tstrings); override;
    function GetSQL: Tstrings; override;
    procedure SetDatabase(const Value: TfrxDACDatabase); virtual;
    procedure SetIndexName(const Value: string);
    function GetIndexName: string;
    procedure SetMasterFields(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor DesignCreate(AOwner: TComponent; Flags: Word); override;
    class function GetDescription: string; override;
    procedure BeforeStartReport; override;
    procedure UpdateParams; override;
    property Query: TCustomDADataSet read FQuery;
    property Database: TfrxDACDatabase read FDatabase write SetDatabase;
    property IndexName: string read GetIndexName write SetIndexName;
  end;

 {$IFDEF QBUILDER}
  TfrxEngineDAC = class(TfqbEngine)
  protected
    FQuery: TCustomDADataSet;
  public
    procedure ReadTableList(ATableList: Tstrings); override;
    function ResultDataSet: TDataSet; override;
    procedure SetSQL(const Value: string); override;
  end;
 {$ENDIF}

{$IFNDEF Delphi6}
  procedure SetDelimitedText(strings: Tstrings; Delimiter:Char; const Value: string);
{$ENDIF}
  procedure GetMasterDetailNames(const Value: string; var MasterNames: string; var DetailNames: string);
  procedure RegisterDacComponents(Components: TfrxDACComponentsClass);
  procedure UnRegisterDacComponents(Components: TfrxDACComponentsClass);

implementation

uses
{$IFNDEF NO_EDITORS}
  frxDACEditor,
{$ENDIF}
  frxDsgnIntf, frxRes,
  SysConst;

procedure RegisterDacComponents(Components: TfrxDACComponentsClass);
begin
  frxObjects.RegisterCategory(Components.GetComponentsName, Components.GetComponentsBitmap, Components.GetComponentsName + ' Components');
  frxObjects.RegisterObject1(Components.GetDatabaseClass, nil, '', Components.GetComponentsName, 0, 37);
  frxObjects.RegisterObject1(Components.GetTableClass, nil, '', Components.GetComponentsName, 0, 38);
  frxObjects.RegisterObject1(Components.GetQueryClass, nil, '', Components.GetComponentsName, 0, 39);
end;

procedure UnRegisterDacComponents(Components: TfrxDACComponentsClass);
begin
  frxObjects.UnRegister(Components.GetDatabaseClass);
  frxObjects.UnRegister(Components.GetTableClass);
  frxObjects.UnRegister(Components.GetQueryClass);
end;

{$IFNDEF Delphi6}
procedure SetDelimitedText(strings: Tstrings; Delimiter:Char; const Value: string);
var
  P, P1: PChar;
  S: string;
begin
  with strings do begin
    BeginUpdate;
    try
      Clear;
      P := PChar(Value);
      while P^ in [#1..' '] do
      {$IFDEF MSWINDOWS}
        P := CharNext(P);
      {$ELSE}
        Inc(P);
      {$ENDIF}
      while P^ <> #0 do begin
        if P^ = '"' then
          S := AnsiExtractQuotedStr(P, '"')
        else begin
          P1 := P;
          while (P^ > ' ') and (P^ <> Delimiter) do
          {$IFDEF MSWINDOWS}
            P := CharNext(P);
          {$ELSE}
            Inc(P);
          {$ENDIF}
          Setstring(S, P1, P - P1);
        end;
        Add(S);
        while P^ in [#1..' '] do
        {$IFDEF MSWINDOWS}
          P := CharNext(P);
        {$ELSE}
          Inc(P);
        {$ENDIF}
        if P^ = Delimiter then begin
          P1 := P;
          {$IFDEF MSWINDOWS}
          if CharNext(P1)^ = #0 then
          {$ELSE}
          Inc(P1);
          if P1^ = #0 then
          {$ENDIF}
            Add('');
          repeat
            {$IFDEF MSWINDOWS}
            P := CharNext(P);
            {$ELSE}
            Inc(P);
            {$ENDIF}
          until not (P^ in [#1..' ']);
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;
{$ENDIF}

procedure GetMasterDetailNames(const Value: string; var MasterNames: string; var DetailNames: string);
var
  List: TstringList;
  i: integer;
begin
  List := TstringList.Create;
  try
{$IFNDEF Delphi6}
    SetDelimitedText(List, ';', Value);
{$ELSE}
    List.Delimiter := ';';
    List.DelimitedText := Value;
{$ENDIF}
    MasterNames := '';
    DetailNames := '';
    for i := 0 to List.Count - 1 do begin
      if MasterNames <> '' then
        MasterNames := MasterNames + ';';
      if DetailNames <> '' then
        DetailNames := DetailNames + ';';
      MasterNames := MasterNames + List.Values[List.Names[i]];
      DetailNames := DetailNames + List.Names[i];
    end;
  finally
    List.Free;
  end;
end;

{ TfrxDACComponents }

class function TfrxDACComponents.GetComponentsBitmap: TBitmap;
begin
  Result := nil;
end;

class function TfrxDACComponents.GetComponentsName: string;
begin
  result := 'DAC';
end;

class function TfrxDACComponents.GetDatabaseClass: TfrxDACDatabaseClass;
begin
  Raise EAbstractError.Create(SAbstractError);
end;

function TfrxDACComponents.GetDescription: string;
begin
  Result := 'DAC';
end;

class function TfrxDACComponents.GetQueryClass: TfrxDACQueryClass;
begin
  Raise EAbstractError.Create(SAbstractError);
end;

class function TfrxDACComponents.GetTableClass: TfrxDACTableClass;
begin
  Raise EAbstractError.Create(SAbstractError);
end;

class function TfrxDACComponents.ResourceName: string;
begin
  Raise EAbstractError.Create(SAbstractError);
end;

{ TfrxDACDatabase }

function TfrxDACDatabase.GetParams: Tstrings;
begin
  Result := FParams;
end;

procedure TfrxDACDatabase.SetParams(Value: Tstrings);
begin
  FParams := Value;
end;

constructor TfrxDACDatabase.Create(AOwner: TComponent);
begin
  inherited;
  FParams := TstringList.Create;
end;

destructor TfrxDACDatabase.Destroy;
begin
  inherited;
end;

class function TfrxDACDatabase.GetDescription: string;
begin
  Result := 'DAC Database';
end;

function TfrxDACDatabase.GetConnected: Boolean;
begin
  Result := FDatabase.Connected;
end;

function TfrxDACDatabase.GetUsername: string;
begin
  Result := FDatabase.Username;
end;

function TfrxDACDatabase.GetPassword: string;
begin
  Result := FDatabase.Password
end;

function TfrxDACDatabase.GetServer: string;
begin
  Result := FDatabase.Server;
end;

function TfrxDACDatabase.GetLoginPrompt: Boolean;
begin
  Result := FDatabase.LoginPrompt;
end;

procedure TfrxDACDatabase.Loaded;
begin
  inherited;

  try
    try
      if FStreamedConnected then
        SetConnected(True);
    except
      on E: Exception do
        if csDesigning in ComponentState then
          ShowException(E, ExceptAddr)
        else
          raise;
    end;
  finally
    FStreamedConnected := False;
  end;
end;

procedure TfrxDACDatabase.SetConnected(Value: Boolean);
begin
  if IsLoading then
    FStreamedConnected := Value
  else begin
    BeforeConnect(Value);
    FDatabase.Connected := Value;
  end;
end;

procedure TfrxDACDatabase.SetUsername(const Value: string);
begin
  FDatabase.Username := Value;
end;

procedure TfrxDACDatabase.SetPassword(const Value: string);
begin
  FDatabase.Password := Value;
end;

procedure TfrxDACDatabase.SetServer(const Value: string);
begin
  FDatabase.Server := Value;
end;

procedure TfrxDACDatabase.SetLoginPrompt(Value: Boolean);
begin
  FDatabase.LoginPrompt := Value;
end;

procedure TfrxDACDatabase.SetLogin(const Login, Password: string);
begin
  Self.UserName := Login;
  Self.Password := Password;
end;

{ TfrxDACTable }

constructor TfrxDACTable.Create(AOwner: TComponent);
begin
  SetDatabase(nil);
  inherited;
end;

constructor TfrxDACTable.DesignCreate(AOwner: TComponent; Flags: Word);
var
  i: Integer;
  l: TList;
begin
  inherited;
  l := Report.AllObjects;
  for i := 0 to l.Count - 1 do
    if TObject(l[i]) is TfrxDACDatabase then
    begin
      Database := TfrxDACDatabase(l[i]);
      break;
    end;
end;

class function TfrxDACTable.GetDescription: string;
begin
  Result := 'DAC Table';
end;

procedure TfrxDACTable.BeforeStartReport;
begin
  SetDatabase(FDatabase);
end;

procedure TfrxDACTable.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDatabase) then
    SetDatabase(nil);
end;

procedure TfrxDACTable.SetDatabase(const Value: TfrxDACDatabase);
begin
  FDatabase := Value;
end;

{ TfrxDACQuery }

constructor TfrxDACQuery.Create(AOwner: TComponent);
begin
  Dataset := FQuery;
  Database := nil;
  inherited Create(AOwner);
end;

constructor TfrxDACQuery.DesignCreate(AOwner: TComponent; Flags: Word);
var
  i: Integer;
  l: TList;
begin
  inherited;
  l := Report.AllObjects;
  for i := 0 to l.Count - 1 do
    if TObject(l[i]) is TfrxDACDatabase then
    begin
      SetDatabase(TfrxDACDatabase(l[i]));
      break;
    end;
end;

class function TfrxDACQuery.GetDescription: string;
begin
  Result := 'DAC Query';
end;

procedure TfrxDACQuery.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDatabase) then
    SetDatabase(nil);
end;

procedure TfrxDACQuery.SetDatabase(const Value: TfrxDACDatabase);
begin
  FDatabase := Value;
end;

procedure TfrxDACQuery.SetIndexName(const Value: string);
begin
  FQuery.IndexFieldNames := Value;
end;

function TfrxDACQuery.GetIndexName: string;
begin
  Result := FQuery.IndexFieldNames;
end;

function TfrxDACQuery.GetSQL: Tstrings;
begin
  Result := FQuery.SQL;
end;

procedure TfrxDACQuery.SetSQL(Value: Tstrings);
begin
  FQuery.SQL := Value;
end;

procedure TfrxDACQuery.SetMaster(const Value: TDataSource);
begin
  FQuery.MasterSource := Value;
end;

procedure TfrxDACQuery.BeforeStartReport;
begin
  SetDatabase(FDatabase);
  { needed to update parameters }
  SQL.Text := SQL.Text;
end;

procedure TfrxDACQuery.UpdateParams;
var
  i: integer;
begin                                                                  // Bug with ftCursor datatype
  for i := 0 to Params.Count - 1 do                                    // Problem with calling static method
    if FQuery.Params.FindParam(Params[i].Name) <> nil then             // TParam.SetDataType instead TUniParam.SetDataType
      FQuery.ParamByName(Params[i].Name).DataType := Params[i].DataType;

  frxParamsToTParams(Self, FQuery.Params);
end;

procedure TfrxDACQuery.SetMasterFields(const Value: string);
var
  MasterNames: string;
  DetailNames: string;
begin
  GetMasterDetailNames(MasterFields, MasterNames, DetailNames);
  FQuery.MasterFields := MasterNames;
  FQuery.DetailFields := DetailNames;
end;

 {$IFDEF QBUILDER}

{ TfrxEngineDAC }

procedure TfrxEngineDAC.ReadTableList(ATableList: Tstrings);
begin
  ATableList.Clear;
  FQuery.Connection.GetTableNames(ATableList);
end;

function TfrxEngineDAC.ResultDataSet: TDataSet;
begin
  Result := FQuery;
end;

procedure TfrxEngineDAC.SetSQL(const Value: string);
begin
  FQuery.SQL.Text := Value;
end;
 {$ENDIF}

end.

