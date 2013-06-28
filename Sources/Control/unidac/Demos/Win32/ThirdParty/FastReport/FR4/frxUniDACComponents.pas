
{******************************************}
{                                          }
{             FastReport  4.0              }
{        UniDAC enduser components         }
{                                          }

// Created by: Devart
// E-mail: unidac@devart.com

{                                          }
{******************************************}

unit frxUniDACComponents;

interface

{$I frx.inc}

uses
  Windows, SysUtils, Classes, frxClass, frxCustomDB, DB, Uni,
  Graphics, UniDacVcl, frxDACComponents
{$IFDEF Delphi6}
, Variants
{$ENDIF}
{$IFDEF QBUILDER}
, fqbClass
{$ENDIF}
;

type
  TUniDACTable = class(TUniTable)
  protected
    procedure InitFieldDefs; override;
  end;

  TUniDACQuery = class(TUniQuery)
  protected
    procedure InitFieldDefs; override;
  end;

  TfrxUniDACComponents = class(TfrxDACComponents)
  private
    FOldComponents: TfrxDACComponents;
    function GetDefaultDatabase: TUniConnection;
    procedure SetDefaultDatabase(Value: TUniConnection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetDescription: string; override;

    class function GetComponentsBitmap: TBitmap; override;
    class function GetComponentsName: string; override;
    class function ResourceName: string; override;
    class function GetDatabaseClass: TfrxDACDatabaseClass; override;
    class function GetTableClass: TfrxDACTableClass; override;
    class function GetQueryClass: TfrxDACQueryClass; override;
  published
    property DefaultDatabase: TUniConnection read GetDefaultDatabase write SetDefaultDatabase;  
  end;

  TfrxUniDACDatabase = class(TfrxDACDatabase)
  protected
    function GetPort: integer;
    procedure SetPort(Value: integer);
    function GetDatabaseName: string; override;
    procedure SetDatabaseName(const Value: string); override;
    function GetProviderName: string;
    procedure SetProviderName(const Value: string);
    function GetSpecificOptions: TStrings;
    procedure SetSpecificOptions(Value: TStrings);

  public
    constructor Create(AOwner: TComponent); override;
    class function GetDescription: string; override;

  published
    property LoginPrompt;
    property DatabaseName;
    property Username;
    property Password;
    property Server;
    property Port: integer read GetPort write SetPort;
    property ProviderName: string read GetProviderName write SetProviderName;
    property Connected;
    property Params;
    property SpecificOptions: TStrings read GetSpecificOptions write SetSpecificOptions;
  end;

  TfrxUniDACTable = class(TfrxDACTable)
  private
    FTable: TUniDACTable;
  protected
    procedure SetDatabase(const Value: TfrxDACDatabase); override;
    procedure SetMaster(const Value: TDataSource); override;
    procedure SetMasterFields(const Value: string); override;
    procedure SetIndexFieldNames(const Value: string); override;
    function GetIndexFieldNames: string; override;
    function GetTableName: string; override;
    procedure SetTableName(const Value: string); override;
    function GetSpecificOptions: TStrings;
    procedure SetSpecificOptions(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    class function GetDescription: string; override;
    property Table: TUniDACTable read FTable;
  published
    property Database;
    property TableName: string read GetTableName write SetTableName;
    property SpecificOptions: TStrings read GetSpecificOptions write SetSpecificOptions;
  end;

  TfrxUniDACQuery = class(TfrxDACQuery)
  protected
    procedure SetDatabase(const Value: TfrxDACDatabase); override;
    function GetSpecificOptions: TStrings;
    procedure SetSpecificOptions(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    class function GetDescription: string; override;
{$IFDEF QBUILDER}
    function QBEngine: TfqbEngine; override;
{$ENDIF}
  published
    property Database;
    property IndexName;
    property MasterFields;
    property SpecificOptions: TStrings read GetSpecificOptions write SetSpecificOptions;
  end;

{$IFDEF QBUILDER}
  TfrxEngineUniDAC = class(TfrxEngineDAC)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadFieldList(const ATableName: string; var AFieldList: TfqbFieldList); override;
  end;
{$ENDIF}

var
  CatBmp: TBitmap;
  UniDACComponents: TfrxDACComponents;

implementation

{$R *.res}

uses
  frxUniDACRTTI,
{$IFNDEF NO_EDITORS}
  frxUniDACEditor,
{$ENDIF}
  frxDsgnIntf, frxRes;

{ TUniDACTable }

procedure TUniDACTable.InitFieldDefs;
begin
  if (TableName <> '') and (Assigned(Connection)) then
    inherited;
end;

{ TUniDACQuery }

procedure TUniDACQuery.InitFieldDefs;
begin
  if (SQL.Text <> '') and Assigned(Connection) then
    inherited;
end;

{ TfrxUniDACComponents }

constructor TfrxUniDACComponents.Create(AOwner: TComponent);
begin
  inherited;

  FOldComponents := UniDACComponents;
  UniDACComponents := Self;
end;

destructor TfrxUniDACComponents.Destroy;
begin
  if UniDACComponents = Self then
    UniDACComponents := FOldComponents;

  inherited;
end;

function TfrxUniDACComponents.GetDefaultDatabase: TUniConnection;
begin
  Result := TUniConnection(FDefaultDatabase);
end;

procedure TfrxUniDACComponents.SetDefaultDatabase(Value: TUniConnection);
begin
  FDefaultDatabase := Value;
end;

class function TfrxUniDACComponents.GetComponentsBitmap: TBitmap;
begin
  Result := CatBmp;
end;

class function TfrxUniDACComponents.GetComponentsName: string;
begin
  Result := 'UniDAC';
end;

class function TfrxUniDACComponents.GetDatabaseClass: TfrxDACDatabaseClass;
begin
  Result := TfrxUniDACDatabase;
end;

class function TfrxUniDACComponents.GetTableClass: TfrxDACTableClass;
begin
  Result := TfrxUniDACTable;
end;

class function TfrxUniDACComponents.GetQueryClass: TfrxDACQueryClass;
begin
  Result := TfrxUniDACQuery;
end;

class function TfrxUniDACComponents.ResourceName: string;
begin
  Result := 'FRXUNIDACOBJECTS';
end;

function TfrxUniDACComponents.GetDescription: string;
begin
  Result := 'UniDAC';
end;

{ TfrxUniDACDatabase }

constructor TfrxUniDACDatabase.Create(AOwner: TComponent);
begin
  inherited;

  FDatabase := TUniConnection.Create(nil);
  Component := FDatabase;
end;

class function TfrxUniDACDatabase.GetDescription: string;
begin
  Result := 'UniDAC Database';
end;

function TfrxUniDACDatabase.GetPort: integer;
begin
  Result := TUniConnection(FDatabase).Port;
end;

procedure TfrxUniDACDatabase.SetPort(Value: integer);
begin
  TUniConnection(FDatabase).Port := Value;
end;

function TfrxUniDACDatabase.GetDatabaseName: string;
begin
  Result := TUniConnection(FDatabase).Database;
end;

procedure TfrxUniDACDatabase.SetDatabaseName(const Value: string);
begin
  TUniConnection(FDatabase).Database := Value;
end;

function TfrxUniDACDatabase.GetProviderName: string;
begin
  Result := TUniConnection(FDatabase).ProviderName;
end;

procedure TfrxUniDACDatabase.SetProviderName(const Value: string);
begin
  TUniConnection(FDatabase).ProviderName := Value;
end;

function TfrxUniDACDatabase.GetSpecificOptions: TStrings;
begin
  Result := TUniConnection(FDatabase).SpecificOptions;
end;

procedure TfrxUniDACDatabase.SetSpecificOptions(Value: TStrings);
begin
  TUniConnection(FDatabase).SpecificOptions := Value;
end;

{ TfrxUniDACTable }

constructor TfrxUniDACTable.Create(AOwner: TComponent);
begin
  FTable := TUniDACTable.Create(nil);
  DataSet := FTable;
  SetDatabase(nil);
  inherited;
end;

class function TfrxUniDACTable.GetDescription: string;
begin
  Result := 'UniDAC Table';
end;

procedure TfrxUniDACTable.SetDatabase(const Value: TfrxDACDatabase);
begin
  inherited;
  
  if Value <> nil then
    FTable.Connection := TUniConnection(Value.Database)
  else
    if UniDACComponents <> nil then
      FTable.Connection := TUniConnection(UniDACComponents.DefaultDatabase)
    else
      FTable.Connection := nil;
end;

function TfrxUniDACTable.GetIndexFieldNames: string;
begin
  Result := FTable.IndexFieldNames;
end;

function TfrxUniDACTable.GetTableName: string;
begin
  Result := FTable.TableName;
end;

procedure TfrxUniDACTable.SetIndexFieldNames(const Value: string);
begin
  FTable.IndexFieldNames := Value;
end;

procedure TfrxUniDACTable.SetTableName(const Value: string);
begin
  FTable.TableName := Value;
  if Assigned(FTable.Connection) then
    FTable.InitFieldDefs;
end;

procedure TfrxUniDACTable.SetMaster(const Value: TDataSource);
begin
  FTable.MasterSource := Value;
end;

procedure TfrxUniDACTable.SetMasterFields(const Value: string);
var
  MasterNames: string;
  DetailNames: string;
begin
  GetMasterDetailNames(MasterFields, MasterNames, DetailNames);
  FTable.MasterFields := MasterNames;
  FTable.DetailFields := DetailNames;
end;

function TfrxUniDACTable.GetSpecificOptions: TStrings;
begin
  Result := FTable.SpecificOptions;
end;

procedure TfrxUniDACTable.SetSpecificOptions(Value: TStrings);
begin
  FTable.SpecificOptions := Value;
end;

{ TfrxUniDACQuery }

constructor TfrxUniDACQuery.Create(AOwner: TComponent);
begin
  FQuery := TUniDACQuery.Create(nil);
  inherited Create(AOwner);  
end;

class function TfrxUniDACQuery.GetDescription: string;
begin
  Result := 'UniDAC Query';
end;

procedure TfrxUniDACQuery.SetDatabase(const Value: TfrxDACDatabase);
begin
  inherited;
  
  if Value <> nil then
    FQuery.Connection := Value.Database
  else
    if UniDACComponents <> nil then
      FQuery.Connection := TUniConnection(UniDACComponents.DefaultDatabase)
    else
      FQuery.Connection := nil;
end;

function TfrxUniDACQuery.GetSpecificOptions: TStrings;
begin
  Result := (FQuery as TCustomUniDataSet).SpecificOptions;
end;

procedure TfrxUniDACQuery.SetSpecificOptions(Value: TStrings);
begin
  (FQuery as TCustomUniDataSet).SpecificOptions := Value;
end;

{$IFDEF QBUILDER}
function TfrxUniDACQuery.QBEngine: TfqbEngine;
begin
  Result := TfrxEngineUniDAC.Create(nil);
  TfrxEngineUniDAC(Result).FQuery.Connection := TUniConnection(FQuery.Connection);
end;
{$ENDIF}

{ TfrxEngineUniDAC }

{$IFDEF QBUILDER}
constructor TfrxEngineUniDAC.Create(AOwner: TComponent);
begin
  inherited;
  
  FQuery := TUniDACQuery.Create(Self);
end;

destructor TfrxEngineUniDAC.Destroy;
begin
  FQuery.Free;
  
  inherited;
end;

procedure TfrxEngineUniDAC.ReadFieldList(const ATableName: string;
  var AFieldList: TfqbFieldList);
var
  TempTable: TUniDACTable;
  Fields: TFieldDefs;
  i: Integer;
  tmpField: TfqbField;
begin
  AFieldList.Clear;
  TempTable := TUniDACTable.Create(Self);
  try
    TempTable.Connection := TUniConnection(FQuery.Connection);
    TempTable.TableName := ATableName;
    Fields := TempTable.FieldDefs;
    try
      TempTable.Active := True;
      tmpField:= TfqbField(AFieldList.Add);
      tmpField.FieldName := '*';
      for i := 0 to Fields.Count - 1 do begin
        tmpField := TfqbField(AFieldList.Add);
        tmpField.FieldName := Fields.Items[i].Name;
        tmpField.FieldType := Ord(Fields.Items[i].DataType)
      end;
    except
    end;
  finally
    TempTable.Free;
  end;
end;
{$ENDIF}

initialization
  CatBmp := TBitmap.Create;
  CatBmp.LoadFromResourceName(hInstance, TfrxUniDACComponents.ResourceName);
  RegisterDacComponents(TfrxUniDACComponents);

finalization
  UnRegisterDacComponents(TfrxUniDACComponents);
  CatBmp.Free;

end.
