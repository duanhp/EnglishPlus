
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}

unit UniDump;
{$ENDIF}

interface

uses
  Classes, SysUtils, Variants, DB,
  MemUtils, MemData, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF}, CRAccess, DBAccess,
  DADump, DAScript, Uni, UniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniDump = class(TDADump)
  private
    FSpecificOptions: TStringOptionsHolder;

    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
    function GetSpecificOptions: _TStrings;
    procedure SetSpecificOptions(Value: _TStrings);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetProcessorClass: TDADumpProcessorClass; override;
    procedure SetProcessor(Value: TDADumpProcessor); override;
    function GetTableNames: _string; override;
    procedure SetTableNames(Value: _string); override;
    function CreateScript: TDAScript; override;
    function GenerateHeader: _string; override;
    procedure BeginConnection; override;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  published
    property Connection: TUniConnection read GetConnection write SetConnection;
    property SpecificOptions: _TStrings read GetSpecificOptions write SetSpecificOptions;

    property Options;
  end;

implementation

uses
  DAConsts, UniScript;

function NamesFromList(List: _TStrings): _string;
var
  i: integer;
begin
  for i := 0 to List.Count - 1 do begin
    if i > 0 then
      Result := Result + ';';
    Result := Result + List[i];
  end;
end;

procedure NamesToList(Value: _string; List: _TStrings);
var
  Pos: integer;
  Name: _string;
begin
  List.Clear;
  Pos := 1;
  while Pos <= Length(Value) do begin
    Name := ExtractFieldName(Value, Pos);
    if Name <> '' then
      List.Add(Name);
  end;
end;

constructor TUniDump.Create(Owner: TComponent);
begin
  inherited;

  FSpecificOptions := TStringOptionsHolder.Create;
end;

destructor TUniDump.Destroy;
begin
  FSpecificOptions.Free;

  inherited;
end;

procedure TUniDump.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TUniDump then begin
    TUniDump(Dest).SpecificOptions.Assign(SpecificOptions);
  end;
end;

function TUniDump.GetProcessorClass: TDADumpProcessorClass;
begin
  if FConnection = nil then
    raise Exception.Create(SConnectionNotDefined);
  Result := TUniUtils.GetProvider(TUniConnection(FConnection)).GetDumpProcessorClass;
end;

procedure TUniDump.SetProcessor(Value: TDADumpProcessor);
begin
  inherited;

  if FProcessor <> nil then begin
    TUniUtils.GetProvider(TUniConnection(FConnection)).SetObjectProps(
      FProcessor, FSpecificOptions.Values, True);
  end;
end;

function TUniDump.GenerateHeader: _string;
var
  ProviderName: string;
begin
  ProviderName := TUniUtils.GetProvider(TUniConnection(FConnection)).GetProviderName;
  Result := _Format(SBHCaption, ['UniDAC', UniDacVersion,
    ProviderName, Connection.ServerVersion, ProviderName, Connection.ClientVersion,
    DateTimeToStr(Now), Connection.Server, Connection.Database]);
end;

procedure TUniDump.BeginConnection;
begin
  inherited;

  if FSpecificOptions.IsModified then begin
    TUniUtils.GetProvider(TUniConnection(FConnection)).SetObjectProps(
      FProcessor, FSpecificOptions.Values, False);
    FSpecificOptions.IsModified := False;
  end;
end;

function TUniDump.GetConnection: TUniConnection;
begin
  Result := TUniConnection(inherited Connection);
end;

procedure TUniDump.SetConnection(Value: TUniConnection);
begin
  inherited Connection := Value;
end;

function TUniDump.GetSpecificOptions: _TStrings;
begin
  Result := FSpecificOptions.Values;
end;

procedure TUniDump.SetSpecificOptions(Value: _TStrings);
begin
  FSpecificOptions.Values.Assign(Value);
end;

function TUniDump.GetTableNames: _string;
begin
  Result := NamesFromList(FTables);
end;

procedure TUniDump.SetTableNames(Value: _string);
begin
  NamesToList(Value, FTables);
end;

function TUniDump.CreateScript: TDAScript;
begin
  Result := TUniScript.Create(nil);
end;

end.
 