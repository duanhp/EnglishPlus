
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}

unit UniAlerter;
{$ENDIF}

interface

uses
  Classes, SysUtils, Variants, DB,
  MemUtils, MemData, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF}, CRAccess, DBAccess,
  DAAlerter, Uni, UniProvider;

type

{ TUniAlerter }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniAlerter = class (TDAAlerter)
  private
    FSpecificOptions: TStringOptionsHolder;

    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
    function GetUniTransaction: TUniTransaction;
    procedure SetUniTransaction(Value: TUniTransaction);
    function GetSpecificOptions: _TStrings;
    procedure SetSpecificOptions(Value: _TStrings);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetInternalAlerterClass: TCRAlerterClass; override;
    procedure SetInternalAlerter(Value: TCRAlerter); override;
    procedure BeginConnection(WithTransaction: boolean = True); override;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  published
    property Connection: TUniConnection read GetConnection write SetConnection;
    property Transaction: TUniTransaction read GetUniTransaction write SetUniTransaction stored IsTransactionStored;
    property SpecificOptions: _TStrings read GetSpecificOptions write SetSpecificOptions;
    property Events;
    property Active;
    property AutoRegister;

    property OnEvent;
    property OnError;
  end;

implementation

uses
  DAConsts;

{ TUniAlerter }

constructor TUniAlerter.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FSpecificOptions := TStringOptionsHolder.Create;
end;

destructor TUniAlerter.Destroy;
begin
  FSpecificOptions.Free;

  inherited;
end;

procedure TUniAlerter.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TUniAlerter then begin
    TUniAlerter(Dest).SpecificOptions.Assign(SpecificOptions);
  end;
end;

function TUniAlerter.GetInternalAlerterClass: TCRAlerterClass;
begin
  if UsedConnection = nil then
    raise Exception.Create(SConnectionNotDefined);
  Result := TUniUtils.GetProvider(TUniConnection(UsedConnection)).GetAlerterClass;
end;

procedure TUniAlerter.SetInternalAlerter(Value: TCRAlerter);
begin
  inherited;

  if FIAlerter <> nil then begin
    TUniUtils.GetProvider(TUniConnection(UsedConnection)).SetObjectProps(
      FIAlerter, FSpecificOptions.Values, True);
  end;
end;

procedure TUniAlerter.BeginConnection(WithTransaction: boolean = True);
begin
  inherited;

  if FSpecificOptions.IsModified then begin
    TUniUtils.GetProvider(TUniConnection(UsedConnection)).SetObjectProps(
      FIAlerter, FSpecificOptions.Values, False);
    FSpecificOptions.IsModified := False;
  end;
end;

function TUniAlerter.GetConnection: TUniConnection;
begin
  Result := TUniConnection(inherited Connection);
end;

procedure TUniAlerter.SetConnection(Value: TUniConnection);
begin
  inherited Connection := Value;
end;

function TUniAlerter.GetUniTransaction: TUniTransaction;
begin
  Result := TUniTransaction(inherited Transaction);
end;

procedure TUniAlerter.SetUniTransaction(Value: TUniTransaction);
begin
  inherited Transaction := Value;
end;

function TUniAlerter.GetSpecificOptions: _TStrings;
begin
  Result := FSpecificOptions.Values;
end;

procedure TUniAlerter.SetSpecificOptions(Value: _TStrings);
begin
  FSpecificOptions.Values.Assign(Value);
end;

end.
 