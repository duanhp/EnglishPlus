{$IFNDEF CLR}
{$I UniDac.inc}
unit UniSQLMonitor;
{$ENDIF}

interface

uses
  SysUtils, Classes, DB, DBAccess, DASQLMonitor;

type
  TUniSQLMonitorClass = class of TUniSQLMonitor;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniSQLMonitor = class(TCustomDASQLMonitor)
  protected
    class function GetMonitor: TCustomDASQLMonitor; override;
    procedure SetMonitor; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetCaption: string; override;

  published
    property Active;
    property Options;
    property DBMonitorOptions;
    property TraceFlags;
    property OnSQL;
  end;

implementation

uses
  Uni;

var
  UniMonitor: TUniSQLMonitor;

{ TUniSQLMonitor }

constructor TUniSQLMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TUniSQLMonitor.Destroy;
begin
  if UniMonitor = Self then
    UniMonitor := nil;

  inherited;
end;

class function TUniSQLMonitor.GetMonitor: TCustomDASQLMonitor;
begin
  Result := UniMonitor;
end;

class function TUniSQLMonitor.GetCaption: string;
begin
  Result := 'UniDAC';
end;

procedure TUniSQLMonitor.SetMonitor;
begin
  if UniMonitor = nil then
    UniMonitor := Self;
end;

end.
