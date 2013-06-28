{$IFNDEF CLR}

{$I UniDac.inc}

unit UniReg;
{$ENDIF}
interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  SysUtils, Classes, MemUtils, UniProvider, Uni, UniScript, UniSQLMonitor,
  Dialogs, UniDacVcl;

{$IFNDEF FPC}
type
  TUniProvidersLoader = class (TDAList)
  public
    destructor Destroy; override;

    procedure LoadProviders;
    procedure FreeProviders;
  end;
{$ENDIF}

procedure Register;

implementation

{$IFNDEF FPC}
{$IFNDEF CLR}
  {$IFDEF VER9}
    {$R UniDesign9.res}
  {$ELSE}
    {$R UniDesign.res}
  {$ENDIF}
  {$IFDEF VER10P}
    {$R UniDesign10p.res}
  {$ENDIF}
{$ENDIF}
{$ENDIF}

uses
{$IFDEF CLR}
  System.Reflection,
{$ENDIF}
  DB, DBAccess, DacReg, UniDump, UniLoader, UniAlerter;

{$IFNDEF FPC}
var
  ProvidersLoader: TUniProvidersLoader;

destructor TUniProvidersLoader.Destroy;
begin
  FreeProviders;

  inherited;
end;

procedure TUniProvidersLoader.LoadProviders;
var
  i: integer;
  List: TList;
{$IFNDEF CLR}
  DelphiVer: string;
{$ELSE}
  AssemblyInfo: string;
{$ENDIF}
  function LowerCaseEx(Value: string): string;
  begin
    Result := UpperCase(Copy(Value, 1, 1)) + LowerCase(Copy(Value, 2, Length(Value))); 
  end;

  procedure LoadProvider(PackageName, SiblingPackageName, ProviderUnitName: string);
  var
  {$IFNDEF CLR}
    ProviderHandle: HMODULE;
    RegisterProc: TProcedure;
  {$ELSE}
    AAssembly: Assembly;
    AType: System.Type;
  {$ENDIF}
  begin
  {$IFDEF CLR}
    try
      AAssembly := Assembly.Load(PackageName);
      AType := AAssembly.GetType(ProviderUnitName);
      AType.InvokeMember('RegisterComponent',
        BindingFlags.InvokeMethod or BindingFlags.Public or BindingFlags.Static, nil, nil, nil);
    except
    end;
  {$ELSE}
  {$IFDEF STD}
    try
      ProviderHandle := LoadPackage(SiblingPackageName);
      UnLoadPackage(ProviderHandle);
    except
      ProviderHandle := 0;
    end;
    if ProviderHandle = 0 then
      Exit; 
  {$ENDIF}
    try
      ProviderHandle := LoadPackage(PackageName);
    except
      ProviderHandle := 0;
    end;
    if ProviderHandle <> 0 then begin
      RegisterProc := GetProcAddress(ProviderHandle, PChar('@' + LowerCaseEx(ProviderUnitName) + '@' + 'RegisterComponent$qqrv'));
      if Assigned(RegisterProc) then begin
        RegisterProc;
        Add(Pointer(ProviderHandle));
      end
      else // old version of odac.bpl, ...
        UnloadPackage(ProviderHandle);
    end;
  {$ENDIF}
  end;

begin
  FreeProviders;

{$IFNDEF CLR}
  {$IFDEF VER6}
    DelphiVer := '60';
  {$ENDIF}
  {$IFDEF VER7}
    DelphiVer := '70';
  {$ENDIF}
  {$IFDEF VER9}
    DelphiVer := '90';
  {$ENDIF}
  {$IFDEF VER10}
    DelphiVer := '100';
  {$ENDIF}
  {$IFDEF VER11}
    DelphiVer := '105';
  {$ENDIF}
  {$IFDEF VER12}
    DelphiVer := '120';
  {$ENDIF}
  {$IFDEF VER14}
    DelphiVer := '140';
  {$ENDIF}
  {$IFDEF VER15}
    DelphiVer := '150';
  {$ENDIF}
  {$IFDEF VER16}
    DelphiVer := '160';
  {$ENDIF}
{$ELSE}
  AssemblyInfo := Assembly.GetExecutingAssembly.GetName.ToString;
  i := Pos(',', AssemblyInfo);
  if i > 0 then
    AssemblyInfo := AssemblyInfo.Substring(i - 1);
{$ENDIF}

  List := UniProviders.LockList;
  try
    for i := 0 to List.Count - 1 do
      with TUniProviderDesc(List[i]) do
      {$IFNDEF CLR}
        LoadProvider(PackageName + DelphiVer + '.bpl',
          LowerCase(SiblingProduct) + DelphiVer + '.bpl',   
          ProviderUnitName);
      {$ELSE}
        LoadProvider(AssemblyName + AssemblyInfo, '', AssemblyName + '.Units.' + ProviderUnitName);
      {$ENDIF}
  finally
    UniProviders.UnlockList;
  end;
end;

procedure TUniProvidersLoader.FreeProviders;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do begin
  {$IFDEF CLR}
  {$ELSE}
    UnloadPackage(Cardinal(Items[i]));
  {$ENDIF}
    Delete(i);
  end;
end;
{$ENDIF}

procedure Register;
begin
{$IFNDEF STD}
  RegisterCRBatchMove;
{$ENDIF}

  RegisterComponents('UniDAC', [TUniConnection]);
  RegisterComponents('UniDAC', [TUniQuery]);
  RegisterComponents('UniDAC', [TUniTable]);
  RegisterComponents('UniDAC', [TUniStoredProc]);
  RegisterComponents('UniDAC', [TUniSQL]);
  RegisterComponents('UniDAC', [TUniScript]);
  RegisterComponents('UniDAC', [TUniMetaData]);
  RegisterComponents('UniDAC', [TUniUpdateSQL]);
  RegisterComponents('UniDAC', [TUniDataSource]);
  RegisterComponents('UniDAC', [TUniLoader]);
  RegisterComponents('UniDAC', [TUniDump]);
  RegisterComponents('UniDAC', [TUniAlerter]);
  RegisterComponents('UniDAC', [TUniSQLMonitor]);
  RegisterComponents('UniDAC', [TUniConnectDialog]);
  RegisterComponents('UniDAC', [TUniTransaction]);

{$IFNDEF FPC}
  if ProvidersLoader = nil then
    ProvidersLoader := TUniProvidersLoader.Create;
  ProvidersLoader.LoadProviders;
{$ENDIF}
end;

initialization
{$IFDEF FPC}
{$I UniDesign.lrs}
{$ENDIF}

{$IFNDEF FPC}
  ProvidersLoader := nil;
{$ENDIF}

finalization
{$IFNDEF FPC}
  ProvidersLoader.Free;
{$ENDIF}
end.
