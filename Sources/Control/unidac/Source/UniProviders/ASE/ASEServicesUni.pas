
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ASEDac.inc}
unit ASEServicesUni;
{$ENDIF}

interface

uses
{$IFNDEF CLR}
  CLRClasses,
{$ELSE}
  System.Text,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows, Registry, IniFiles,
{$ENDIF}
  SysUtils, Classes, Variants, DB,
  MemUtils, MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  CRParser, CRAccess, DBAccess, DAScript, DADump,
{$IFNDEF UNIDACPRO}
  ODBCClasses, ODBCParser, ODBCCall, ODBCServices, ASEClasses,
  ASEParser;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCCallUni, ODBCServicesUni, ASEClassesUni,
  ASEParserUni;
{$ENDIF}

type

{  TCustomASEDataSetUpdater }

  TCustomASEDataSetUpdater = class(TCustomODBCDataSetUpdater)
  protected
    function GetIdentityFieldValue(var Value: variant): boolean; override;
  end;

{ TCustomASEDataSetService }

  TCustomASEDataSetService = class(TCustomODBCDataSetService)
  protected
    procedure CreateDataSetUpdater; override;
    function KeysFromSpecialColumns: boolean; override;
    function DetectIdentityField: TCRFieldDesc; override;
    function IdentityFieldIsData: boolean; override;
    function GetMetaDataRecordSet: TCRRecordSet; override;
  end;

{ TASEScriptProcessor }

  TASEScriptProcessor = class (TODBCScriptProcessor)
  private
    FPrevLexem: integer;

  protected
    function ExecuteNext: boolean; override;
    function GetParserClass: TSQLParserClass; override;
    procedure CheckLexem(Code: integer; var StatementType: integer; var Omit: boolean); override;
    function GetReady(Code: integer): boolean; override;
  end;

{ TASEServerEnumerator }

  TASEServerEnumerator = class (TCRServerEnumerator)
  public
    procedure GetServerList(List: _TStrings); override;
  end;

implementation

uses
  DAConsts;

{ TCustomASEDataSetUpdater }

function TCustomASEDataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
  Value := SelectDBValue('Get identity value', 'SELECT @@IDENTITY');
  Result := True;
end;

{ TCustomASEDataSetService }

procedure TCustomASEDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomASEDataSetUpdater.Create(Self));
end;

function TCustomASEDataSetService.KeysFromSpecialColumns: boolean;
begin
  Result := True;
end;

function TCustomASEDataSetService.DetectIdentityField: TCRFieldDesc;
var
  i: integer;
  FieldDesc: TCRFieldDesc;
  RecordSet: TCRRecordSet;
begin
  RecordSet := GetIRecordSet;
  for i := 0 to RecordSet.FieldCount - 1 do begin
    FieldDesc := TCRFieldDesc(RecordSet.Fields[i]);
    if FieldDesc.IsAutoIncrement and (FieldDesc.TableInfo = UpdatingTableInfo) then begin
      Result := FieldDesc;
      exit;
    end;
  end;
  Result := nil;
end;

function TCustomASEDataSetService.IdentityFieldIsData: boolean;
begin
  Result := True;
end;

function TCustomASEDataSetService.GetMetaDataRecordSet: TCRRecordSet;
begin
  Result := inherited GetMetaDataRecordSet;
  if (GetICommand <> nil) and (GetICommand.GetTransaction <> nil) and GetICommand.GetTransaction.GetInTransaction then
    Result.SetConnection(TASEConnection(GetIConnection).GetServiceConnection);
end;

{ TASEScriptProcessor }

function TASEScriptProcessor.ExecuteNext: boolean;
begin
  FPrevLexem := 0;
    
  Result := inherited ExecuteNext;
end;

function TASEScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TASEParser;
end;

procedure TASEScriptProcessor.CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean);
begin
  if (FPrevLexem = lxBEGIN) and (Code <> lxTRANSACTION) then
    StatementType := ST_SPECIFIC_SQL;

  FPrevLexem := Code;
end;

function TASEScriptProcessor.GetReady(Code: integer): boolean;
begin
  Result := Code = lxGO;
end;

{ TASEServerEnumerator }

procedure TASEServerEnumerator.GetServerList(List: _TStrings);
{$IFDEF MSWINDOWS}
var
  List1: TStringList;
  Reg: TRegistry;
  RootPath: String;
  ASEIni:TIniFile;
  IniSection, IniParams: TStringList;
  i: integer;
  ParamName: string;
{$ENDIF}
begin
  List.Clear;
{$IFDEF MSWINDOWS}
  List1:= TStringList.create;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('SOFTWARE\SYBASE\Setup') then
    begin
      RootPath := Reg.ReadString('SYBASE');
      if FileExists(RootPath+'\ini\sql.ini') then
      begin
        ASEIni := TIniFile.Create(RootPath+'\ini\sql.ini');
        IniSection:= TStringList.Create;
        try
          ASEIni.ReadSections(IniSection);
          for i:= 0 to IniSection.Count - 1 do
          begin
            if ASEIni.ValueExists(IniSection[i],'master') then
              ParamName:= 'master'
            else if ASEIni.ValueExists(IniSection[i],'query') then
              ParamName:= 'query'
            else
              exit;
            IniParams:= TStringList.Create;
            try
              IniParams.Delimiter:= ',';
              IniParams.DelimitedText :=  ASEIni.ReadString(IniSection[i],ParamName,'');
              List1.AddObject(IniParams[1],TObject(StrToInt(IniParams[2])));
            finally
              IniParams.Free;
            end;
          end;
          AssignStrings(List1, List);
        finally
          IniSection.Free;
          ASEIni.Free;
        end;
      end;
    end;
  finally
    Reg.Free;
    List1.Free;
  end;
{$ENDIF}
end;

end.
