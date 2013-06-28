
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  UniDAC Params Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I UniDac.inc}

unit UniParamsFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  Classes, SysUtils, Variants, DB,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess, Uni,
  CRFrame, CRTabEditor, DAParamsFrame;

type
  TUniParamsFrame = class(TDAParamsFrame)
    cbNational: TCheckBox;
    lbNational: TLabel;
  protected
    procedure ItemToControls(Item: TCollectionItem); override;
    procedure ControlsToItem(Item: TCollectionItem); override;
    procedure UpdateControlsState; override;

  public
   constructor Create(AOwner: TComponent); override;

  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniParamsFrame.dfm}
{$ENDIF}
{$ENDIF}

uses
  CRColFrame, CREditor;

{ TUniParamsFrame }

constructor TUniParamsFrame.Create(AOwner: TComponent);
begin
  inherited;

  AddDataType('Unknown',       ftUnknown,       True,  False, False, '');
  AddDataType('String',        ftString,        False, True,  True,  '');
  AddDataType('WideString',    ftWideString,    False, True,  True,  '');
  AddDataType('Smallint',      ftSmallint,      True,  True,  False, '0');
  AddDataType('Integer',       ftInteger,       True,  True,  False, '0');
  AddDataType('Word',          ftWord,          True,  True,  False, '0');
  AddDataType('LargeInt',      ftLargeInt,      True,  True,  False, '0');
  AddDataType('Boolean',       ftBoolean,       False, True,  True,  'False');
  AddDataType('Float',         ftFloat,         True,  True,  False, '0');
  AddDataType('Currency',      ftCurrency,      True,  True,  False, '0');
  AddDataType('BCD',           ftBCD,           True,  True,  False, '0');
{$IFNDEF FPC}
  AddDataType('FMTBcd',        ftFMTBcd,        True,  True,  False, '0');
{$ENDIF}

  AddDataType('Date',          ftDateTime,      True,  True,  False, '');
  AddDataType('Time',          ftTime,          True,  True,  False, '');
  AddDataType('DateTime',      ftDateTime,      True,  True,  False, '');
{$IFNDEF FPC}
  AddDataType('TimeStamp',     ftTimeStamp,     True,  True,  False, '');
{$ENDIF}

  AddDataType('FixedChar',     ftFixedChar,     True,  True,  False, '');
  AddDataType('FixedWideChar', TFieldType(ftFixedWideChar), True,  True,  False, '');

  AddDataType('Memo',          ftMemo,          True,  True,  False, '');
{$IFDEF VER10P}
  AddDataType('WideMemo',      ftWideMemo,      True,  True,  False, '');
{$ENDIF}
  AddDataType('Blob',          ftBlob,          True,  True,  False, '');
  AddDataType('OraClob',       ftOraClob,       True,  True,  False, '');
  AddDataType('OraBlob',       ftOraBlob,       True,  True,  False, '');

  AddDataType('Bytes',         ftBytes,         False, False, True,  '');
  AddDataType('VarBytes',      ftVarBytes,      False, False, True,  '');

  AddDataType('Cursor',        ftCursor,        True,  False, False, '');
  AddDataType('GUID',          ftGuid,          False, False, False, '');
  AddDataType('Variant',       ftVariant,       False, False, False, '');
//  AddDataType('Object',        ftObject,        True,  False,  False, '');
//  AddDataType('XML',           TFieldType(ftXML), True,  True,   False, '');
//  AddDataType('Reference',     ftReference,     True,  False,  False, '');
//  AddDataType('Array',         ftArray,         True,  False,  False, '');
//  AddDataType('Table',         ftTable,         True,  True,   False, '');
//  AddDataType('BFile',         TFieldType(ftBFile), False, False,  False, '');

  AddParamType('IN', ptInput);
  AddParamType('OUT', ptOutput);
  AddParamType('IN/OUT', ptInputOutput);
  AddParamType('RESULT', ptResult);
end;

procedure TUniParamsFrame.ItemToControls(Item: TCollectionItem);
begin
  inherited;

  if cbNational.Enabled then
    cbNational.Checked := TUniParam(Item).National;
end;

procedure TUniParamsFrame.ControlsToItem(Item: TCollectionItem);
begin
  inherited;

  TUniParam(Item).National := cbNational.Checked;
end;

procedure TUniParamsFrame.UpdateControlsState;
begin
  inherited;

  cbNational.Enabled := cbNullValue.Enabled;
end;

initialization
{$IFDEF FPC}
{$I UniParamsFrame.lrs}
{$ENDIF}

end.
