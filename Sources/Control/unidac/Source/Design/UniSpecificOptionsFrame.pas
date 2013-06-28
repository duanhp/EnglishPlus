{$IFNDEF CLR}

{$I UniDac.inc}

unit UniSpecificOptionsFrame;
{$ENDIF}

{$IFNDEF FPC}
{$DEFINE USE_VALEDIT}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, DacVcl, Buttons, Grids,
{$IFDEF USE_VALEDIT}
  ValEdit,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  MemUtils, DBAccess, UniProvider;

type
  TOptionsType = (otConnection, otSQL, otDataSet, otScript, otLoader, otDump,
    otAlerter);

  TOptionsMemo = class(TMemo);

  TUniSpecificOptionsFrame = class(TFrame)
    Panel1: TPanel;
    Label1: TLabel;
    edProvider: TComboBox;
    procedure edProviderChange(Sender: TObject);
  protected
    edOptions: {$IFDEF USE_VALEDIT}TValueListEditor{$ELSE}TOptionsMemo{$ENDIF};
    FEditorList: TStrings;

    FProviderName: string;
    FOptionsType: TOptionsType;
    FOptionsList: TOptionsList;
    FSpecificOptions: _TStrings;

  {$IFDEF USE_VALEDIT}
    procedure edOptionsGetPickList(Sender: TObject; const KeyName: String; Values: TStrings);
    procedure edOptionsValidate(Sender: TObject; ACol, ARow: Integer; const KeyName, KeyValue: String);
    procedure edOptionsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
  {$ENDIF}
    procedure edOptionsExit(Sender: TObject);

    function CheckOptionDefault(Option: TOption; var Value: _string): boolean;

  public
    constructor Create(Owner: TComponent); override;

    procedure SaveOptions;
    procedure LoadOptions(const ProviderName: string; OptionsType: TOptionsType; SpecificOptions: _TStrings);
  end;

implementation

uses
  Uni;

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniSpecificOptionsFrame.dfm}
{$ENDIF}
{$ENDIF}

constructor TUniSpecificOptionsFrame.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  
  UniProviders.GetProviderNames(edProvider.Items);

  edOptions := {$IFDEF USE_VALEDIT}TValueListEditor{$ELSE}TOptionsMemo{$ENDIF}.Create(Self);
  edOptions.Parent := Self;
  edOptions.Align := alClient;
  edOptions.OnExit := edOptionsExit;
{$IFDEF USE_VALEDIT}
  edOptions.OnGetPickList := edOptionsGetPickList;
  edOptions.OnValidate := edOptionsValidate;
  edOptions.OnDrawCell := edOptionsDrawCell;
  FEditorList := edOptions.Strings;
{$ELSE}
  FEditorList := edOptions.Lines;
{$ENDIF}
end;

function TUniSpecificOptionsFrame.CheckOptionDefault(Option: TOption; var Value: _string): boolean;
var
  DefaultValue: _string;
begin
  if Option.CheckValue(Value) then
    Value := Option.GetAsString(Option.GetAsNative(Value)); // normalize

  DefaultValue := Option.GetAsString(Option.GetDefaultValue);

  Result := Value = DefaultValue;
end;

procedure TUniSpecificOptionsFrame.LoadOptions(const ProviderName: string; OptionsType: TOptionsType;
  SpecificOptions: _TStrings);
var
  OptionValue: _string;
  i: integer;
  Provider: TUniProvider;
begin
{$IFDEF FPC}
  FEditorList := edOptions.Lines;
  FEditorList.NameValueSeparator := '=';
{$ENDIF}

  FProviderName := ProviderName;
  FOptionsType := OptionsType;
  FSpecificOptions := SpecificOptions;

  edProvider.Text := ProviderName;
  FEditorList.Clear;

  FOptionsList := nil;

  if ProviderName <> '' then begin
    Provider := UniProviders.GetProvider(ProviderName);
    if Provider = nil then
      exit;

    case OptionsType of
      otConnection:
        FOptionsList := Provider.GetConnectionOptions;
      otSQL:
        FOptionsList := Provider.GetSQLOptions;
      otDataSet:
        FOptionsList := Provider.GetDataSetOptions;
      otScript:
        FOptionsList := Provider.GetScriptOptions;
      otLoader:
        FOptionsList := Provider.GetLoaderOptions;
      otDump:
        FOptionsList := Provider.GetDumpOptions;
      otAlerter:
        FOptionsList := Provider.GetAlerterOptions;
    else
      Assert(False);
    end;

    for i := 0 to FOptionsList.Count - 1 do begin
      OptionValue := FSpecificOptions.Values[
        FProviderName + '.' + FOptionsList[i].OptionName];
      if OptionValue = '' then
        OptionValue := FOptionsList[i].GetAsString(FOptionsList[i].GetDefaultValue);

    {$IFDEF USE_VALEDIT} // FEditorList.Add causes AV
      edOptions.InsertRow(FOptionsList[i].OptionName, OptionValue, True);
    {$ELSE}
      FEditorList.Add(FOptionsList[i].OptionName + '=' + OptionValue);
    {$ENDIF}
    end;
  end;  
end;

procedure TUniSpecificOptionsFrame.SaveOptions;
var
  i, j: integer;
  Value, Name: _string;
  Option: TOption;
begin
{$IFDEF FPC}
  FEditorList := edOptions.Lines;
  FEditorList.NameValueSeparator := '=';
{$ENDIF}
  if FOptionsList <> nil then
    for i := 0 to FOptionsList.Count - 1 do begin
      Option := FOptionsList[i];
      Name := FProviderName + '.' + Option.OptionName;
      Value := FEditorList.Values[Option.OptionName];
      if not CheckOptionDefault(Option, Value) then
        FSpecificOptions.Values[Name] := Value
      else begin
        j := FSpecificOptions.IndexOfName(Name);
        if j >= 0 then
          FSpecificOptions.Delete(j);
      end;
    end;
end;

{$IFDEF USE_VALEDIT}
procedure TUniSpecificOptionsFrame.edOptionsGetPickList(Sender: TObject;
  const KeyName: String; Values: TStrings);
var
  List: _TStringList;
begin
  if FOptionsList <> nil then begin
    List := _TStringList.Create;
    try
      GetOptionValuesList(KeyName, FOptionsList, List);
      AssignStrings(List, Values);
    finally
      List.Free;
    end;
  end;
end;

procedure TUniSpecificOptionsFrame.edOptionsValidate(Sender: TObject; ACol,
  ARow: Integer; const KeyName, KeyValue: String);
var
  Option: TOption;
  OldValue: _string;
  j: integer;
begin
  if FOptionsList <> nil then begin
    Option := FOptionsList.OptionByName(KeyName);
    if Option <> nil then // Delphi bug : event can be called with KeyName = ''
      try
        Option.Validate(KeyValue);
      except
        j := FSpecificOptions.IndexOfName(FProviderName + '.' + KeyName);
        if j >= 0 then
          OldValue := FSpecificOptions.Values[FProviderName + '.' + KeyName]
        else
          OldValue := Option.GetAsString(Option.GetDefaultValue);

        FEditorList.Values[KeyName] := OldValue;
        raise;
      end;
  end;
end;

procedure TUniSpecificOptionsFrame.edOptionsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  Value, CellText: _string;
  Option: TOption;
begin
  with edOptions do begin
    if (FOptionsList <> nil) and (ARow > 0) and (ACol = 1) and not (gdSelected in State) then begin
      Option := FOptionsList[ARow - 1];
      Value := Trim(Cells[1, ARow]);

      if not CheckOptionDefault(Option, Value) then
        Canvas.Font.Style := [fsBold];
    end;

    CellText := Cells[ACol, ARow];
    Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, CellText);
  end;
end;
{$ENDIF}

procedure TUniSpecificOptionsFrame.edOptionsExit(Sender: TObject);
begin
  SaveOptions;
end;

procedure TUniSpecificOptionsFrame.edProviderChange(Sender: TObject);
begin
  SaveOptions;
  LoadOptions(edProvider.Text, FOptionsType, FSpecificOptions);
end;

initialization
{$IFDEF FPC}
{$I UniSpecificOptionsFrame.lrs}
{$ENDIF}

end.
