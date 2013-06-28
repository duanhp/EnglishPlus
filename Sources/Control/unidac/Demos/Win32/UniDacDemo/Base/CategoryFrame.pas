{$I DacDemo.inc}

unit CategoryFrame;

interface

uses
{$IFDEF MSWINDOWS}
  ShellApi, Windows,
{$ENDIF}
{$IFDEF KYLIX}
  QControls, QStdCtrls, QComCtrls, QGraphics, QForms,
{$ELSE}
  Controls, StdCtrls, Forms,
{$IFDEF WEB}
  OleCtrls, SHDocVw, mshtml, ActiveX,
{$ELSE}
  Graphics, Types,
{$ENDIF}
{$ENDIF}
{$IFNDEF VER130}
  Variants,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  SysUtils, Classes, DemoFrame;

type
{$IFNDEF WEB}
  TOnNavigate = procedure (DemoDescription: string) of object;
{$ELSE}
  TOnNavigate = procedure (Index: integer) of object;
{$ENDIF}

  TCategoryFrame = class(TDemoFrame)
  protected
    FOnNavigate: TOnNavigate;
    FDemosDescription: {$IFNDEF WEB}TListBox{$ELSE}TWebBrowser{$ENDIF};
    FTempFileName: string;  // for Win98 problem avoidance
  public
    procedure SetDemoDescriptions(Descriptions: TStrings);
    constructor Create(AOwner: TComponent); override;
  {$IFNDEF WEB}
    procedure DemosDescriptionDblClick(Sender: TObject);
    procedure DemoDescriptionMouseMove(Sender: TObject;
        Shift: TShiftState; X, Y: Integer);
  {$ELSE}
    procedure DemosDescriptionBeforeNavigate2(Sender: TObject;
       const pDisp: IDispatch; {$IFDEF VER230}const{$ELSE}var{$ENDIF} URL, Flags,
       TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
  {$ENDIF}
    property OnNavigate: TOnNavigate read FOnNavigate write FOnNavigate;
  {$IFNDEF WEB}
    property DemosDescription: TListBox read FDemosDescription;
  {$ENDIF}
  end;

implementation

{$IFNDEF FPC}
{$IFDEF CLR}
{$R *.nfm}
{$ENDIF}
{$IFDEF WIN32_64}
{$R *.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}
{$ENDIF}

{$IFDEF VER130}
function TryStrToInt(const S: string; out Value: Integer): boolean;
begin
  Result := True;
  try
    Value := StrToInt(S);
  except
    Result := False
  end;
end;
{$ENDIF}

constructor TCategoryFrame.Create(AOwner: TComponent);
begin
  inherited;
{$IFNDEF WEB}
  FDemosDescription := TListBox.Create(self);
{$IFDEF LINUX}
  FDemosDescription.Font.Name := 'adobe-courier';
  FDemosDescription.Font.Pitch := fpFixed;
{$ENDIF}
{$IFDEF CLR}
  FDemosDescription.Font.Name := 'courier new';
  FDemosDescription.Font.Size := 11;
{$ENDIF}
  FDemosDescription.OnDblClick := DemosDescriptionDblClick;
  FDemosDescription.OnMouseMove := DemoDescriptionMouseMove;
{$ELSE}
  FDemosDescription := TWebBrowser.Create(self);
  FDemosDescription.Navigate('about:blank'); // to create document object
  FDemosDescription.OnBeforeNavigate2 := DemosDescriptionBeforeNavigate2;
{$ENDIF}
  InsertControl(FDemosDescription);
  FDemosDescription.Align := alClient;
end;

{$IFNDEF WEB}

procedure TCategoryFrame.SetDemoDescriptions(Descriptions: TStrings);
begin
  FDemosDescription.Items.Assign(Descriptions);
end;

procedure TCategoryFrame.DemoDescriptionMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  n: integer;
begin
  n := FDemosDescription.ItemAtPos(Point(X, Y), True);
  If (n = -1) or (Trim(FDemosDescription.Items[n]) = '') or (FDemosDescription.Items[n][1] = ' ') then
    FDemosDescription.Cursor := crDefault
  else
    FDemosDescription.Cursor := crHandPoint;
end;

// Navigation
procedure TCategoryFrame.DemosDescriptionDblClick(Sender: TObject);
begin
  inherited;
  if (Trim(FDemosDescription.Items[FDemosDescription.ItemIndex]) <> '') and (pos('    ', FDemosDescription.Items[FDemosDescription.ItemIndex]) <> 1) and Assigned(FOnNavigate) then begin
    FOnNavigate(FDemosDescription.Items[FDemosDescription.ItemIndex]);
  end;
end;

{$ELSE}

procedure TCategoryFrame.SetDemoDescriptions(Descriptions: TStrings);
var
  v: Variant;
  HTMLDocument: IHTMLDocument2;
  Len: integer;
begin
  HTMLDocument := FDemosDescription.Document as IHTMLDocument2;
  if Assigned(HTMLDocument) then begin
    v := VarArrayCreate([0, 0], varVariant);
    v[0] := Descriptions.Text;
    HTMLDocument.Write(PSafeArray(TVarData(v).VArray));
    HTMLDocument.Close;
  end
  else begin
    Len := GetEnvironmentVariable(PChar('TEMP'), nil, 0);
    if Len > 0 then
    begin
      SetLength(FTempFileName, Len - 1);
      GetEnvironmentVariable(PChar('TEMP'), PChar(FTempFileName), Len);
    end;
    if Length(FTempFileName) > 1 then begin
      if FTempFileName[length(FTempFileName) - 1] <> '\' then
        FTempFileName := FTempFileName + '\';
      FTempFileName := FTempFileName + 'DAC_Demo_tmp.html';
      Descriptions.SaveToFile(FTempFileName);
      FDemosDescription.Navigate(FTempFileName)
    end;  
  end;
end;

// Navigation
procedure TCategoryFrame.DemosDescriptionBeforeNavigate2(Sender: TObject;
  const pDisp: IDispatch; {$IFDEF VER230}const{$ELSE}var{$ENDIF} URL, Flags,
  TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
var
  Index: integer;
  str: string;
begin
  str := ExtractFileName(URL);
  if (str = 'blank') or (str = ExtractFileName(FTempFileName)) then // navigating 'about:blank', or the html temporary file
    Exit;               // let navigator to handle
  // our handler
  Cancel := True;
{$IFNDEF LINUX}
  if not TryStrToInt(str, Index) then begin
    str := ExtractFilePath(Application.Exename) + copy(url, 4, Length(url) - 3);
    ShellExecute(0, 'open', PChar(str), '', '.', SW_SHOW);
    exit;
  end;
{$ENDIF}
  if Assigned(FOnNavigate) then
    FOnNavigate(Index);
end;

{$ENDIF}

{$IFDEF WEB}
initialization
  OleInitialize(nil);

finalization
  OleUninitialize;
{$ELSE}
{$IFDEF FPC}
initialization
  {$i CategoryFrame.lrs}
{$ENDIF}
{$ENDIF}

end.
