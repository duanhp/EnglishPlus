{$I DacDemo.inc}

unit DemoBase;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, ShellAPI,
{$ENDIF}
  Classes, SysUtils, DemoFrame, CategoryFrame, DBAccess, MemUtils,
{$IFDEF KYLIX}
  QControls, QGraphics, QComCtrls, QForms
{$ELSE}
  ComCtrls, Controls, Graphics, Forms, HTMLConsts
{$ENDIF}
  ;

type
  TDemoType = (dtDemo, dtCategory);

  TDemo = class
  protected
    FName: string;
    FFileName: string;
    FHint: string;
    FDescription: string;
    FDemoType: TDemoType;
    FFrameClass: TDemoFrameClass;
    FFrame: TDemoFrame;
  public
    constructor Create(Name, Hint, Description: string; DemoType: TDemoType; FrameClass: TDemoFrameClass; FileName: string = '');
    destructor Destroy; override;

    procedure LoadDemoCode(Strings: TStrings);
    procedure LoadFormCode(Strings: TStrings);
    procedure OpenDemoFolder;
    procedure FreeFrame;

    property Name: string read FName;
    property Hint: string read FHint;
    property Description: string read FDescription;
    property DemoType: TDemoType read FDemoType;
    property FrameClass: TDemoFrameClass read FFrameClass;
    property Frame: TDemoFrame read FFrame;
  end;

  TDemos = class
  protected
    FDemoTreeNodes: TTreeNodes;
    FSelectedDemo: TDemo;
    FSupplementaryDemosDescription: TStrings;
    function GetSelectedDemo: TDemo;
    function GetItems(Index: integer): TDemo;

    function WrapDescription(Demo: TDemo): TStrings;
  public
    constructor Create(TreeNodes: TTreeNodes); virtual;
    destructor Destroy; override;

    procedure RegisterCategory(CategoryName, Hint: string; ImgIndex: integer = -1; SupplementaryDemo: boolean = False);
    procedure RegisterDemo(DemoName, DemoHint, DemoDescription, DemoCategory: string; FrameClass: TDemoFrameClass; ImgIndex: integer; FileName: string = ''; SupplementaryDemo: boolean = False);
    procedure Clear;
    //Navigation
    function SelectDemo(DemoIndex: integer): TDemo;    //Create demo frame by DemoIndex

    function GetDemoIndex(AbsoluteIndex: integer): integer;

    property Items[Index: integer]: TDemo read GetItems; default;
    property SelectedDemo: TDemo read GetSelectedDemo;
  end;

implementation

const
  MainDemosHeader = 'Main Demo Projects';
  SupplementaryDemosHeader = 'Supplementary Demo Projects';
  SupplementaryDemosNote = '       Note, the demo projects listed below are separate projects. ' {$IFNDEF WEB} + #13#10 {$ENDIF} + '       This project contains only their descriptions and links to their folders.';

{$IFNDEF WEB}
procedure WriteTable(CategoryName, CategoryDescription: string; List: TStrings);
var
  strs: TStringList;
begin
  List.Add(' ');
  List.Add('    ' + CategoryName);
  strs := TStringList.Create;
  strs.Text := CategoryDescription;
  List.AddStrings(strs);
  strs.Free;
end;
{$ELSE}
procedure WriteTable(CategoryName, CategoryDescription: string; List: TStrings);
begin
  if CategoryName = '' then begin  // used to close virtual category
    List.Add('</table>');
    Exit;
  end;

  List.Add('<p><b><div style="padding-top: 10px;">' + CategoryName + '</div></b></p>');
  List.Add('<table class="xmldoctable" cellspacing="0">');
  List.Add('<tr>');
  List.Add('<th width="150">Demo</th>');
  List.Add('<th>Description</th>');
  List.Add('</tr>');
  if CategoryDescription <> '' then begin
    List.Add(CategoryDescription);
    List.Add('</table>');
  end;
end;
{$ENDIF}

constructor TDemos.Create(TreeNodes: TTreeNodes);
begin
  inherited Create;

  if not Assigned(TreeNodes) then
    raise Exception.Create('TreeNodes should be set');
  FDemoTreeNodes := TreeNodes;
  FSupplementaryDemosDescription := TStringList.Create;
{$IFNDEF WEB}
  FSupplementaryDemosDescription.Text :=  #13#10'     ' + SupplementaryDemosHeader;
  FSupplementaryDemosDescription.Add(SupplementaryDemosNote);
{$ENDIF}
end;

destructor TDemos.Destroy;
begin
  Clear;
  FSupplementaryDemosDescription.Free;

  inherited;
end;

procedure TDemos.RegisterCategory(CategoryName, Hint: string; ImgIndex: integer = -1; SupplementaryDemo: boolean = False);
var
  Node: TTreeNode;
  Index: integer;
  Category: TDemo;
begin
  if SupplementaryDemo then
    WriteTable(CategoryName, Hint, FSupplementaryDemosDescription)
  else begin
    Category := TDemo.Create(CategoryName, Hint, '', dtCategory, TCategoryFrame);
    Node := FDemoTreeNodes.AddChildObject(FDemoTreeNodes.GetFirstNode, CategoryName, Category);
    if ImgIndex < 0 then
      Index := 0
    else
      Index := ImgIndex;
    Node.ImageIndex := Index;
    Node.SelectedIndex := Index;
  {$IFNDEF LINUX}
    Node.StateIndex := Index;
  {$ENDIF}
  end;
end;

procedure TDemos.RegisterDemo(DemoName, DemoHint, DemoDescription, DemoCategory: string; FrameClass: TDemoFrameClass; ImgIndex: integer; FileName: string = ''; SupplementaryDemo: boolean = False);

  function FindCategoryNode(CategoryName: string): TTreeNode;
  var
    RootNode: TTreeNode;
  begin
    RootNode := FDemoTreeNodes.GetFirstNode;
    if RootNode <> nil then
      Result := RootNode.getFirstChild
    else
      Result := nil;
    while Result <> nil do begin
      if Result.Text = CategoryName then
        break;
      Result := Result.getNextSibling;
    end;
  end;

{$IFNDEF WEB}
  function CompleteWithSpaces(s: string; ResultLength: word): string;
  var
    n, i: integer;
  begin
    result := s;
    n := ResultLength - Length(s);
    if n > 0 then
      for i := 1 to n do
        result := result + ' ';
  end;
{$ENDIF}

  function ToTableLine(Name, Description, DemoLink: string): string;
  begin
  {$IFDEF WEB}
    Result := Format('<tr height="23"> <td>' +
      '<b><a href="x:\%s">%s</a></b></td><td class="xmldoctable">%s</td></tr>'#13#10,
      [DemoLink, Name, Description]);
  {$ELSE}
    Result := CompleteWithSpaces(Name, 15) + '- ' + Description + {$IFDEF LINUX}#13{$ELSE}#13#10{$ENDIF};
  {$ENDIF}
  end;

var
  CategoryNode, DemoNode: TTreeNode;
  Index: integer;
  Category, Demo: TDemo;
begin
  if SupplementaryDemo then
    FSupplementaryDemosDescription.Text := FSupplementaryDemosDescription.Text + ToTableLine(DemoName, DemoDescription, '..\' + DemoCategory + '\' + DemoName)
  else begin
    CategoryNode := FindCategoryNode(DemoCategory);
    if not Assigned(CategoryNode) then
      raise Exception.Create('DemoCategory is wrong');

    Category := TDemo(CategoryNode.Data);
    Demo := TDemo.Create(DemoName, DemoHint, DemoDescription, dtDemo, FrameClass, FileName);
    DemoNode := FDemoTreeNodes.AddChildObject(CategoryNode, Demo.Name, Demo);
    if ImgIndex < 0 then
      Index := 1
    else
      Index := ImgIndex;
    DemoNode.ImageIndex := Index;
    DemoNode.SelectedIndex := Index;
  {$IFNDEF LINUX}
    DemoNode.StateIndex := Index;
  {$ENDIF}

    Category.FDescription := Category.FDescription + ToTableLine(DemoName, DemoDescription, IntToStr(DemoNode.AbsoluteIndex));
  end;
end;

procedure TDemos.Clear;
var
  i: integer;
begin
  for i := 0 to FDemoTreeNodes.Count - 1 do
    if FDemoTreeNodes[i].Data <> nil then
      TDemo(FDemoTreeNodes[i].Data).Free;
end;

function TDemos.GetSelectedDemo: TDemo;
begin
  if FSelectedDemo <> nil then
    Result := FSelectedDemo
  else
    raise Exception.Create('No selected demo');
end;

function TDemos.GetDemoIndex(AbsoluteIndex: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FDemoTreeNodes.Count - 1 do
    if FDemoTreeNodes[i].AbsoluteIndex = AbsoluteIndex then begin
      if FDemoTreeNodes[i].Data <> nil then
        Result := i;
      Break;
    end;
end;

function TDemos.GetItems(Index: integer): TDemo;
var
  i: integer;
begin
  i := GetDemoIndex(Index);
  if i >= 0 then
    Result := TDemo(FDemoTreeNodes[i].Data)
  else
    raise Exception.Create('Wrong demo index');
end;

function TDemos.WrapDescription(Demo: TDemo): TStrings;

var
  CatNode, RootNode: TTreeNode;
begin
  Result := TStringList.Create;
{$IFDEF WEB}
  Result.Add(HTMLHeader);
  Result.Add('<h3 class="dxH3" style = "text-align:center">' + MainDemosHeader + '</h3>');
{$ELSE}
  Result.Add('');
  Result.Add('     ' + MainDemosHeader);
  Result.Add('');
{$ENDIF}
  if Demo <> GetItems(0) then         //RootCategory
    WriteTable(Demo.Name, Demo.Description, Result)
  else begin
    RootNode := FDemoTreeNodes.GetFirstNode;
    CatNode := RootNode.getFirstChild;
    while CatNode <> nil do begin
      WriteTable(TDemo(CatNode.Data).Name, TDemo(CatNode.Data).Description, Result);
      CatNode := CatNode.getNextSibling;
    end;
{$IFDEF WEB}
    Result.Add('<h3 class="dxH3" style = "text-align:center"><br>' + SupplementaryDemosHeader + '</h3>');
    Result.Add('<p Align="center"><b>' + SupplementaryDemosNote + '</p></b>');
{$ENDIF}

    Result.AddStrings(FSupplementaryDemosDescription);
  end;
{$IFDEF WEB}
  Result.Add(HTMLFooter);
{$ENDIF}
end;

function TDemos.SelectDemo(DemoIndex: integer): TDemo;  //Init and show demo by DemoIndex
var
  Descriptions: TStrings;
begin
  Result := GetItems(DemoIndex);
  if (FSelectedDemo <> nil) and (Result <> FSelectedDemo) then
    if FSelectedDemo.DemoType <> dtCategory then
      FSelectedDemo.FreeFrame //In case of demo selection change we should free demo frame except category description
    else
      FSelectedDemo.Frame.Hide;
  FSelectedDemo := Result;
  with FSelectedDemo do
    if FFrame = nil then begin
      FFrame := FFrameClass.Create(nil);
      if DemoType = dtCategory then begin
        Descriptions := WrapDescription(FSelectedDemo);
        try
          TCategoryFrame(FFrame).SetDemoDescriptions(Descriptions);
        finally
          Descriptions.Free;
        end;
      end;
    end
    else
      FFrame.Show;
end;

{TDemo}
constructor TDemo.Create(Name, Hint, Description: string; DemoType: TDemoType; FrameClass: TDemoFrameClass; FileName: string = '');
begin
  inherited Create;

  FName := Name;
  if FileName = '' then
    FFileName := Name
  else
    FFileName := FileName;
  FHint := Hint;
  FDescription := Description;
  FFrameClass := FrameClass;
  FDemoType := DemoType;
end;

destructor TDemo.Destroy;
begin
  FreeFrame;

  inherited;
end;

procedure TDemo.LoadDemoCode(Strings: TStrings);
var
  FileName: string;
begin
  if DemoType = dtCategory then
    Strings.Clear
  else begin
  {$IFDEF LINUX}
     FileName := Format('%s/%s/%s.pas', [ExtractFilePath(Application.ExeName), Name, FFileName]);
  {$ELSE}
     FileName := Format('%s\%s\%s.pas', [ExtractFilePath(Application.ExeName), Name, FFileName]);
  {$ENDIF}

    if FileExists(FileName) then
      Strings.LoadFromFile(FileName)
    else
      Strings.Clear;
  end;
end;

procedure TDemo.LoadFormCode(Strings: TStrings);
var
  FileName: string;
begin
  if DemoType = dtCategory then
    Strings.Clear
  else begin
  {$IFDEF LINUX}
     FileName := Format('%s/%s/%s.xfm', [ExtractFilePath(Application.ExeName), Name, FFileName]);
  {$ENDIF}
  {$IFDEF CLR}
     FileName := Format('%s\%s\%s.nfm', [ExtractFilePath(Application.ExeName), Name, FFileName]);
  {$ENDIF}
  {$IFDEF WEB}
     FileName := Format('%s\%s\%s.dfm', [ExtractFilePath(Application.ExeName), Name, FFileName]);
  {$ENDIF}
  {$IFDEF FPC}
     FileName := Format('%s\%s\%s.lfm', [ExtractFilePath(Application.ExeName), Name, FFileName]);
  {$ENDIF}

    if FileExists(FileName) then
      Strings.LoadFromFile(FileName)
    else
      Strings.Clear;
  end;
end;

procedure TDemo.OpenDemoFolder;
{$IFNDEF LINUX}
var
  FolderName: string;
begin
  if DemoType = dtDemo then begin
    FolderName := ExtractFilePath(Application.ExeName) + Name;
    ShellExecute(0, 'open', PChar(FolderName), '', '.', SW_SHOW);
  end;
end;
{$ELSE}
begin

end;
{$ENDIF}

procedure TDemo.FreeFrame;
begin
  FFrame.Free;
  FFrame := nil;
end;


end.
