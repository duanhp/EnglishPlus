unit CommonFunc;

interface

uses
  SHDocVw, MSHTML, ActiveX, Variants, Forms, SysUtils, ComCtrls, ComObj;

///	<summary>
///	  �������ֱ�Ӵ�Html�ַ���
///	</summary>
///	<param name="wb">
///	  ������ؼ�
///	</param>
///	<param name="HtmlStr">
///	  html����
///	</param>
procedure WebBrowserOpenHtmlStr(wb: TWebBrowser; HtmlStr: string);

///	<summary>
///	  �ѽ�����Ƕ��listview�ؼ���
///	</summary>
///	<param name="lv">
///	  listview�ؼ�
///	</param>
///	<param name="Row">
///	  ��
///	</param>
///	<param name="Col">
///	  ��
///	</param>
///	<returns>
///	  ����TrueǶ��ɹ�����ʧ��
///	</returns>
function SetProgressBarInListView(lv: TListView; Row, Col: Integer): Boolean;

///	<summary>
///	  �ѽ�����Ƕ��״̬��
///	</summary>
///	<param name="sb">
///	  ״̬���ؼ�
///	</param>
///	<param name="nIndex">
///	  �ڼ�����
///	</param>
///	<param name="pbName">
///	  ���������������
///	</param>
///	<returns>
///	  ����TrueǶ��ɹ�����ʧ��
///	</returns>
function SetProgreesBarInStatusBar(sb: TStatusBar; nIndex: Integer; pbName: string='pb'): Boolean;

function IsSetupExcel: Boolean;

implementation

procedure WebBrowserOpenHtmlStr(wb: TWebBrowser; HtmlStr: string);
var
  Doc: IHtmlDocument2;
  V: OleVariant;
begin
  wb.Navigate('about:blank');
  while not (wb.ReadyState = READYSTATE_COMPLETE) do
  begin
    Application.ProcessMessages;
    Sleep(1);
  end;
  Doc := wb.Document as IHtmlDocument2;
  if not Assigned(Doc) then
    Exit;
  V := VarArrayCreate([0, 0], varVariant);
  V[0] := HtmlStr;
  Doc.Write(PSafeArray(TVarData(V).VArray));
  Doc.Close;
end;

function SetProgressBarInListView(lv: TListView; Row, Col: Integer): Boolean;
var
  li: TListItem;
  pb: TProgressBar;
  I, L: Integer;
begin
  Result := False;
  //lvΪnil�Ļ� �˳�
  if not Assigned(lv) then
    Exit;
  //���Ǳ�����ʽ�� �˳�
  if not (lv.ViewStyle in [vsReport]) then
    Exit;
  //�Ҳ������˳�
  if lv.Columns.Count < Col + 1 then
    Exit;
  //�Ҳ������˳�
  if lv.Items.Count < Row + 1 then
    Exit;

  li := lv.Items[Row];
  if li.Data = nil then
  begin
    li.Data := TProgressBar.Create(nil);
    TProgressBar(li.Data).Parent := lv;
    TProgressBar(li.Data).Tag := li.Index;
  end;

  //��������λ
  pb := TProgressBar(li.Data);
  L := li.DisplayRect(drBounds).Left;
  for I := 0 to Col - 1 do
    L := L + lv.Columns[I].Width;
  pb.Left := L;
  pb.Top := li.DisplayRect(drBounds).Top + 1;
  pb.Width := lv.Columns[Col].Width;
  pb.Height := li.DisplayRect(drBounds).Bottom - li.DisplayRect(drBounds).Top - 2;

  Result := True;
end;

function SetProgreesBarInStatusBar(sb: TStatusBar; nIndex: Integer; pbName: string): Boolean;
var
  pb: TProgressBar;
  I,L: Integer;
begin
  Result := False;
  pb := TProgressBar.Create(nil);
  pb.Parent := sb;
  pb.Name := pbName;
  L := 0;
  for I := 0 to nIndex - 1 do
    L := L + sb.Panels[I].Width;
  pb.Left := L+2;
  pb.Top := 2;
  pb.Width := sb.Panels[nIndex].Width-4;
  pb.Height := sb.Height-4;
  Result := True;
end;

function IsSetupExcel: Boolean;
var
  ExcelApp: Variant;
begin
  Result := True;
  try
    ExcelApp := CreateOleObject('Excel.Application');
    ExcelApp.WorkBooks.Add;
    ExcelApp.Quit;
  except
    Result := False;
  end;
end;

end.
