
unit WebForm;

interface

uses
  System.Collections, System.ComponentModel,
  System.Data, System.Drawing, System.Web, System.Web.SessionState,
  System.Web.UI, System.Web.UI.WebControls, System.Web.UI.HtmlControls,
  System.Globalization, DB, Classes,
  Uni, UniProvider, Devart.UniDac.DataAdapter,
  Devart.UniDac.Oracle.OracleUniProvider,
  Devart.UniDac.SQLServer.SQLServerUniProvider,
  Devart.UniDac.InterBase.InterBaseUniProvider,
  Devart.UniDac.MySQL.MySQLUniProvider;

type
  TWebForm = class(System.Web.UI.Page)
  {$REGION 'Designer Managed Code'}
  strict private
    procedure InitializeComponent;
                procedure btTest_Click(sender: System.Object; e: System.EventArgs);
                procedure btFill_Click(sender: System.Object; e: System.EventArgs);
                procedure btUpdate_Click(sender: System.Object; e: System.EventArgs);
                procedure btInsertRecord_Click(sender: System.Object; e: System.EventArgs);
                procedure dataGrid_EditCommand(source: System.Object; e: System.Web.UI.WebControls.DataGridCommandEventArgs);
                procedure dataGrid_DeleteCommand(source: System.Object; e: System.Web.UI.WebControls.DataGridCommandEventArgs);
                procedure dataGrid_CancelCommand(source: System.Object; e: System.Web.UI.WebControls.DataGridCommandEventArgs);
                procedure dataGrid_UpdateCommand(source: System.Object; e: System.Web.UI.WebControls.DataGridCommandEventArgs);
  {$ENDREGION}
  strict private
    procedure Page_Load(sender: System.Object; e: System.EventArgs);
  strict protected
                lbTitle: System.Web.UI.WebControls.Label;
                lbInfo: System.Web.UI.WebControls.Label;
                btInsertRecord: System.Web.UI.WebControls.Button;
                btUpdate: System.Web.UI.WebControls.Button;
                Label2: System.Web.UI.WebControls.Label;
                btFill: System.Web.UI.WebControls.Button;
                tbSQL: System.Web.UI.WebControls.TextBox;
                lbResult: System.Web.UI.WebControls.Label;
                lbError: System.Web.UI.WebControls.Label;
                tbUsername: System.Web.UI.WebControls.TextBox;
                tbDatabase: System.Web.UI.WebControls.TextBox;
                tbPassword: System.Web.UI.WebControls.TextBox;
                tbServer: System.Web.UI.WebControls.TextBox;
                lbState: System.Web.UI.WebControls.Label;
                btTest: System.Web.UI.WebControls.Button;
                dlProviderName: System.Web.UI.WebControls.DropDownList;
                dataGrid: System.Web.UI.WebControls.DataGrid;
                UniDataAdapter: Devart.UniDac.DataAdapter.UniDataAdapter;
                DataSet: System.Data.DataSet;
                Form1: System.Web.UI.HtmlControls.HtmlForm;
  protected
    UniConnection: TUniConnection;
    UniQuery: TUniQuery;
    procedure OnInit(e: EventArgs); override;
    procedure BindGrid();
  public
    { Public Declarations }
  end;

implementation

{$REGION 'Designer Managed Code'}
/// <summary>
/// Required method for Designer support -- do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TWebForm.InitializeComponent;
begin
  Self.UniDataAdapter := Devart.UniDac.DataAdapter.UniDataAdapter.Create;
  Include(Self.btInsertRecord.Click, Self.btInsertRecord_Click);
  Include(Self.btUpdate.Click, Self.btUpdate_Click);
  Include(Self.btFill.Click, Self.btFill_Click);
  Include(Self.btTest.Click, Self.btTest_Click);
  Include(Self.dataGrid.CancelCommand, Self.dataGrid_CancelCommand);
  Include(Self.dataGrid.EditCommand, Self.dataGrid_EditCommand);
  Include(Self.dataGrid.UpdateCommand, Self.dataGrid_UpdateCommand);
  Include(Self.dataGrid.DeleteCommand, Self.dataGrid_DeleteCommand);
  // 
  // UniDataAdapter
  // 
  Self.UniDataAdapter.DataSet := nil;
  Self.UniDataAdapter.Name := '';
  Self.UniDataAdapter.Tag := nil;
  Include(Self.Load, Self.Page_Load);
end;
{$ENDREGION}

procedure TWebForm.Page_Load(sender: System.Object; e: System.EventArgs);
begin
  dataSet := System.Data.DataSet(HttpContext.Current.Session['dataset']);
end;

procedure TWebForm.OnInit(e: EventArgs);
var
  sl: TStringList;
  i: integer;
begin
  //
  // Required for Designer support
  //
  InitializeComponent;

  UniConnection := TUniConnection.Create(nil);
  with UniConnection do begin
    LoginPrompt := False;
    Username := '';
    Password := '';
    Database := 'Test';
    Port := 3306;
  end;
  UniQuery := TUniQuery.Create(nil);
  with UniQuery do begin
    Connection := UniConnection;
    SQL.Text := 'select * from master';
  end;
  UniDataAdapter.DataSet := UniQuery;

  dlProviderName.Items.Clear;
  sl := TStringList.Create;
  try
    UniProviders.GetProviderNames(sl);
    for i := 0 to sl.Count - 1 do
      dlProviderName.Items.Add(sl[i]);
  finally
    sl.Free;
  end;

  tbUsername.Text := UniConnection.Username;
  tbDatabase.Text := UniConnection.Database;
  tbPassword.Text := UniConnection.Password;
  tbServer.Text := UniConnection.Server;
  tbSQL.Text := UniQuery.SQL.Text;

  inherited OnInit(e);
end;

procedure TWebForm.BindGrid();
begin
  if dataSet.Tables['Table'] <> Nil then begin
    dataGrid.DataSource := dataSet.Tables['Table'].DefaultView;
    lbResult.Visible := true;
    btInsertRecord.Visible := true;
    if dataSet.Tables['Table'].GetChanges() <> Nil then
      lbInfo.Text := 'Changed'
  end
  else begin
    dataGrid.DataSource := Nil;
    lbResult.Visible := false;
    btInsertRecord.Visible := false;
  end;

  dataGrid.DataBind();
end;

procedure TWebForm.dataGrid_UpdateCommand(source: System.Object; e: System.Web.UI.WebControls.DataGridCommandEventArgs);
var
 i : integer;
 colValue: string;
begin
  for i := 2 to e.Item.Cells.Count - 1 do
    if e.Item.Cells[i].Controls.Count > 0 then begin
      colValue := System.Web.UI.WebControls.TextBox(e.Item.Cells[i].Controls[0]).Text;
      if colValue = '' then
        dataSet.Tables['Table'].Rows[e.Item.ItemIndex][i - 2] := Convert.DBNull
      else
        dataSet.Tables['Table'].Rows[e.Item.ItemIndex][i - 2] := colValue;
    end;

  dataGrid.EditItemIndex := -1;
  BindGrid();
end;

procedure TWebForm.dataGrid_CancelCommand(source: System.Object; e: System.Web.UI.WebControls.DataGridCommandEventArgs);
begin
  dataGrid.EditItemIndex := -1;
  BindGrid();
end;

procedure TWebForm.dataGrid_DeleteCommand(source: System.Object; e: System.Web.UI.WebControls.DataGridCommandEventArgs);
begin
  dataSet.Tables['Table'].Rows[e.Item.ItemIndex].Delete();
  BindGrid();
end;

procedure TWebForm.dataGrid_EditCommand(source: System.Object; e: System.Web.UI.WebControls.DataGridCommandEventArgs);
begin
  dataGrid.EditItemIndex := System.Int32(e.Item.ItemIndex);
  BindGrid();
end;

procedure TWebForm.btInsertRecord_Click(sender: System.Object; e: System.EventArgs);
var
  row : DataRow;
begin
  row := dataSet.Tables['Table'].NewRow();
  dataSet.Tables['Table'].Rows.Add(row);
  dataGrid.EditItemIndex := dataSet.Tables['Table'].Rows.Count - 1;
  BindGrid();
end;

procedure TWebForm.btUpdate_Click(sender: System.Object; e: System.EventArgs);
begin
  if dataSet.Tables['Table'] <> Nil then begin
    try try
      UniConnection.ProviderName := dlProviderName.SelectedValue;
      UniConnection.Username := tbUsername.Text;
      UniConnection.Database := tbDatabase.Text;
      UniConnection.Password := tbPassword.Text;
      UniConnection.Server := tbServer.Text;

      UniQuery.SQL.Text := tbSQL.Text;

      UniDataAdapter.Update(dataSet, 'Table');

      lbInfo.Text := 'Updated';
    except
      on exception : Exception do begin
        lbError.Text := exception.Message;
      end;
    end;
    finally
       BindGrid();
    end;
  end;
end;

procedure TWebForm.btFill_Click(sender: System.Object; e: System.EventArgs);
var
  i : integer;
  table : DataTable;
begin
  try try
    dataSet.Clear();
    for i := 0 to dataSet.Tables.Count - 1 do begin
      table := dataSet.Tables[i];
      table.Constraints.Clear();
      table.Columns.Clear();
    end;
    dataSet.Tables.Clear();

    UniConnection.ProviderName := dlProviderName.SelectedValue;
    UniConnection.Username := tbUsername.Text;
    UniConnection.Database := tbDatabase.Text;
    UniConnection.Password := tbPassword.Text;
    UniConnection.Server := tbServer.Text;

    UniQuery.SQL.Text := tbSQL.Text;

    UniDataAdapter.Fill(dataSet, 'Table');

    lbInfo.Text := 'Filled';
  except
    on exception : Exception do
     lbError.Text := exception.Message;
  end;
  finally
    BindGrid();
  end;
end;

procedure TWebForm.btTest_Click(sender: System.Object; e: System.EventArgs);
begin
  try
    UniConnection.ProviderName := dlProviderName.SelectedValue;
    UniConnection.Username := tbUsername.Text;
    UniConnection.Database := tbDatabase.Text;
    UniConnection.Password := tbPassword.Text;
    UniConnection.Server := tbServer.Text;

    UniConnection.Open;
    lbState.Text := 'Success';
    lbState.ForeColor := Color.Blue;
    UniConnection.Close;
  except on exception: Exception do
    begin
      lbState.Text := 'Failed';
      lbState.ForeColor := Color.Red;
      lbError.Text := exception.Message;
    end;
  end;
end;

end.

