unit MainForm;

interface

uses
  System.Drawing, System.Collections, System.ComponentModel,
  System.Windows.Forms, System.Data, System.Globalization,
  DB, Uni, DBAccess, UniDacVcl, Devart.UniDac.DataAdapter,
  Devart.UniDac.Oracle.OracleUniProvider,
  Devart.UniDac.SQLServer.SQLServerUniProvider,
  Devart.UniDac.InterBase.InterBaseUniProvider,
  Devart.UniDac.MySQL.MySQLUniProvider;

type
  TMainForm = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    Components: System.ComponentModel.Container;
    topPanel: System.Windows.Forms.Panel;
    btConnect: System.Windows.Forms.Button;
    btDisconnect: System.Windows.Forms.Button;
    btFill: System.Windows.Forms.Button;
    btClear: System.Windows.Forms.Button;
    tbSql: System.Windows.Forms.TextBox;
    dataGrid: System.Windows.Forms.DataGrid;
    splitter: System.Windows.Forms.Splitter;
    statusBar: System.Windows.Forms.StatusBar;
    StatusBarPanel1: System.Windows.Forms.StatusBarPanel;
    dataSet: System.Data.DataSet;
    UniDataAdapter1: Devart.UniDac.DataAdapter.UniDataAdapter;
    btUpdate: System.Windows.Forms.Button;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure btConnect_Click(sender: System.Object; e: System.EventArgs);
    procedure btDisconnect_Click(sender: System.Object; e: System.EventArgs);
    procedure btFill_Click(sender: System.Object; e: System.EventArgs);
    procedure btClear_Click(sender: System.Object; e: System.EventArgs);
    procedure TMainForm_Load(sender: System.Object; e: System.EventArgs);
    procedure tbSql_Leave(sender: System.Object; e: System.EventArgs);
    procedure btUpdate_Click(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    procedure Dispose(Disposing: Boolean); override;
  protected
    ConnectDialog: TUniConnectDialog;
    UniConnection: TUniConnection;
    UniQuery: TUniQuery;
  public
    constructor Create;
  end;

implementation

uses
  UniConnectForm;

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support - do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TMainForm.InitializeComponent;
type
        TArrayOfSystem_Windows_Forms_StatusBarPanel = array of System.Windows.Forms.StatusBarPanel;
begin
        Self.topPanel := System.Windows.Forms.Panel.Create;
        Self.btClear := System.Windows.Forms.Button.Create;
        Self.btFill := System.Windows.Forms.Button.Create;
        Self.btDisconnect := System.Windows.Forms.Button.Create;
        Self.btConnect := System.Windows.Forms.Button.Create;
        Self.tbSql := System.Windows.Forms.TextBox.Create;
        Self.dataGrid := System.Windows.Forms.DataGrid.Create;
        Self.dataSet := System.Data.DataSet.Create;
        Self.splitter := System.Windows.Forms.Splitter.Create;
        Self.statusBar := System.Windows.Forms.StatusBar.Create;
        Self.StatusBarPanel1 := System.Windows.Forms.StatusBarPanel.Create;
        Self.UniDataAdapter1 := Devart.UniDac.DataAdapter.UniDataAdapter.Create;
        Self.btUpdate := System.Windows.Forms.Button.Create;
        Self.topPanel.SuspendLayout;
        (System.ComponentModel.ISupportInitialize(Self.dataGrid)).BeginInit;
        (System.ComponentModel.ISupportInitialize(Self.dataSet)).BeginInit;
        (System.ComponentModel.ISupportInitialize(Self.StatusBarPanel1)).BeginInit;
        Self.SuspendLayout;
        // 
        // topPanel
        // 
        Self.topPanel.Controls.Add(Self.btUpdate);
        Self.topPanel.Controls.Add(Self.btClear);
        Self.topPanel.Controls.Add(Self.btFill);
        Self.topPanel.Controls.Add(Self.btDisconnect);
        Self.topPanel.Controls.Add(Self.btConnect);
        Self.topPanel.Dock := System.Windows.Forms.DockStyle.Top;
        Self.topPanel.Location := System.Drawing.Point.Create(0, 0);
        Self.topPanel.Name := 'topPanel';
        Self.topPanel.Size := System.Drawing.Size.Create(568, 24);
        Self.topPanel.TabIndex := 0;
        // 
        // btClear
        // 
        Self.btClear.Location := System.Drawing.Point.Create(368, 0);
        Self.btClear.Name := 'btClear';
        Self.btClear.TabIndex := 3;
        Self.btClear.Text := 'Clear';
        Include(Self.btClear.Click, Self.btClear_Click);
        // 
        // btFill
        // 
        Self.btFill.Location := System.Drawing.Point.Create(200, 0);
        Self.btFill.Name := 'btFill';
        Self.btFill.TabIndex := 2;
        Self.btFill.Text := 'Fill';
        Include(Self.btFill.Click, Self.btFill_Click);
        // 
        // btDisconnect
        // 
        Self.btDisconnect.Location := System.Drawing.Point.Create(76, 0);
        Self.btDisconnect.Name := 'btDisconnect';
        Self.btDisconnect.TabIndex := 1;
        Self.btDisconnect.Text := 'Disconnect';
        Include(Self.btDisconnect.Click, Self.btDisconnect_Click);
        // 
        // btConnect
        // 
        Self.btConnect.Location := System.Drawing.Point.Create(0, 0);
        Self.btConnect.Name := 'btConnect';
        Self.btConnect.TabIndex := 0;
        Self.btConnect.Text := 'Connect';
        Include(Self.btConnect.Click, Self.btConnect_Click);
        // 
        // tbSql
        // 
        Self.tbSql.Dock := System.Windows.Forms.DockStyle.Top;
        Self.tbSql.Font := System.Drawing.Font.Create('Courier New', 9.75);
        Self.tbSql.Location := System.Drawing.Point.Create(0, 24);
        Self.tbSql.Multiline := True;
        Self.tbSql.Name := 'tbSql';
        Self.tbSql.Size := System.Drawing.Size.Create(568, 64);
        Self.tbSql.TabIndex := 1;
        Self.tbSql.Text := '';
        Include(Self.tbSql.Leave, Self.tbSql_Leave);
        // 
        // dataGrid
        // 
        Self.dataGrid.AllowNavigation := False;
        Self.dataGrid.DataMember := '';
        Self.dataGrid.DataSource := Self.dataSet;
        Self.dataGrid.Dock := System.Windows.Forms.DockStyle.Fill;
        Self.dataGrid.HeaderForeColor := System.Drawing.SystemColors.ControlText;
        Self.dataGrid.Location := System.Drawing.Point.Create(0, 90);
        Self.dataGrid.Name := 'dataGrid';
        Self.dataGrid.Size := System.Drawing.Size.Create(568, 253);
        Self.dataGrid.TabIndex := 2;
        Self.dataGrid.AllowSorting := False;
        // 
        // dataSet
        // 
        Self.dataSet.DataSetName := 'NewDataSet';
        Self.dataSet.Locale := System.Globalization.CultureInfo.Create('');
        // 
        // splitter
        // 
        Self.splitter.Cursor := System.Windows.Forms.Cursors.HSplit;
        Self.splitter.Dock := System.Windows.Forms.DockStyle.Top;
        Self.splitter.Location := System.Drawing.Point.Create(0, 88);
        Self.splitter.Name := 'splitter';
        Self.splitter.Size := System.Drawing.Size.Create(568, 2);
        Self.splitter.TabIndex := 3;
        Self.splitter.TabStop := False;
        // 
        // statusBar
        // 
        Self.statusBar.Location := System.Drawing.Point.Create(0, 343);
        Self.statusBar.Name := 'statusBar';
        Self.statusBar.Panels.AddRange(TArrayOfSystem_Windows_Forms_StatusBarPanel.Create(Self.StatusBarPanel1));
        Self.statusBar.ShowPanels := True;
        Self.statusBar.Size := System.Drawing.Size.Create(568, 22);
        Self.statusBar.TabIndex := 4;
        // 
        // StatusBarPanel1
        // 
        Self.StatusBarPanel1.AutoSize := System.Windows.Forms.StatusBarPanelAutoSize.Spring;
        Self.StatusBarPanel1.Width := 552;
        // 
        // UniDataAdapter1
        // 
        Self.UniDataAdapter1.DataSet := nil;
        Self.UniDataAdapter1.Name := '';
        Self.UniDataAdapter1.Tag := nil;
        // 
        // btUpdate
        // 
        Self.btUpdate.Location := System.Drawing.Point.Create(280, 0);
        Self.btUpdate.Name := 'btUpdate';
        Self.btUpdate.TabIndex := 4;
        Self.btUpdate.Text := 'Update';
        Include(Self.btUpdate.Click, Self.btUpdate_Click);
        // 
        // TMainForm
        // 
        Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
        Self.ClientSize := System.Drawing.Size.Create(568, 365);
        Self.Controls.Add(Self.dataGrid);
        Self.Controls.Add(Self.splitter);
        Self.Controls.Add(Self.tbSql);
        Self.Controls.Add(Self.topPanel);
        Self.Controls.Add(Self.statusBar);
        Self.Name := 'TMainForm';
        Self.Text := 'UniDac .NET demo - using DataSet component';
        Include(Self.Load, Self.TMainForm_Load);
        Self.topPanel.ResumeLayout(False);
        (System.ComponentModel.ISupportInitialize(Self.dataGrid)).EndInit;
        (System.ComponentModel.ISupportInitialize(Self.dataSet)).EndInit;
        (System.ComponentModel.ISupportInitialize(Self.StatusBarPanel1)).EndInit;
        Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TMainForm.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TMainForm.Create;
begin
  inherited Create;
  //
  // Required for Windows Form Designer support
  //
  InitializeComponent;
  ConnectDialog := TUniConnectDialog.Create(nil);
  with ConnectDialog do begin
    SavePassword := True;
    Caption := 'Connect';
    UsernameLabel := 'Username';
    PasswordLabel := 'Password';
    ServerLabel := 'Server';
    ConnectButton := 'Connect';
    CancelButton := 'Cancel';
  end;
  UniConnection := TUniConnection.Create(nil);
  with UniConnection do begin
    ConnectDialog := ConnectDialog;
    LoginPrompt := True;
    Username := '';
    Database := 'test';
    Password := '';
  end;
  UniQuery := TUniQuery.Create(nil);
  with UniQuery do begin
    Connection := UniConnection;
    SQL.Text := 'select * from master';
  end;
  UniDataAdapter1.DataSet := UniQuery;
end;

procedure TMainForm.btUpdate_Click(sender: System.Object; e: System.EventArgs);
begin
  UniDataAdapter1.Update(dataSet, 'Table1');
end;

procedure TMainForm.TMainForm_Load(sender: System.Object; e: System.EventArgs);
begin
  tbSql.Text := UniQuery.SQL.Text;
end;

procedure TMainForm.tbSql_Leave(sender: System.Object; e: System.EventArgs);
begin
  UniQuery.SQL.Text := tbSql.Text;
end;

procedure TMainForm.btConnect_Click(sender: System.Object; e: System.EventArgs);
begin
  UniConnection.Open;
end;

procedure TMainForm.btDisconnect_Click(sender: System.Object; e: System.EventArgs);
begin
  UniConnection.Close;
end;

procedure TMainForm.btFill_Click(sender: System.Object; e: System.EventArgs);
begin
  UniDataAdapter1.Fill(dataSet, 'Table1');
  dataGrid.DataMember := 'Table1';
end;

procedure TMainForm.btClear_Click(sender: System.Object; e: System.EventArgs);
var
  table: DataTable;
  i: integer;
begin
  dataGrid.DataMember := '';
  dataSet.Clear;

  for i := 0 to dataSet.Tables.Count - 1 do begin
    table := dataSet.Tables[i];
    table.Constraints.Clear;
    table.Columns.Clear;
  end;
end;

end.
