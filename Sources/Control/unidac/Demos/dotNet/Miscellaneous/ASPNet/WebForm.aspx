<%@ Page language="c#" Codebehind="WebForm.pas" AutoEventWireup="false" Inherits="WebForm.TWebForm" %>
<!doctype HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html>
  <head>
    <title>WebForm</title>
<meta content="Microsoft Visual Studio 7.0" name=GENERATOR>
<meta content=C# name=CODE_LANGUAGE>
<meta content=JavaScript name=vs_defaultClientScript>
<meta content=http://schemas.microsoft.com/intellisense/ie5 name=vs_targetSchema>
  </head>
<body ms_positioning="GridLayout">
  <form id=Form1 method=post runat="server">
    <table id=Table1 
           style="Z-INDEX: 104; LEFT: 10px; WIDTH: 700px; POSITION: absolute; TOP: 17px" 
           cellspacing=5 cellpadding=0 bgcolor=#ccccff>
      <tbody>
        <tr>
          <td>
            <asp:label id=lbTitle runat="server" 
                       font-names="Verdana" font-size="12pt" 
					   enableviewstate="False" font-bold="True"
					   forecolor="Navy">Using UniDAC .NET with ASP .NET</asp:label>
		  </td>
        </tr>
      </tbody>
    </table>
    <asp:label id=lbInfo 
               style="Z-INDEX: 111; LEFT: 646px; POSITION: absolute; TOP: 214px" 
               runat="server" font-names="Verdana" font-size="10pt" 
               enableviewstate="False" font-bold="True" forecolor="Navy"></asp:label>
    <asp:button id=btInsertRecord 
                style="Z-INDEX: 110; LEFT: 614px; POSITION: absolute; TOP: 302px" 
                runat="server" text="Insert record" width="100px" 
                visible="False">
    </asp:button>
    <asp:button id=btUpdate 
                style="Z-INDEX: 109; LEFT: 438px; POSITION: absolute; TOP: 214px" 
                runat="server" text="Update" width="100px">
    </asp:button>
	<asp:label id=Label2
               style="Z-INDEX: 108; LEFT: 334px; POSITION: absolute; TOP: 54px" 
               runat="server" font-names="Verdana" font-size="10pt" 
               enableviewstate="False" font-bold="True" forecolor="Navy">SQL</asp:label>
    <asp:button id=btFill 
                style="Z-INDEX: 103; LEFT: 334px; POSITION: absolute; TOP: 214px" 
                runat="server" text="Fill" width="100px">
    </asp:button>
    <asp:textbox id=tbSQL 
                 style="Z-INDEX: 101; LEFT: 334px; POSITION: absolute; TOP: 78px" 
                 runat="server" font-names="Courier New" width="373px" 
                 height="121px" wrap="False" textmode="MultiLine">
    </asp:textbox>
    <asp:label id=lbResult 
               style="Z-INDEX: 107; LEFT: 14px; POSITION: absolute; TOP: 310px" 
               runat="server" font-names="Verdana" font-size="10pt" 
               enableviewstate="False" font-bold="True" forecolor="Navy" 
               visible="False">Result</asp:label>
    <asp:label id=lbError 
               style="Z-INDEX: 105; LEFT: 14px; POSITION: absolute; TOP: 284px" 
               runat="server" font-names="Verdana" font-size="10pt" 
               enableviewstate="False" font-bold="True" forecolor="Red"></asp:label>
    <table style="BORDER-RIGHT: black 1px solid; BORDER-TOP: black 1px solid; Z-INDEX: 102; LEFT: 14px; FONT: 10pt verdana; BORDER-LEFT: black 1px solid; BORDER-BOTTOM: black 1px solid; POSITION: absolute; TOP: 54px; BACKGROUND-COLOR: lemonchiffon" 
           cellspacing=15 cellpadding=0>
      <tbody>
        <tr>
          <td>
            <b>Provider name </b>
          </td>
          <td>
            <asp:DropDownList id="dlProviderName" runat="server" width="155px"></asp:DropDownList>
          </td>
        </tr>
        <tr>
          <td>
            <b>Username </b>
          </td>
          <td>
            <asp:textbox id=tbUsername runat="server">
            </asp:textbox>
          </td>
        </tr>
        <tr>
          <td>
            <b>Password </b>
          </td>
          <td>
            <asp:textbox id=tbPassword runat="server">
            </asp:textbox>
          </td>
        </tr>
        <tr>
          <td>
            <b>Database </b>
          </td>
          <td>
            <asp:textbox id=tbDatabase runat="server">
            </asp:textbox>
          </td>
        </tr>
        <tr>
          <td>
            <b>Server</b>
          </td>
          <td>
            <asp:textbox id=tbServer runat="server">
            </asp:textbox>
          </td>
        </tr>
        <tr>
          <td>
            <asp:label id=lbState runat="server" 
                       enableviewstate="False" font-bold="True"></asp:label>
          </td>
          <td align=right>
            <asp:button id=btTest runat="server" 
                        enableviewstate="False" text="Test connection">
            </asp:button>
          </td>
        </tr>
      </tbody>
    </table>
    <br>
    <br>
    <asp:datagrid id=dataGrid 
                  style="Z-INDEX: 100; LEFT: 14px; POSITION: absolute; TOP: 334px" 
                  runat="server" font-names="Verdana" font-size="8pt" 
                  width="700px" backcolor="#CCCCFF" bordercolor="Black" 
                  cellpadding="3">
      <headerstyle backcolor="#AAAADD">
      </headerstyle>
      <columns>
        <asp:editcommandcolumn buttontype="LinkButton" 
                               updatetext="Update" canceltext="Cancel" 
                               edittext="Edit">
        </asp:editcommandcolumn>
        <asp:buttoncolumn text="Delete" commandname="Delete">
          <headerstyle width="60px">
          </headerstyle>
        </asp:buttoncolumn>
      </columns>
    </asp:datagrid>
  </form>
</body>
</html>