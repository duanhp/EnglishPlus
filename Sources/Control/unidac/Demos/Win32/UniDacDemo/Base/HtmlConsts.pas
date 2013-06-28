unit HtmlConsts;

interface

const
  HTMLHeader: string =
    '<!doctype html public "-//w3c//dtd html 4.0 Transitional//en">'#13#10 +
    '<html>'#13#10 +
    '<head>'#13#10 +
    '  <title>Samples - Oracle Data Access Components</title>'#13#10 +
    '<style>'#13#10 +
    'body {'#13#10 +
    '  margin: 5px 5px 5px 5px;'#13#10 +
    '  padding: 0px 0px 0px 0px;'#13#10 +
    '  background: #ffffff; '#13#10 +
    '  color: #000000;'#13#10 +
    '  font-family: Verdana, Arial, Helvetica, sans-serif;'#13#10 +
    '  font-size: 70%;'#13#10 +
    '  width: 100%;'#13#10 +
    '  }'#13#10 +
    'p'#13#10 +
    '	{'#13#10 +
    '	margin: .5em 0em .5em 0em;'#13#10 +
    '	}'#13#10 +
    'ol, ul'#13#10 +
    '	{'#13#10 +
    '	margin-top: .5em; '#13#10 +
    '	}'#13#10 +
    'li'#13#10 +
    '	{'#13#10 +
    '	margin-bottom: .5em;'#13#10 +
    '	}'#13#10 +
    'ul p, ol p, dl p'#13#10 +
    '	{'#13#10 +
    '	margin-left: 0em;'#13#10 +
    '	}'#13#10 +
    'blockquote.dxOverload'#13#10 +
    '	{'#13#10 +
    '	margin: .5em 1.5em .5em 1.5em;'#13#10 +
    '	}'#13#10 +
    'h1, h2, h3, h4'#13#10 +
    '	{'#13#10 +
    '	font-family: Verdana, Arial, Helvetica, sans-serif;'#13#10 +
    '	margin-bottom: .4em; '#13#10 +
    '	margin-top: 1em;'#13#10 +
    '	font-weight: bold;'#13#10 +
    '	}'#13#10 +
    'h1'#13#10 +
    '	{'#13#10 +
    '	font-size: 120%;'#13#10 +
    '	margin-top: 0em;'#13#10 +
    '	}'#13#10 +
    'h2'#13#10 +
    '	{'#13#10 +
    '	font-size: 130%;'#13#10 +
    '	}'#13#10 +
    'h3'#13#10 +
    '	{'#13#10 +
    '	font-size: 115%;'#13#10 +
    '	}'#13#10 +
    'h4'#13#10 +
    '	{'#13#10 +
    '	font-size: 100%;'#13#10 +
    '	}'#13#10 +
    '.dxH1, .dxH2, .dxH3, .dxH4'#13#10 +
    '	{'#13#10 +
    '	margin-left: -18px;	'#13#10 +
    '	}'#13#10 +
    'A'#13#10 +
    '  {'#13#10 +
    '  color: #0000FF;'#13#10 +
    '  }'#13#10 +
    'A:link'#13#10 +
    '  {'#13#10 +
    '  color: #0000FF;'#13#10 +
    '  }'#13#10 +
    'A:active'#13#10 +
    '  {'#13#10 +
    '  color: #000080;'#13#10 +
    '  }'#13#10 +
    'A:visited'#13#10 +
    '  {'#13#10 +
    '  color: #800080;'#13#10 +
    '  }'#13#10 +
    'A:hover'#13#10 +
    '  {'#13#10 +
    '  color: #FF0000;'#13#10 +
    '  }'#13#10 +
    'pre'#13#10 +
    '  {'#13#10 +
    '  font-family: Courier New;'#13#10 +
    '  font-size: 9pt;'#13#10 +
    '  color: #000060;'#13#10 +
    '  margin-top: 0px;'#13#10 +
    '  }'#13#10 +
    'code, p.example, p.sourcecode, table.sourcecode, tr.sourcecode, td.sourcecode'#13#10 +
    '  {'#13#10 +
    '  font-family: Courier New;'#13#10 +
    '  font-size: 9pt;'#13#10 +
    '  color: #000060;'#13#10 +
    '  }'#13#10 +
    'table.filtereditemlisttable, table.xmldoctable'#13#10 +
    '	{'#13#10 +
    '	width: 95%; '#13#10 +
    '	margin-top: .6em;'#13#10 +
    '	margin-left: .6em;'#13#10 +
    '	margin-bottom: .3em;'#13#10 +
    '	border-width: 1px 1px 0px 0px;'#13#10 +
    '	border-style: solid;'#13#10 +
    '	border-color: #999999;'#13#10 +
    '	background-color: #999999; '#13#10 +
    '	font-size: 100%;'#13#10 +
    '  border-collapse:collapse;'#13#10 +
    '	}'#13#10 +
    'table.filtereditemlisttable th, table.filtereditemlisttable td, table.xmldoctable th, table.xmldoctable td'#13#10 +
    '	{ '#13#10 +
    '	border-width: 0px 0px 1px 1px;'#13#10 +
    '	border-style: solid;'#13#10 +
    '	border-color: #999999;'#13#10 +
    '	padding: 4px 6px;'#13#10 +
    '	text-align: left;'#13#10 +
    '	vertical-align: top;'#13#10 +
    '	}'#13#10 +
    'table.filtereditemlisttable th, table.xmldoctable th'#13#10 +
    '	{ '#13#10 +
    '	background: #cccccc; '#13#10 +
    '	vertical-align: bottom;'#13#10 +
    '	}'#13#10 +
    'table.filtereditemlisttable td, table.xmldoctable td'#13#10 +
    '	{'#13#10 +
    '	background-color: #ffffff;'#13#10 +
    '	vertical-align: top;'#13#10 +
    '	}'#13#10 +
    '</style>'#13#10 +
    '</head>'#13#10 +
    '<body>'#13#10;

  HTMLFooter: string = '</body>' + #13#10 + '</html>';

implementation

end.
