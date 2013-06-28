unit dclunidac10; 

interface

uses
  UniUpdateSQLEditor, UniScriptEditor, UniStoredProcEditor, UniTableEditor, 
    UniTableSQLFrame, UniSQLEditor, UniQueryOptionsFrame, UniQueryEditor, 
    UniSPCallFrame, UniParamsFrame, UniSQLOptionsFrame, UniConnectionEditor, 
    UniSpecificOptionsFrame, UniSpecificOptionsEditor, UniReg, UniDesign,
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('UniReg', @UniReg.Register); 
  RegisterUnit('UniDesign', @UniDesign.Register); 
end; 

initialization
  RegisterPackage('dclunidac10', @Register); 
end.
