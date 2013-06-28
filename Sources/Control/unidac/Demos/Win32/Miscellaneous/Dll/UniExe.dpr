program UniExe;

uses
  ShareMem,
  Forms,
  ExeMain in 'ExeMain.pas' {fmExeMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmExeMain, fmExeMain);
  Application.Run;
end.
