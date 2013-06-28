program FailOver;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Data in 'Data.pas' {DM: TDataModule},
  About in 'About.pas' {AboutForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
