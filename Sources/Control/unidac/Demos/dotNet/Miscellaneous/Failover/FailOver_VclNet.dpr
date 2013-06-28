program FailOver_VclNet;

{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\system.drawing.dll'}
{%DelphiDotNetAssemblyCompiler 'Devart.Dac.dll'}
{%DelphiDotNetAssemblyCompiler 'Devart.UniDac.dll'}

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
