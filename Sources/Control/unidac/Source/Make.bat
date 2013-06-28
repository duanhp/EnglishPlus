@echo off
rem **********************************************************************
rem *
rem * UniDAC
rem *
rem * Tasks:
rem *   1) Compile Dac packages;
rem *   2) Compile CRControls package;
rem *   2) Compile UniDAC packages;
rem *
rem * Command line:
rem *   call ..\_Make.bat IDEName IDEVer CLR
rem *   
rem * Parameters:
rem *   IDEName = (Delphi, CBuilder)
rem *   IDEVer = (6, 7, 9, 10, 11, 12, 14, 15, 16)
rem *   Platform = (CLR, WIN32, WIN64) WIN32 - default
rem **********************************************************************

rem Prepare ==============================================================
rem ======================================================================
set IDEName=%1
set IDEVer=%2
set Platform=%3
set PrjName=UniDAC
set PrjNameL=unidac

set OracleProvider=TRUE
set InterBaseProvider=TRUE
set MySQLProvider=TRUE
set SQLServerProvider=TRUE
set PostgreSQLProvider=TRUE
set SQLiteProvider=TRUE
set ODBCProvider=TRUE
set AccessProvider=TRUE
set DB2Provider=TRUE
set AdvantageProvider=TRUE
set ASEProvider=TRUE
set NexusDBProvider=FALSE

pushd

if not defined OracleProvider     set OracleProvider=FALSE
if not defined InterBaseProvider  set InterBaseProvider=FALSE
if not defined MySQLProvider      set MySQLProvider=FALSE
if not defined SQLServerProvider  set SQLServerProvider=FALSE
if not defined PostgreSQLProvider set PostgreSQLProvider=FALSE
if not defined SQLiteProvider     set SQLiteProvider=FALSE
if not defined ODBCProvider       set ODBCProvider=FALSE
if not defined AccessProvider     set AccessProvider=FALSE
if not defined DB2Provider        set DB2Provider=FALSE
if not defined AdvantageProvider  set AdvantageProvider=FALSE
if not defined ASEProvider        set ASEProvider=FALSE
if not defined NexusDBProvider    set NexusDBProvider=FALSE

rem Test IDEName
if %IDEName%A==DelphiA goto IDENameOK
if %IDEName%A==CBuilderA goto IDENameOK
echo Command line must be:
echo    call ..\Make.bat IDEName IDEVer
echo    IDEName = (Delphi, CBuilder)
goto Err
:IDENameOK

rem Test IDEVer
if %IDEVer%A==6A goto IDEVerOK
if %IDEVer%A==7A goto IDEVerOK
if %IDEVer%A==9A goto IDEVerOK
if %IDEVer%A==10A goto IDEVerOK
if %IDEVer%A==11A goto IDEVer11
if %IDEVer%A==12A goto IDEVerOK
if %IDEVer%A==14A goto IDEVerOK
if %IDEVer%A==15A goto IDEVerOK
if %IDEVer%A==16A goto IDEVerOK
echo Command line must be:
echo    call ..\_Make.bat IDEName IDEVer
echo    IDEVer = (6, 7, 9, 10, 11, 12, 14, 15, 16)
goto Err

:IDEVer11:
set PkgVer=105
goto PkgVerOK

:IDEVerOK
set PkgVer=%IDEVer%0

:PkgVerOK

if not %Platform%A==CLRA goto PlatformWin64
set PlatformDir=CLR
goto :PlatformOK
:PlatformWin64
if not %Platform%A==WIN64A goto PlatformWin32
set PlatformDir=Win64
goto :PlatformOK
:PlatformWin32
set Platform=WIN32
set PlatformDir=Win32
:PlatformOK

set CompilerOptions=-LE. -LN. -I.. -U..;..\..\Lib\Delphi16\%PlatformDir%;..\..\..\Common\Source;..\..\..\Common\Source\Delphi16 -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell

if %IDEVer%A==16A goto IDEVer16
set PlatformDir=.
set CompilerOptions=-LE. -LN. -I.. -U..
:IDEVer16

if %IDEName%A==CBuilderA goto CBuilder
if %Platform%A==CLRA goto Delphi8

rem Compile ==============================================================
if not %Platform%A==WIN32A goto Win64Compiler
set Compiler=%IdeDir%\Bin\dcc32.exe"
goto CompilerOK
:Win64Compiler
if not %Platform%A==WIN64A goto InvalidPlatform
set Compiler=%IdeDir%\Bin\dcc64.exe"
:CompilerOK

rem Compile DAC packages =================================================
%Compiler% %CompilerOptions% dac%PkgVer%.dpk
@if errorlevel 1 goto Err

%Compiler% %CompilerOptions% dacvcl%PkgVer%.dpk
@if errorlevel 1 goto Err

if %Platform%A==WIN64A goto SkipDcl
%Compiler% %CompilerOptions% dcldac%PkgVer%.dpk
@if errorlevel 1 goto Err
:SkipDcl

rem Compile CRControls package ===========================================
%Compiler% %CompilerOptions% crcontrols%PkgVer%.dpk
@if errorlevel 1 goto Err

if %Platform%A==WIN64A goto Skip_Dcl
%Compiler% %CompilerOptions% dclcrcontrols%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip_Dcl

rem Compile UniDAC packages ===========================================
%Compiler% %CompilerOptions% %PrjNameL%%PkgVer%.dpk
@if errorlevel 1 goto Err

%Compiler% %CompilerOptions% %PrjNameL%vcl%PkgVer%.dpk
@if errorlevel 1 goto Err

if %Platform%A==WIN64A goto Skip__Dcl
%Compiler% %CompilerOptions% dcl%PrjNameL%%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip__Dcl

rem Compile Providers packages ===========================================
if not %OracleProvider%==TRUE goto ibproviderW32
%Compiler% %CompilerOptions% oraprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:ibproviderW32
if not %InterBaseProvider%==TRUE goto myproviderW32
%Compiler% %CompilerOptions% ibprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:myproviderW32
if not %MySQLProvider%==TRUE goto msproviderW32
%Compiler% %CompilerOptions% myprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:msproviderW32
if not %SQLServerProvider%==TRUE goto pgproviderW32
%Compiler% %CompilerOptions% msprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:pgproviderW32
if not %PostgreSQLProvider%==TRUE goto liteproviderW32
%Compiler% %CompilerOptions% pgprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:liteproviderW32
if not %SQLiteProvider%==TRUE goto odbcproviderW32
%Compiler% %CompilerOptions% liteprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:odbcproviderW32
if not %ODBCProvider%==TRUE goto accessproviderW32
%Compiler% %CompilerOptions% odbcprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:accessproviderW32
if not %AccessProvider%==TRUE goto db2providerW32
%Compiler% %CompilerOptions% accessprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:db2providerW32
if not %DB2Provider%==TRUE goto adsproviderW32
%Compiler% %CompilerOptions% db2provider%PkgVer%.dpk
@if errorlevel 1 goto Err

:adsproviderW32
if not %AdvantageProvider%==TRUE goto aseproviderW32
%Compiler% %CompilerOptions% adsprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:aseproviderW32
if not %ASEProvider%==TRUE goto nexusproviderW32
%Compiler% %CompilerOptions% aseprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:nexusproviderW32
if not %NexusDBProvider%==TRUE goto SkipProviderW32
%Compiler% %CompilerOptions% nexusprovider%PkgVer%.dpk
@if errorlevel 1 goto Err

:SkipProviderW32

rem Copy files ===========================================================
rem ======================================================================

if exist *.bpl        move *.bpl               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.dcu        move *.dcu               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist ..\*.dcu     move ..\*.dcu            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.dcp        move *.dcp               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.dcu" move "%%i\*.dcu" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

copy ..\*.res            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

if %Platform%A==CLRA goto SkipD10BCCLib

if exist  *.bpi       move *.bpi               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist  *.lib       move *.lib               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist  *.hpp       move *.hpp               ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%
if exist  ..\*.hpp    move ..\*.hpp            ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%

for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.hpp" move "%%i\*.hpp" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

:SkipD10BCCLib

goto end

:Delphi8
rem Compile Delphi8 ======================================================
rem Compile DAC packages =================================================

%IdeDir%\Bin\dccil.exe" -LE. Devart.Dac.dpk
@if errorlevel 1 goto Err

%IdeDir%\Bin\dccil.exe" -LE. Devart.Dac.AdoNet.dpk
@if errorlevel 1 goto Err

%IdeDir%\Bin\dccil.exe" -LE. Devart.Dac.Design.dpk
@if errorlevel 1 goto Err

rem Compile CRControls package ===========================================
%IdeDir%\Bin\dccil.exe" -LE. Devart.Vcl.dpk -R..\
@if errorlevel 1 goto Err

rem Compile UniDAC packages ===========================================
%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.dpk
@if errorlevel 1 goto Err

%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.AdoNet.dpk
@if errorlevel 1 goto Err

%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.Design.dpk
@if errorlevel 1 goto Err

rem Compile Providers packages ===========================================
if not %OracleProvider%==TRUE goto ibproviderCLR
%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.Oracle.dpk -I..;..\UniProviders\Oracle -U..;..\UniProviders\Oracle -R..;..\UniProviders\Oracle
@if errorlevel 1 goto Err

:ibproviderCLR
if not %InterBaseProvider%==TRUE goto myproviderCLR
%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.InterBase.dpk -I.. -U.. -R..
@if errorlevel 1 goto Err

:myproviderCLR
if not %MySQLProvider%==TRUE goto msproviderCLR
%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.MySQL.dpk -I.. -U.. -R..
@if errorlevel 1 goto Err

:msproviderCLR
if not %SQLServerProvider%==TRUE goto pgproviderCLR
%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.SQLServer.dpk -I.. -U.. -R..
@if errorlevel 1 goto Err

:pgproviderCLR
if not %PostgreSQLProvider%==TRUE goto liteproviderCLR
%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.PostgreSQL.dpk -I.. -U.. -R..
@if errorlevel 1 goto Err

:liteproviderCLR
if not %SQLiteProvider%==TRUE goto odbcproviderCLR
%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.SQLite.dpk -I.. -U.. -R..
@if errorlevel 1 goto Err

:odbcproviderCLR
if not %ODBCProvider%==TRUE goto accessproviderCLR
%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.ODBC.dpk -I.. -U.. -R..
@if errorlevel 1 goto Err

:accessproviderCLR
if not %AccessProvider%==TRUE goto db2providerCLR
%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.Access.dpk -I.. -U.. -R..
@if errorlevel 1 goto Err

:db2providerCLR
if not %DB2Provider%==TRUE goto adsproviderCLR
%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.DB2.dpk -I.. -U.. -R..
@if errorlevel 1 goto Err

:adsproviderCLR
if not %AdvantageProvider%==TRUE goto aseproviderCLR
%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.Advantage.dpk -I.. -U.. -R..
@if errorlevel 1 goto Err

:aseproviderCLR
if not %ASEProvider%==TRUE goto SkipProviderCLR
%IdeDir%\Bin\dccil.exe" -LE. Devart.UniDac.ASE.dpk -I.. -U.. -R..
@if errorlevel 1 goto Err

:SkipProviderCLR

rem Copy files ===========================================================
rem ======================================================================

if exist *.dll      move *.dll               ..\..\Bin\%IDEName%%IDEVer%
if exist *.pdb      move *.pdb               ..\..\Bin\%IDEName%%IDEVer%
if exist *.dcpil    move *.dcpil             ..\..\Lib\%IDEName%%IDEVer%

if exist *.dcuil    move *.dcuil             ..\..\Lib\%IDEName%%IDEVer%

for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.dcuil" move "%%i\*.dcuil" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

copy ..\*.res            ..\..\Lib\%IDEName%%IDEVer%

goto end

:CBuilder
rem Compile ==============================================================
rem Compile DAC packages =================================================
cd %DacDir%

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dac%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dac%PkgVer%.mak
@if errorlevel 1 goto Err

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dacvcl%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dacvcl%PkgVer%.mak
@if errorlevel 1 goto Err

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dcldac%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dcldac%PkgVer%.mak
@if errorlevel 1 goto Err

rem Compile CRControls package ===========================================
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk CRControls%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f CRControls%PkgVer%.mak
@if errorlevel 1 goto Err
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dclCRControls%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dclCRControls%PkgVer%.mak
@if errorlevel 1 goto Err

rem Compile UniDAC packages ===========================================
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk %PrjNameL%%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f %PrjNameL%%PkgVer%.mak
@if errorlevel 1 goto Err

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk %PrjNameL%vcl%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f %PrjNameL%vcl%PkgVer%.mak
@if errorlevel 1 goto Err

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dcl%PrjNameL%%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dcl%PrjNameL%%PkgVer%.mak
@if errorlevel 1 goto Err

rem Compile Providers packages ===========================================
if not %OracleProvider%==TRUE goto ibproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk oraprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f oraprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:ibproviderCB
if not %InterBaseProvider%==TRUE goto myproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk ibprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f ibprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:myproviderCB
if not %MySQLProvider%==TRUE goto msproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk myprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f myprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:msproviderCB
if not %SQLServerProvider%==TRUE goto pgproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk msprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f msprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:pgproviderCB
if not %PostgreSQLProvider%==TRUE goto liteproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk pgprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f pgprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:liteproviderCB
if not %SQLiteProvider%==TRUE goto odbcproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk liteprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f liteprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:odbcproviderCB
if not %ODBCProvider%==TRUE goto accessproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk odbcprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f odbcprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:accessproviderCB
if not %AccessProvider%==TRUE goto db2providerCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk accessprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f accessprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:db2providerCB
if not %DB2Provider%==TRUE goto adsproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk db2provider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f db2provider%PkgVer%.mak
@if errorlevel 1 goto Err

:adsproviderCB
if not %AdvantageProvider%==TRUE goto aseproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk adsprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f adsprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:aseproviderCB
if not %ASEProvider%==TRUE goto nexusproviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk aseprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f aseprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:nexusproviderCB
if not %NexusDBProvider%==TRUE goto SkipProviderCB
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk nexusprovider%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f nexusprovider%PkgVer%.mak
@if errorlevel 1 goto Err

:SkipProviderCB

rem Copy files ===========================================================
rem ======================================================================

if exist *.bpl        move *.bpl               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.tds        move *.tds               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.mak        move *.mak               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%

if exist *.dcu        move *.dcu               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist ..\*.dcu     move ..\*.dcu            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.bpi        move *.bpi               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.lib        move *.lib               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.obj        move *.obj               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.hpp        move *.hpp               ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%
if exist ..\*.hpp     move ..\*.hpp            ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%

for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.obj" move "%%i\*.obj" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
for /f "tokens=*" %%i in ('dir/b/s/a:d ..\UniProviders\') do if exist "%%i\*.hpp" move "%%i\*.hpp" ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

copy ..\*.res            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

goto end

:InvalidPlatform
echo Invalid Platform

:Err
pause

:end
popd
