Universal Data Access Components Source Code
Copyright 1997-2011, Devart. All Rights Reserved

There are two ways to compile and install UniDAC for Windows manually.

I. Using IDE

Delphi and C++ Builder for Win32
--------------------------------

Run your IDE and walk through folowing steps:
  1) Compile DAC run-time package (dacXX.dpk)
  2) Compile DAC GUI related package dacvclXX.dpk
  3) Compile DAC design-time package (dcldacXX.dpk)
  4) Compile UniDAC run-time package (unidacXX.dpk)
  5) Compile UniDAC GUI related package (unidacvclXX.dpk)
     If you are going to create CLX applications compile UniDacClx.pas unit separately.
  6) Compile and install UniDAC design-time package (dclunidacXX.dpk)
  7) Compile providers packages:
    oraproviderXX.dpk, msproviderXX.dpk, myproviderXX.dpk, ibproviderXX.dpk,
    pgproviderXX.dpk, liteproviderXX.dpk, nexusproviderXX.dpk, odbcproviderXX.dpk.
    After you have compiled odbcproviderXX.dpk, compile providers that use ODBC:
    accessproviderXX.dpk, adsproviderXX.dpk, aseproviderXX.dpk, db2providerXX.dpk.

You can find these packages in 
  Source\Delphi6\*.dpk - for Delphi 6 
  Source\CBuilder6\*.bpk - for C++ Builder 6
  Source\Delphi7\*.dpk - for Delphi 7 
  Source\Delphi9\*.dpk - for Delphi 2005
  Source\Delphi10\*.dpk - for BDS 2006
  Source\Delphi11\*.dpk - for RAD Studio 2007
  Source\Delphi12\*.dpk - for RAD Studio 2009
  Source\Delphi14\*.dpk - for RAD Studio 2010
  Source\Delphi15\*.dpk - for RAD Studio XE
  Source\Delphi16\*.dpk - for RAD Studio XE2

To compile UniDAC based application add UniDAC Source directory path 
to the "Library Path".

Delphi for .NET
-----------------

Run your IDE and walk through folowing steps:
  1) Compile DAC run-time package (Devart.Dac.dpk)
  2) Compile DAC design-time package (Devart.Dac.Design.dpk)
  3) Compile UniDAC run-time package (Devart.UniDac.dpk)
  4) Compile and install UniDAC design-time package (Devart.UniDac.Design.dpk)
  5) Compile providers packages:
    Devart.UniDac.Oracle.dpk, Devart.UniDac.SQLServer.dpk, Devart.UniDac.MySQL.dpk,
    Devart.UniDac.InterBase.dpk, Devart.UniDac.PostgreSQL.dpk,
    Devart.UniDac.SQLLite.dpk, Devart.UniDac.ODBC.dpk.
    After you have compiled Devart.UniDac.ODBC.dpk, compile providers that use ODBC:
    Devart.UniDac.Access.dpk, Devart.UniDac.Advantage.dpk, Devart.UniDac.ASE.dpk,
    Devart.UniDac.DB2.dpk.
  6) Specify the path to compiled assembles in "Assembly Search Paths"

You can find these packages in 
  Source\Delphi9\*.dpk - for Delphi 2005
  Source\Delphi10\*.dpk - for BDS 2006
  Source\Delphi11\*.dpk - for RAD Studio 2007

To compile UniDAC based application add Devart.Dac and Devart.UniDac to 
Namespace prefixes, add UniDAC Source directory path to the "Library Path" list.

II. Using make-files

Delphi and C++ Builder for Win32
--------------------------------

  1) Go to one of the following folders (let's denote this folder %MakePath%):
     Source\Delphi6 - for Delphi 6
     Source\CBuilder6 - for C++ Builder 6
     Source\Delphi7 - for Delphi 7
     Source\Delphi9 - for Delphi 2005
     Source\Delphi10\*.dpk - for BDS 2006
     Source\Delphi11\*.dpk - for RAD Studio 2007
     Source\Delphi12\*.dpk - for RAD Studio 2009
     Source\Delphi14\*.dpk - for RAD Studio 2010
     Source\Delphi15\*.dpk - for RAD Studio XE
     Source\Delphi16\*.dpk - for RAD Studio XE2

  2) Find in the 'Make.bat' line containing 

     set IdeDir="D:\Program Files\Borland\Delphi7

     and make sure that correct path to IDE is set (always include forward
     quote and do not include ending quote)

  3) Run 'Make.bat'. Binaries will be copied to %MakePath%\UniDac subfolder
  4) Copy %MakePath%\UniDac\*.bpl files to a folder that is included in the
     PATH environment variable
  5) Run IDE and add dclunidacXX.bpl via Component->Install Packages... menu 
  6) To compile UniDAC based application add UniDAC Source directory path 
     to the "Library Path" list

Delphi for .NET
-----------------

  1) Go to the following folders (let's denote this folder %MakePath%):
     Source\Delphi9\*.dpk - for Delphi 2005
     Source\Delphi10\*.dpk - for BDS 2006
     Source\Delphi11\*.dpk - for RAD Studio 2007

  2) Find in the 'Make.bat' line containing 

     set IdeDir="D:\Program Files\Borland\BDS\4.0

     and make sure that correct path to IDE is set (always include forward
     quote and do not include ending quote)

  3) Run 'Make.bat'. Binaries will be copied to %MakePath%\UniDac subfolder
  4) Run IDE and add Devart.UniDac.Design.dll via Component->Installed 
     .NET Components->.NET VCL Components->Add... menu. Specify the path to compiled assembles 
     in Component->Assembly Search Paths->Add... menu

  6) To compile UniDAC based application add Devart.Dac and Devart.UniDac to 
     Tools->Options->Environment Options->Delphi Options->Library->
     Namespace prefixes