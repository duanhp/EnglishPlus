Universal Data Access Components
Copyright 1997-2011, Devart. All Rights Reserved
--------------------------------------------------

Demo for FastReport included in UniDAC was built and tested using
Fast Report 4

Note:
Fast Query Builder of FastReport is disabled by default. To enable them you
should find frx.inc file in the FastReport installation directory and uncomment
following line:
//{$DEFINE QBUILDER}

IMPORTANT NOTE:
  Demo is provided as is, and there is no warranty that it is fully
  compatible with other versions of Fast Report.

Before using Demo you should install FastReport 4 UniDAC Components.
The following instruction will help you to compile and install 
FastReport 4 UniDAC Components manually using IDE.

C++Builder 2007
---------------
If you have only C++Builder 2007, you should perform the following steps

  1. Make sure that you have UniDAC and FastReport installed
  2. Open the Demos\Win32\ThirdParty\FastReport\FR4\Delphi11\MakeCBuilder.bat file in edit mode
  3. Make sure that the following paths are assigned correctly:
       DCCPATH - path to the compiler DCC32.exe
       PROJECTPATH - path to packages from Fast_Report_Demo\Delphi11
       FRLIBDLLPATH - path to FastReport4_Inst_Dir\LibD11
  4. Save MakeCBuilder.bat
  5. Run MakeCBuilder.bat. After that all generated files will appear in the project path (PROJECTPATH)
  6. Copy PROJECTPATH\*.bpl files to a folder that is included in the PATH environment variable
  7. Run IDE and add dclfrxUniDAC11.bpl and dclfsUniDAC11.bpl via Component->Install Packages... menu 
  8. To compile applications with UniDAC for FastReport components, add PROJECTPATH to the "Library Path" and "Include Path" lists

Delphi and C++Builder for Win32
--------------------------------

Run your IDE and walk through the following steps:
  1) Compile DAC run-time package (frxDACXX.dpk)
  2) Compile UniDAC run-time package (frxUniDACXX.dpk)
  3) Compile and install UniDAC design-time package (dclfrxUniDACXX.dpk)

You can find these packages in 
  Demos\Win32\ThirdParty\FastReport\FR4\Delphi5\*.dpk - for Delphi 5 
  Demos\Win32\ThirdParty\FastReport\FR4\Delphi6\*.dpk - for Delphi 6 
  Demos\Win32\ThirdParty\FastReport\FR4\Delphi7\*.dpk - for Delphi 7 
  Demos\Win32\ThirdParty\FastReport\FR4\Delphi9\*.dpk - for Delphi 2005
  Demos\Win32\ThirdParty\FastReport\FR4\Delphi10\*.dpk - for Delphi 2006
  Demos\Win32\ThirdParty\FastReport\FR4\Delphi11\*.dpk - for Delphi 2007
  Demos\Win32\ThirdParty\FastReport\FR4\Delphi12\*.dpk - for Delphi 2009
  Demos\Win32\ThirdParty\FastReport\FR4\Delphi14\*.dpk - for Delphi 2010
  Demos\Win32\ThirdParty\FastReport\FR4\Delphi15\*.dpk - for Delphi XE
  Demos\Win32\ThirdParty\FastReport\FR4\CBuilder5\*.bpk - for C++Builder 5
  Demos\Win32\ThirdParty\FastReport\FR4\CBuilder6\*.bpk - for C++Builder 6

To compile applications based on FastReport 4 UniDAC Components, add the following 
path to the "Library Path":
%UniDAC%\Demos\Win32\ThirdParty\FastReport\FR4\
where %UniDAC% is the UniDAC installation path on your computer.