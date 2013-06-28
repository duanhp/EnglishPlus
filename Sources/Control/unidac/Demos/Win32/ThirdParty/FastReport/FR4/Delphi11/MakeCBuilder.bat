echo off
rem Path to DCC32.exe
SET DCCPATH=%PROGRAMFILES%\CodeGear\RAD Studio\5.0\bin\
rem Path to project Dir
rem SET PROJECTPATH=%PROGRAMFILES%\Devart\UniDac for RAD Studio 2007\Demos\Win32\ThirdParty\FastReport\FR4\Delphi11\
SET PROJECTPATH=D:\Projects\Delphi\UniDac\Demos\Win32\ThirdParty\FastReport\FR4\Delphi11\
rem FastReport 4 LibD11 Path
SET FRLIBDLLPATH=%PROGRAMFILES%\FastReports\FastReport 4\LibD11


echo on
rem -----------------------BEGIN------------------------------------------
echo off
cd "%PROJECTPATH%"

rem FastReport
"%DCCPATH%DCC32.EXE" -LE. -B -JL frxDAC11.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac105;fs11;fsDB11"
"%DCCPATH%DCC32.EXE" -LE. -B -JL frxUniDAC11.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac105;fs11;fsDB11"
"%DCCPATH%DCC32.EXE" -LE. -B -JL dclfrxUniDAC11.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac105;fs11;fsDB11"
rem FastScript
"%DCCPATH%DCC32.EXE" -LE. -B -JL fsDAC11.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac105;fs11;fsDB11"
"%DCCPATH%DCC32.EXE" -LE. -B -JL fsUniDAC11.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac105;fs11;fsDB11"
"%DCCPATH%DCC32.EXE" -LE. -B -JL dclfsUniDAC11.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac105;fs11;fsDB11"