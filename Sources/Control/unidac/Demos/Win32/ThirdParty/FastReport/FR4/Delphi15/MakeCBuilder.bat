echo off
rem Path to DCC32.exe
SET DCCPATH=%PROGRAMFILES%\Embarcadero\RAD Studio\8.0\bin\
rem Path to project Dir
rem SET PROJECTPATH=%PROGRAMFILES%\Devart\UniDac for RAD Studio XE\Demos\Win32\ThirdParty\FastReport\FR4\Delphi15\
SET PROJECTPATH=D:\Projects\Delphi\UniDac\Demos\Win32\ThirdParty\FastReport\FR4\Delphi15\
rem FastReport 4 LibD15 Path
SET FRLIBDLLPATH=%PROGRAMFILES%\FastReports\FastReport 4\LibD15


echo on
rem -----------------------BEGIN------------------------------------------
echo off
cd "%PROJECTPATH%"

rem FastReport
"%DCCPATH%DCC32.EXE" -LE. -B -JL frxDAC15.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac150;fs15;fsDB15"
"%DCCPATH%DCC32.EXE" -LE. -B -JL frxUniDAC15.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac150;fs15;fsDB15"
"%DCCPATH%DCC32.EXE" -LE. -B -JL dclfrxUniDAC15.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac150;fs15;fsDB15"
rem FastScript
"%DCCPATH%DCC32.EXE" -LE. -B -JL fsDAC15.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac150;fs15;fsDB15"
"%DCCPATH%DCC32.EXE" -LE. -B -JL fsUniDAC15.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac150;fs15;fsDB15"
"%DCCPATH%DCC32.EXE" -LE. -B -JL dclfsUniDAC15.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac150;fs15;fsDB15"