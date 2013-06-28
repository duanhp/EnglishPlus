echo off
rem Path to DCC32.exe
SET DCCPATH=%PROGRAMFILES%\Embarcadero\RAD Studio\7.0\bin\
rem Path to project Dir
rem SET PROJECTPATH=%PROGRAMFILES%\Devart\UniDac for RAD Studio 2010\Demos\Win32\ThirdParty\FastReport\FR4\Delphi14\
SET PROJECTPATH=D:\Projects\Delphi\UniDac\Demos\Win32\ThirdParty\FastReport\FR4\Delphi14\
rem FastReport 4 LibD14 Path
SET FRLIBDLLPATH=%PROGRAMFILES%\FastReports\FastReport 4\LibD14


echo on
rem -----------------------BEGIN------------------------------------------
echo off
cd "%PROJECTPATH%"

rem FastReport
"%DCCPATH%DCC32.EXE" -LE. -B -JL frxDAC14.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac140;fs14;fsDB14"
"%DCCPATH%DCC32.EXE" -LE. -B -JL frxUniDAC14.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac140;fs14;fsDB14"
"%DCCPATH%DCC32.EXE" -LE. -B -JL dclfrxUniDAC14.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac140;fs14;fsDB14"
rem FastScript
"%DCCPATH%DCC32.EXE" -LE. -B -JL fsDAC14.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac140;fs14;fsDB14"
"%DCCPATH%DCC32.EXE" -LE. -B -JL fsUniDAC14.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac140;fs14;fsDB14"
"%DCCPATH%DCC32.EXE" -LE. -B -JL dclfsUniDAC14.dpk -N0. -NO. -NH. -NB. -U"..\;%FRLIBDLLPATH%" -I"..\;%FRLIBDLLPATH%" -LU"dac140;fs14;fsDB14"