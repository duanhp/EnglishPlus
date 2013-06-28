@echo off
rem **********************************************************************
rem *
rem * UniDAC for CBuilder 6
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Borland\CBuilder6
call ..\Make.bat CBuilder 6
