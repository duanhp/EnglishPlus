@echo off
rem **********************************************************************
rem *
rem * Unidac for Delphi XE
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Embarcadero\RAD Studio\8.0
call ..\Make.bat Delphi 15 WIN32
