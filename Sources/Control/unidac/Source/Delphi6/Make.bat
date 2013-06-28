@echo off
rem **********************************************************************
rem *
rem * Unidac for Delphi 6
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Borland\Delphi6
call ..\Make.bat Delphi 6
