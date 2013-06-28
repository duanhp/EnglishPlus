@echo off
rem **********************************************************************
rem *
rem * Unidac for Delphi 10
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\Borland\BDS\4.0
call ..\Make.bat Delphi 10 WIN32
call ..\Make.bat Delphi 10 CLR
