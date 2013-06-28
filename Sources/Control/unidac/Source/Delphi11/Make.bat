@echo off
rem **********************************************************************
rem *
rem * Unidac for Delphi 11
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\CodeGear\RAD Studio\5.0
call ..\Make.bat Delphi 11 WIN32
call ..\Make.bat Delphi 11 CLR
