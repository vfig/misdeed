@echo off
setlocal enableextensions

set basedir=%~dp0
set builddir=%basedir%

rem Run
cd %builddir%
%builddir%\misdeed.exe %*
if errorlevel 1 goto fail

rem Exit with success
:success
endlocal
exit /b 0

rem Exit with error
:fail
echo:Run failed.
endlocal
exit /b 1
