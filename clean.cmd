@echo off
setlocal enableextensions

set basedir=%~dp0..
set builddir=%basedir%\build

rem Clean
if exist %builddir%\ rmdir /s /q %builddir%
if errorlevel 1 goto fail

rem Exit with success
:success
echo:Clean ok.
endlocal
exit /b 0

rem Exit with error
:fail
echo:Clean failed.
endlocal
exit /b 1
