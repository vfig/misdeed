@echo off
setlocal enableextensions

set basedir=%~dp0
set srcdir=%basedir%
set builddir=%basedir%
set clargs=%builddir%\clargs

if defined VCToolsInstallDir (
    rem Ok
) else (
    echo Visual Studio tools ^(%%VCToolsInstallDir%%^) not found.
    goto fail
)

if NOT "%VSCMD_ARG_TGT_ARCH%"=="x64" (
    echo Target architecture is %VSCMD_ARG_TGT_ARCH% not x64'.
    goto fail
)

if not exist %builddir%\ mkdir %builddir%\
if errorlevel 1 (
    echo Failed to create directory: %builddir%
    goto fail
)

rem Log important settings
rem echo:basedir = %basedir%
rem echo:srcdir = %srcdir%
rem echo:builddir = %builddir%
rem echo:

echo:> %clargs%
echo /nologo >> %clargs%
rem Debug configuration
echo /Od >> %clargs%
echo /DDEBUG=1 >> %clargs%
echo /DWINDOWS=1 >> %clargs%
echo /DVISUALCPP=1 >> %clargs%
echo /DDEBUG_MEMORY=0 >> %clargs%
echo /D_CRT_SECURE_NO_WARNINGS=1 >> %clargs%
echo /Zi >> %clargs%
echo /Fd:%builddir%\misdeed.pdb >> %clargs%
echo /MD >> %clargs%
rem echo /fsanitize=address >> %clargs%
rem Output configuration
echo /Fo:%builddir%\ >> %clargs%
rem Compile configuration
echo /TC >> %clargs%
echo /W4 >> %clargs%
rem echo /WX >> %clargs% -------------- warnings as errors
rem C4201: nonstandard extension used: nameless struct/union
echo /wd4201 >> %clargs%
rem C4100: unreferenced formal parameter
echo /wd4100 >> %clargs%
echo /utf-8 >> %clargs%
rem Link configuration
echo /link /WX >> %clargs%
rem echo /link gdi32.lib kernel32.lib shell32.lib user32.lib >> %clargs%
echo /link /out:%builddir%\misdeed.exe >> %clargs%
rem Sources
echo %srcdir%\misdeed.c >> %clargs%

rem Compile
cl @%clargs%
if errorlevel 1 goto fail

rem Exit with success
:success
echo:Build ok.
endlocal
exit /b 0

rem Exit with error
:fail
echo:Build failed.
endlocal
exit /b 1
