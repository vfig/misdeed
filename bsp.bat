@echo off
setlocal
if "%1"=="" goto fail

set NAME=%~n1
misdeed dump_bsp %NAME%.mis -o %NAME%.dot && dot2svg.bat %NAME%.dot
goto end

:fail
echo gimme a filename you fool

:end