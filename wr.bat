@echo off
setlocal
if "%1"=="" goto fail

set NAME=%~n1
misdeed dump_wr %NAME%.mis > %NAME%.wr.txt
goto end

:fail
echo gimme a filename you fool

:end