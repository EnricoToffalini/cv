@echo off
setlocal

set "ROOT=%~dp0"
set "RSCRIPT="

for /d %%D in ("%ProgramFiles%\R\R-*") do (
  if exist "%%~fD\bin\Rscript.exe" set "RSCRIPT=%%~fD\bin\Rscript.exe"
)

if not defined RSCRIPT (
  echo Rscript.exe was not found under "%ProgramFiles%\R".
  exit /b 1
)

pushd "%ROOT%"
"%RSCRIPT%" --vanilla scripts\render_all.R %*
set "STATUS=%ERRORLEVEL%"
popd

exit /b %STATUS%
