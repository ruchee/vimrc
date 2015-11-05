@echo off

IF EXIST packages\FAKE\tools\Fake.exe GOTO FAKEINSTALLED

paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)

paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

:FAKEINSTALLED

packages\FAKE\tools\FAKE.exe install.fsx %*

