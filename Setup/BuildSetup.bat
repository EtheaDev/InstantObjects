"C:\Program Files (x86)\Inno Setup 6\iscc.exe" "D:\ETHEA\InstantObjects\Setup\Setup.iss"
set INNO_STATUS=%ERRORLEVEL%
if %INNO_STATUS%==0 GOTO SIGNSETUP
pause
EXIT

:SIGNSETUP
call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\InstantObjects\Setup\Output\InstantObjects_Setup.exe

:END
pause
