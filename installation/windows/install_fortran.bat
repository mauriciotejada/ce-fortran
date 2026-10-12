:: A SHELL SCRIPT FOR INSTALLING FORTRAN TO A WINDOWS SYSTEM
::
:: This code is published under the GNU General Public License v3
::                         (https://www.gnu.org/licenses/gpl-3.0.en.html)
::
:: Author: Fabian Kindermann (contact@ce-fortran.com)

@ECHO off
chcp 1252

SET "location=C:\cygwin"


:: GET INTO ADMINSTRATOR MODE
:checkPrivileges 
NET FILE 1>NUL 2>NUL
if '%errorlevel%' == '0' ( goto gotPrivileges ) else ( goto getPrivileges ) 

:getPrivileges 
if '%1'=='ELEV' (shift & goto gotPrivileges)  

setlocal DisableDelayedExpansion
set "batchPath=%~0"
setlocal EnableDelayedExpansion
ECHO Set UAC = CreateObject^("Shell.Application"^) > "%temp%\OEgetPrivileges.vbs" 
ECHO UAC.ShellExecute "!batchPath!", "ELEV", "", "runas", 1 >> "%temp%\OEgetPrivileges.vbs" 
%SystemRoot%\System32\CScript.exe "%temp%\OEgetPrivileges.vbs" 
exit /B 

:gotPrivileges
@setlocal enableextensions
@cd /d "%~dp0"


:: ASK FOR INSTALLATION DIRECTORY CHANGES
ECHO.
ECHO This script installs Fortran and its development components into the directory: 
ECHO.
ECHO    %location%
ECHO.
ECHO To change the installation directory take a look at the readme file.
ECHO. 
set /p temp=Do you want to continue (y/n)?
ECHO.

set exitres=T
if %temp:~-0,1%==y set exitres=F
if %temp:~-0,1%==Y set exitres=F
if %exitres%==T exit


:: GENERATE DOWNLOAD FOLDER FOR EVERYTHING THAT NEEDS TO BE DOWNLOADED
mkdir downloads
@cd /d downloads


:: INSTALL CYGWIN

:: download the cygwin installation file
@"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" -NoProfile -InputFormat None -ExecutionPolicy Bypass -Command "$WebClient = New-Object System.Net.WebClient; $WebClient.DownloadFile('http://cygwin.com/setup-x86_64.exe','cygwin.exe')"

:: use cygwin installer
cygwin.exe -q -s http://ftp.fau.de/cygwin --no-shortcuts --root "%location%" --packages bash,gcc-core,gcc-fortran,gccmakedep,colorgcc,gdb,make,git,wget

:: move the cygwin.exe file to the cygwin directory for later use
move "cygwin.exe" "%location%\bin"


:: UPDATE PATH VARIABLE

:: backup current registry key
reg add "HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v Path_backup /t REG_EXPAND_SZ /d "%PATH%" /f

:: update path variable (if necessary)
ECHO.%PATH% | findstr /C:"%location%\bin" 1>nul
if errorlevel 1 (
    set "PATH=%PATH%;%location%\bin"
)

ECHO.%PATH% | findstr /C:"%location%\usr\bin" 1>nul
if errorlevel 1 (
    set "PATH=%PATH%;%location%\usr\bin"
)

:: add path to environment variables
reg add "HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v Path /t REG_EXPAND_SZ /d "%PATH%" /f


:: INSTALL GNUPLOT

:: write the setup log
ECHO [Setup] > "gnusetup.log"
ECHO Lang=en >> "gnusetup.log"
ECHO Group=gnuplot >> "gnusetup.log"
ECHO NoIcons=0 >> "gnusetup.log"
ECHO SetupType=full >> "gnusetup.log"
ECHO Components=core,docs,demo,license >> "gnusetup.log"
ECHO Tasks=defaulttermwxt,associate,associate\plt,associate\gp,associate\gpl,modifypath >> "gnusetup.log"

:: download the installer file
wget -O gnuplotins.exe https://sourceforge.net/projects/gnuplot/files/gnuplot/6.0.2/gp602-win64-mingw.exe

:: give execution rights
chmod +x gnuplotins.exe

:: run installer with configuration
gnuplotins.exe /SILENT /LOADINF="gnusetup.log"


:: ASK FOR EDITOR SELECTION
ECHO.
ECHO Which editor/IDE would you like to install and configure for Fortran?
ECHO   1) Visual Studio Code (VS Code)
ECHO   2) Geany
ECHO.
set /p editor_choice=Select an editor [1 or 2, default: 1]: 
if "%editor_choice%"=="" set editor_choice=1
if "%editor_choice%"=="2" goto :install_geany


:: INSTALL AND CONFIGURE VISUAL STUDIO CODE
:install_vscode
ECHO.
ECHO Checking Visual Studio Code installation...

set "CODE_CLI="
where code 1>nul 2>nul
if %errorlevel% equ 0 set "CODE_CLI=code"
if not defined CODE_CLI if exist "%LOCALAPPDATA%\Programs\Microsoft VS Code\bin\code.cmd" set "CODE_CLI=%LOCALAPPDATA%\Programs\Microsoft VS Code\bin\code.cmd"
if not defined CODE_CLI if exist "%ProgramFiles%\Microsoft VS Code\bin\code.cmd" set "CODE_CLI=%ProgramFiles%\Microsoft VS Code\bin\code.cmd"
if not defined CODE_CLI if exist "%ProgramFiles(x86)%\Microsoft VS Code\bin\code.cmd" set "CODE_CLI=%ProgramFiles(x86)%\Microsoft VS Code\bin\code.cmd"

if defined CODE_CLI (
    ECHO Visual Studio Code is already installed - skipping download.
) else (
    ECHO Downloading Visual Studio Code installer...
    @"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" -NoProfile -InputFormat None -ExecutionPolicy Bypass -Command "$WebClient = New-Object System.Net.WebClient; $WebClient.DownloadFile('https://update.code.visualstudio.com/latest/win32-x64-user/stable', 'vscode_setup.exe')"
    ECHO Running Visual Studio Code installer...
    vscode_setup.exe /VERYSILENT /NORESTART /MERGETASKS=!runcode,addtopath
    del /Q vscode_setup.exe 2>nul

    if exist "%LOCALAPPDATA%\Programs\Microsoft VS Code\bin\code.cmd" set "CODE_CLI=%LOCALAPPDATA%\Programs\Microsoft VS Code\bin\code.cmd"
    if not defined CODE_CLI if exist "%ProgramFiles%\Microsoft VS Code\bin\code.cmd" set "CODE_CLI=%ProgramFiles%\Microsoft VS Code\bin\code.cmd"
)

:: install extensions
if defined CODE_CLI (
    ECHO Installing Fortran extensions for VS Code...
    call "%CODE_CLI%" --install-extension fortran-lang.linter-gfortran
    call "%CODE_CLI%" --install-extension vadimcn.vscode-lldb
) else (
    ECHO WARNING: could not locate the vscode 'code' command line tool.
    ECHO Please install extensions fortran-lang.linter-gfortran and vadimcn.vscode-lldb manually.
)

:: configure VS Code for Fortran
@"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" -NoProfile -ExecutionPolicy Bypass -File "%~dp0configure_vscode.ps1" "%location%"

goto :install_toolbox


:: INSTALL GEANY
:install_geany

:: download file
wget -O geanyins.exe https://download.geany.org/geany-2.0_setup.exe

:: give execution rights
chmod +x geanyins.exe

:: run installer silently
geanyins.exe /S


:: PATCH GEANY FOR USE WITH FORTRAN

:: Fortran compilation file
set locnew=%location:\=\\%
ECHO [build-menu] >  "filetypes.fortran"
ECHO FT_00_LB=_Compile >> "filetypes.fortran"
ECHO FT_00_CM=gfortran -O3 -Wno-unused -frecursive -ffree-line-length-none -fopenmp -fimplicit-none -Wall -fcheck=bound,do -ffpe-trap=invalid,zero,overflow -I"%locnew%\\include" -c "%%f" -o "%%e.o" >> "filetypes.fortran"
ECHO FT_00_WD= >> "filetypes.fortran"
ECHO FT_01_LB=_Build >> "filetypes.fortran"
ECHO FT_01_CM=gfortran -O3 -fopenmp "%locnew%\\include\\toolbox.o" "%%e.o" -o prog >> "filetypes.fortran"
ECHO FT_01_WD= >> "filetypes.fortran"
ECHO FT_02_LB=_Debug >> "filetypes.fortran"
ECHO FT_02_CM=gfortran -Wno-unused -frecursive -ffree-line-length-none -fopenmp -fimplicit-none -Wall -fcheck=bound,do -ffpe-trap=invalid,zero,overflow -I"%locnew%\\include" -g "%%f" -o "%%e.o" "%locnew%\\include\\toolbox_debug.o" >> "filetypes.fortran"
ECHO FT_02_WD= >> "filetypes.fortran"
ECHO EX_00_LB=_Run Program >> "filetypes.fortran"
ECHO EX_00_CM="prog" >> "filetypes.fortran"
ECHO EX_00_WD= >> "filetypes.fortran"
ECHO EX_01_LB=_Run Debugger >> "filetypes.fortran"
ECHO EX_01_CM=gdb "%%e.o" >> "filetypes.fortran"
ECHO EX_01_WD= >> "filetypes.fortran"

:: geany editor behavior
ECHO [geany] >  "geany.conf"
ECHO indent_type=0 >>  "geany.conf"
ECHO pref_main_load_session=false >>  "geany.conf"
ECHO beep_on_errors=false >>  "geany.conf"

:: update geany configuration
mkdir "%userprofile%\AppData\Roaming\geany\filedefs\" 2>nul
move filetypes.fortran "%userprofile%\AppData\Roaming\geany\filedefs\filetypes.fortran"
move geany.conf "%userprofile%\AppData\Roaming\geany\geany.conf"

goto :install_toolbox


:: INSTALL THE TOOLBOX
:install_toolbox

gfortran -c -Werror -fopenmp -Wno-unused  -ffree-line-length-none -fimplicit-none -Wall -fcheck=bound,do -ffpe-trap=invalid,zero,overflow -frecursive -g ./../../toolbox/toolbox.f90 -o toolbox_debug.o
gfortran -c -O3 -fopenmp -ffree-line-length-none ./../../toolbox/toolbox.f90 -o toolbox.o
mkdir "%location%\include\" 2>nul
del /Q "%location%\include\toolbox.mod" 2>nul
del /Q "%location%\include\toolbox.o" 2>nul
del /Q "%location%\include\toolbox_debug.o" 2>nul
move "toolbox.mod" "%location%\include\"
move "toolbox.o" "%location%\include%\"
move "toolbox_debug.o" "%location%\include%\"


:: ASK FOR FILE ASSOCIATION (IF GEANY WAS SELECTED)
if not "%editor_choice%"=="2" goto :theend

ECHO.
ECHO.
ECHO I can associate all Fortran files with the editor Geany.
ECHO. 
set /p temp=Do you want me to do this (y/n)?
ECHO.

set exitres=T
if %temp:~-0,1%==y set exitres=F
if %temp:~-0,1%==Y set exitres=F
if %exitres%==T goto :theend

ASSOC .f77=Geany.ProjectFile
ASSOC .f90=Geany.ProjectFile
ASSOC .f95=Geany.ProjectFile
ASSOC .f03=Geany.ProjectFile
ASSOC .f08=Geany.ProjectFile


:: IF EVERYTHING RAN CORRECTLY, AT THIS POINT EVERYTHING SHOULD BE INSTALLED PROPERLY

:theend
ECHO. 
ECHO. 
ECHO. 
ECHO ...INSTALLATION COMPLETED.
ECHO.
if "%editor_choice%"=="2" (
    ECHO You can now open Geany to start writing and running Fortran programs.
    ECHO Use the Build menu to compile, build, and run your code.
) else (
    ECHO In VS Code, open a .f90 file and press Ctrl+Shift+B to compile and build it.
    ECHO Use "Terminal > Run Task..." for "Fortran: Run Program" and "Fortran: Debug Build",
    ECHO or press F5 to start the debugger.
)
ECHO.
ECHO YOU SHOULD RESTART YOUR SYSTEM TO FULLY ENJOY THE FORTRAN DEVELOPMENT ENVIRONMENT.
ECHO.
ECHO.
ECHO In case you encountered any problem, check on www.ce-fortran.com for help.
ECHO.
ECHO.
set /p temp=Press RETURN to end...
ECHO.