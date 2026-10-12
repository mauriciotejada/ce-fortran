# Windows Installation Package

The Windows installation package provides three batch files that serve different purposes:

1. An installation file `install_fortran.bat` to install Fortran on your Windows computer.
2. A file `uninstall_fortran.bat` that completely removes the Fortran installation from your computer.
3. A file `update_toolbox.bat` that compiles and copies the latest version of our toolbox to your system.

All installation files have been tested with Windows 10, but should work down to Windows 7, as long as Power Shell is available.



## Installation

When you execute the file `install_fortran.bat`, it will install the [GNU Fortran Compiler](https://gcc.gnu.org/fortran/) environment, the plotting program [gnuplot](http://gnuplot.info/), and prompt you to choose your preferred development editor: **Visual Studio Code** or **Geany**. In addition, it will compile and store the toolbox on your computer.


#### The installation process in detail

The file `install_fortran.bat` is a batch file used to execute installation routines on your computer. You can run this file by double-clicking on it. During the installation process, changes are made to your system registry requiring administrator rights (granted via the UAC prompt). Once administrator rights are granted, confirm installation by typing `y` and pressing ENTER. We recommend using the default installation directory `C:\cygwin`.

You will then be prompted to select your editor of choice:
1. **Visual Studio Code (VS Code)** (default)
2. **Geany**

In detail, you will see the following happening:

1. The installer installs the GNU Fortran compiler and related build tools using [Cygwin](https://www.cygwin.com).
2. Next, it installs [gnuplot](http://gnuplot.info/).
3. Depending on your editor selection:
   - **Visual Studio Code**: The installer checks for VS Code (installing it if not already present), installs the Fortran linter (`fortran-lang.linter-gfortran`) and LLDB debugger (`vadimcn.vscode-lldb`) extensions, and merges Fortran compile, build, run, and debug tasks into `%APPDATA%\Code\User\tasks.json` and `settings.json` (backing up any existing files to `.bak`).
   - **Geany**: The installer downloads and installs Geany silently, configures its build menu (`filetypes.fortran`) and editor behavior (`geany.conf`), and optionally associates Fortran file extensions (`.f90`, `.f77`, etc.) with Geany.
4. Finally, it compiles and installs the `ce-fortran` toolbox (`toolbox.o`, `toolbox_debug.o`, `toolbox.mod`) to `%location%\include\`.

**This is it!** If you saw no error messages appearing, then Fortran as well as our toolbox were successfully installed on your system.

#### Working with your chosen editor:
- **VS Code**: Open any `.f90` file and press `Ctrl+Shift+B` to compile and build. Use **Terminal > Run Task...** to select **Fortran: Run Program**, or press `F5` to start debugging.
- **Geany**: Open any `.f90` file and use the **Build** menu (or shortcuts `F8` to compile, `F9` to build, and `F5` to execute).


#### Changing the installation directory

We strongly recommend that you use the standard installation directory, which is `C:\cygwin`. If for some reasons you are not able to install Fortran to this directory, you can change the installation directory according to the following steps:

1. You have to edit the file `install_fortran.bat`. To do so, right-click on the file and choose Edit from the drop-down menu that appears.
2. A text editor should appear that shows the content of the batch file. Go to line 12 of the batch file, which reads
    ```
    SET "location=C:\cygwin"
    ```
3. Now you can change `C:\cygwin` to the directory you want Fortran to be installed to. For example, if you want Fortran to be located in `D:\Fortran\myfortran`, you should change line 14 to
    ```
    SET "location=D:\Fortran\myfortran"
    ```
4. Save the file `install_fortran.bat` and exit the text editor.
5. Now run `install_fortran.bat` again. The installation directory should have changed.



## Uninstallation

In the unlikely event that you want to uninstall all the software components as well as the toolbox from your system, you can use the file `uninstall_fortran.bat`. It follows the same logic as the installation batch file. You just have to follow the instructions. Don’t forget to change the location of your Fortran installation, if you have changed the installation directory according to bullet point 3.


## Toolbox Update

Sometimes you might feel that your toolbox is out of date. In this case, just pull the most recent version of this github repository and execute the file `update_toolbox.bat`. This will compile the latest toolbox version and store is such that you can use it in all your programs.


## Run your first Fortran program

If you've successfully installed Fortran on you computer, [you can learn here how to run your first program](https://www.ce-fortran.com/run-program-win/).
