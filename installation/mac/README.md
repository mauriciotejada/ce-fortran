# macOS Installation Package

The macOS installation package provides three command files that serve different purposes:

1. An installation file `install_fortran.command` to install Fortran on your Mac.
2. A file `uninstall_fortran.command` that completely removes the Fortran installation from your Mac.
3. A file `update_toolbox.command` that compiles and copie the latest version of our toolbox to your system.

All installation files have been tested with macOS Big Sur, but should work on previous versions.

## Important security motice for macOS users ##

Starting with macOS Catalina, Apple has removed terminal access to the "Documents" and the "Download" folder for security purposes. As a result, when you download an installation files from this respository (into Documents or Downloads) and try to execute the respective install_fortran.command file, it will not execute correctly. To solve this issue, please copy the installation files into another folder of yours, e.g. create a folder "Programming" in your home directory. If you install fortran from there, installation (and uninstallation) should run smoothly.

The same is true when compiling, building and executing Fortran (.f90) files. You can't do this within your Documents or Download folder anymore. Please copy your source files to another folder, again e.g. create a folder "Programming" in your home directory.

There are some fixes for this issue on the internet. Yet, they involve changing access privileges to your hard drive, which we can not recommend. If anyone still faces problems with this, please open a thread on the forum.

## Installation

When you execute the file `install_fortran.command`, it will install the [GNU Fortran Compiler](https://gcc.gnu.org/fortran/) environment, the plotting program [gnuplot](http://gnuplot.info/), and prompt you to choose your preferred development editor: **Visual Studio Code** or **Geany**. In addition, it will compile and store the toolbox on your Mac.


#### The installation process in detail

The file `install_fortran.command` is a bash file used to execute the installation routines on your Mac. You can run this file by right-clicking on it and selecting `Open`. During the installation process, changes are made to lower system files requiring administrator privileges. When prompted, confirm installation by typing `y` and pressing ENTER.

You will then be prompted to select your editor of choice:
1. **Visual Studio Code (VS Code)** (default)
2. **Geany**

In detail, you will see the following:

1. The installer checks for [Homebrew](https://brew.sh) (supporting both Apple Silicon `/opt/homebrew` and Intel `/usr/local`) and installs the GNU Fortran compiler (`gcc`) and [gnuplot](http://gnuplot.info/).
2. Depending on your editor selection:
   - **Visual Studio Code**: The installer installs VS Code (if not already installed), adds the Fortran linter (`fortran-lang.linter-gfortran`) and LLDB debugger (`vadimcn.vscode-lldb`) extensions, and merges Fortran compile, build, run, and debug tasks into `tasks.json` and `settings.json` (backing up any existing files to `.bak`).
   - **Geany**: The installer installs Geany via Homebrew and configures its build menu (`filetypes.fortran`) and editor behavior (`geany.conf`).
3. Finally, it compiles and installs the `ce-fortran` toolbox (`toolbox.o`, `toolbox_debug.o`, `toolbox.mod`) to `/usr/local/include`.

**This is it!** If you saw no error messages appearing, then Fortran as well as our toolbox were successfully installed on your system.

#### Working with your chosen editor:
- **VS Code**: Open any `.f90` file and press `Cmd+Shift+B` to compile and build. Use **Terminal > Run Task...** to select **Fortran: Run Program**, or press `F5` to start debugging.
- **Geany**: Open any `.f90` file and use the **Build** menu (or shortcuts `F8` to compile, `F9` to build, and `F5` to execute).


## Uninstallation

In the unlikely event that you want to uninstall all the software components as well as the toolbox from your system, you can use the file `uninstall_fortran.command`. It follows the same logic as the installation bash file. You just have to follow the instructions.


## Toolbox Update

Sometimes you might feel that your toolbox is out of date. In this case, just pull the most recent version of this github repository and execute the file `update_toolbox.command`. This will compile the latest toolbox version and store is such that you can use it in all your programs.


## Run your first Fortran program

If you've successfully installed Fortran on you , [you can learn here how to run your first program](https://www.ce-fortran.com/run-program-mac/).
