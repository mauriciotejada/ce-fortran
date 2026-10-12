# Ubuntu Installation Package

The Ubuntu installation package provides three command files that serve different purposes:

1. An installation file `install_fortran.sh` to install Fortran on your computer.
2. A file `uninstall_fortran.sh` that completely removes the Fortran installation from your computer.
3. A file `update_toolbox.sh` that compiles and copie the latest version of our toolbox to your system.

All installation files have been tested with Ubuntu 20.04, but should work on previous and later versions.


## Installation

When you execute the file `install_fortran.sh`, it will install the [GNU Fortran Compiler](https://gcc.gnu.org/fortran/) environment, the plotting program [gnuplot](http://gnuplot.info/), and prompt you to choose your preferred development editor: **Visual Studio Code** or **Geany**. In addition, it will compile and store the toolbox on your computer.


#### The installation process in detail

The file `install_fortran.sh` is a bash file used to execute the installation routines on your computer. You can run this file in a terminal (you might need to add executable rights by typing `chmod +x install_fortran.sh`). During the installation process, changes are made to lower system files requiring administrator rights (`sudo`). When prompted, confirm installation by typing `y` and pressing ENTER.

You will then be prompted to select your editor of choice:
1. **Visual Studio Code (VS Code)** (default)
2. **Geany**

In detail, you will see the following happening:

1. The installer installs the GNU Fortran compiler (`gfortran`), GNU debugger (`gdb`), build tools, and [gnuplot](http://gnuplot.info/).
2. Depending on your editor selection:
   - **Visual Studio Code**: The installer installs VS Code (via official deb package if not already present), installs the Fortran linter (`fortran-lang.linter-gfortran`) and LLDB debugger (`vadimcn.vscode-lldb`) extensions, and merges Fortran compile, build, run, and debug tasks into `~/.config/Code/User/tasks.json` and `settings.json` (backing up any existing files to `.bak`).
   - **Geany**: The installer installs Geany via apt and configures its build menu (`filetypes.fortran`) and editor behavior (`geany.conf`).
3. Finally, it compiles and installs the `ce-fortran` toolbox (`toolbox.o`, `toolbox_debug.o`, `toolbox.mod`) to `/usr/local/include`.

**This is it!** If you saw no error messages appearing, then Fortran as well as our toolbox were successfully installed on your system.

#### Working with your chosen editor:
- **VS Code**: Open any `.f90` file and press `Ctrl+Shift+B` to compile and build. Use **Terminal > Run Task...** to select **Fortran: Run Program**, or press `F5` to start debugging.
- **Geany**: Open any `.f90` file and use the **Build** menu (or shortcuts `F8` to compile, `F9` to build, and `F5` to execute).


## Uninstallation

In the unlikely event that you want to uninstall all the software components as well as the toolbox from your system, you can use the file `uninstall_fortran.sh`. It follows the same logic as the installation bash file. You just have to follow the instructions.


## Toolbox Update

Sometimes you might feel that your toolbox is out of date. In this case, just pull the most recent version of this github repository and execute the file `update_toolbox.sh`. This will compile the latest toolbox version and store is such that you can use it in all your programs.


## Run your first Fortran program

If you've successfully installed Fortran on you , [you can learn here how to run your first program](https://www.ce-fortran.com/run-program-lin/).
