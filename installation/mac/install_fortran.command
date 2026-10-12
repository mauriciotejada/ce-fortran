#!/bin/bash

# A SHELL SCRIPT FOR INSTALLING FORTRAN ON MACOS
#
# This code is published under the GNU General Public License v3
#                         (https://www.gnu.org/licenses/gpl-3.0.en.html)
#
# Author: Fabian Kindermann (contact@ce-fortran.com)


# set the current directory as running directory
cd "$( cd "$( dirname "$0" )" && pwd )"


# ASK FOR INSTALLATION CONFIRMATION
echo
echo This script installs Fortran to your system.
echo
read -rsp $'Do you want to continue (y/n)?' -n 1 key
echo

if [ "$key" != "y" ]; then
    exit 0
fi


# INSTALL HOMEBREW WHICH WILL GOVERN THE INSTALLATION OF EVERTHING ELSE (ONLY IF IT IS NOT ALREADY THERE)

# brew is often not in the PATH (e.g. /opt/homebrew on Apple Silicon), so look in the default places
if ! command -v brew >/dev/null 2>&1; then
    for brew_path in /opt/homebrew/bin/brew /usr/local/bin/brew; do
        if [ -x "$brew_path" ]; then
            eval "$("$brew_path" shellenv)"
            break
        fi
    done
fi

if command -v brew >/dev/null 2>&1; then
    echo
    echo Homebrew is already installed - skipping installation.
else
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    # make brew available in this session after a fresh installation
    for brew_path in /opt/homebrew/bin/brew /usr/local/bin/brew; do
        if [ -x "$brew_path" ]; then
            eval "$("$brew_path" shellenv)"
            break
        fi
    done
fi

# INSTALL GNU GFORTRAN COMPILER
brew install gcc

# INSTALL GNUPLOT
brew install gnuplot

# ASK FOR EDITOR SELECTION
echo
echo "Which editor/IDE would you like to install and configure for Fortran?"
echo "  1) Visual Studio Code (VS Code)"
echo "  2) Geany"
echo
read -rp $'Select an editor [1 or 2, default: 1]: ' editor_choice
if [ -z "$editor_choice" ]; then
    editor_choice=1
fi

if [ "$editor_choice" = "2" ]; then
    # INSTALL GEANY TEXT EDITOR
    brew install --cask geany

    # PATCH GEANY FOR USE WITH FORTRAN

    # Fortran compilation file
    echo '[build-menu]' >  "filetypes.fortran"
    echo 'FT_00_LB=_Compile' >> "filetypes.fortran"
    echo 'FT_00_CM=mkdir -p Build && gfortran -O3 -fopenmp -frecursive -ffree-line-length-none -Wno-unused -fimplicit-none -Wall -fcheck=bound,do -ffpe-trap=invalid,zero,overflow -I/usr/local/include/ -J"./Build" -c "%f" -o "Build/%e.o"' >> "filetypes.fortran"
    echo 'FT_00_WD=' >> "filetypes.fortran"
    echo 'FT_01_LB=_Build' >> "filetypes.fortran"
    echo 'FT_01_CM=gfortran -O3 -fopenmp -J"./Build" /usr/local/include/toolbox.o "Build/%e.o" -o Build/prog' >> "filetypes.fortran"
    echo 'FT_01_WD=' >> "filetypes.fortran"
    echo 'FT_02_LB=_Debug' >> "filetypes.fortran"
    echo 'FT_02_CM=mkdir -p Build && gfortran -fopenmp -frecursive -ffree-line-length-none -Wno-unused -fimplicit-none -Wall -fcheck=bound,do -ffpe-trap=invalid,zero,overflow -I/usr/local/include/ -J"./Build" -g "%f" -o "Build/%e.o" "/usr/local/include/toolbox_debug.o"' >> "filetypes.fortran"
    echo 'FT_02_WD=' >> "filetypes.fortran"
    echo 'EX_00_LB=_Run Program' >> "filetypes.fortran"
    echo 'EX_00_CM=clear && "Build/prog"' >> "filetypes.fortran"
    echo 'EX_00_WD=' >> "filetypes.fortran"
    echo 'EX_01_LB=_Run Debugger'>> "filetypes.fortran"
    echo 'EX_01_CM=clear && lldb "Build/%e.o"' >> "filetypes.fortran"
    echo 'EX_01_WD=' >> "filetypes.fortran"

    # geany editor behavior
    echo '[geany]' >  "geany.conf"
    echo 'indent_type=0' >>  "geany.conf"
    echo 'pref_main_load_session=false' >>  "geany.conf"
    echo 'beep_on_errors=false' >>  "geany.conf"
    echo ' ' >>  "geany.conf"
    echo '[tools]' >>  "geany.conf"
    echo 'shell_cmd=/bin/zsh' >>  "geany.conf"

    # update geany configuration
    mkdir -p ~/.config/geany/filedefs/
    mv ./filetypes.fortran ~/.config/geany/filedefs/filetypes.fortran
    mv ./geany.conf ~/.config/geany/geany.conf
else
    # INSTALL VISUAL STUDIO CODE (ONLY IF IT IS NOT ALREADY THERE)
VSCODE_APP="/Applications/Visual Studio Code.app"
VSCODE_APP_CLI="$VSCODE_APP/Contents/Resources/app/bin/code"

if command -v code >/dev/null 2>&1 || [ -d "$VSCODE_APP" ]; then
    echo
    echo Visual Studio Code is already installed - skipping installation.
else
    brew install --cask visual-studio-code
fi

# find the command line interface of vscode (it is often not in the PATH)
if command -v code >/dev/null 2>&1; then
    VSCODE_CLI="$(command -v code)"
elif [ -x "$VSCODE_APP_CLI" ]; then
    VSCODE_CLI="$VSCODE_APP_CLI"
else
    VSCODE_CLI=""
fi

# install the vscode extensions used for Fortran (skip those already installed)
if [ -n "$VSCODE_CLI" ]; then
    INSTALLED_EXTENSIONS="$("$VSCODE_CLI" --list-extensions 2>/dev/null)"
    for ext in fortran-lang.linter-gfortran vadimcn.vscode-lldb; do
        if echo "$INSTALLED_EXTENSIONS" | grep -qix "$ext"; then
            echo Extension $ext is already installed - skipping.
        else
            "$VSCODE_CLI" --install-extension "$ext"
        fi
    done
else
    echo
    echo WARNING: could not find the vscode command line tool. Please install the
    echo extensions fortran-lang.linter-gfortran and vadimcn.vscode-lldb manually.
fi


# CONFIGURE VSCODE FOR USE WITH FORTRAN

VSCODE_USER_DIR="$HOME/Library/Application Support/Code/User"
mkdir -p "$VSCODE_USER_DIR"
CE_TMP="$(mktemp -d)"

# Fortran tasks (compile, build, debug, run) - same commands as we used to give geany
cat > "$CE_TMP/tasks.json" <<'EOF'
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Fortran: Compile",
            "type": "shell",
            "command": "mkdir -p Build && gfortran -O3 -fopenmp -frecursive -ffree-line-length-none -Wno-unused -fimplicit-none -Wall -fcheck=bound,do -ffpe-trap=invalid,zero,overflow -I/usr/local/include/ -J\"./Build\" -c \"${file}\" -o \"Build/${fileBasenameNoExtension}.o\"",
            "options": { "cwd": "${fileDirname}" },
            "problemMatcher": "$gcc",
            "presentation": { "reveal": "always", "clear": true }
        },
        {
            "label": "Fortran: Link",
            "type": "shell",
            "command": "gfortran -O3 -fopenmp -J\"./Build\" /usr/local/include/toolbox.o \"Build/${fileBasenameNoExtension}.o\" -o Build/prog",
            "options": { "cwd": "${fileDirname}" },
            "problemMatcher": "$gcc",
            "presentation": { "reveal": "always" }
        },
        {
            "label": "Fortran: Build",
            "dependsOn": ["Fortran: Compile", "Fortran: Link"],
            "dependsOrder": "sequence",
            "group": { "kind": "build", "isDefault": true },
            "problemMatcher": []
        },
        {
            "label": "Fortran: Debug Build",
            "type": "shell",
            "command": "mkdir -p Build && gfortran -fopenmp -frecursive -ffree-line-length-none -Wno-unused -fimplicit-none -Wall -fcheck=bound,do -ffpe-trap=invalid,zero,overflow -I/usr/local/include/ -J\"./Build\" -g \"${file}\" -o \"Build/${fileBasenameNoExtension}.o\" \"/usr/local/include/toolbox_debug.o\"",
            "options": { "cwd": "${fileDirname}" },
            "problemMatcher": "$gcc",
            "presentation": { "reveal": "always", "clear": true }
        },
        {
            "label": "Fortran: Run Program",
            "type": "shell",
            "command": "clear && \"Build/prog\"",
            "options": { "cwd": "${fileDirname}" },
            "group": { "kind": "test", "isDefault": true },
            "problemMatcher": [],
            "presentation": { "reveal": "always", "focus": true }
        }
    ]
}
EOF

# settings: editor behavior and debugger launch configuration
cat > "$CE_TMP/settings.json" <<'EOF'
{
    "[fortran_free-form]": {
        "editor.insertSpaces": true
    },
    "terminal.integrated.defaultProfile.osx": "zsh",
    "fortran.linter.includePaths": ["/usr/local/include"],
    "fortran.linter.extraArgs": ["-fopenmp", "-ffree-line-length-none", "-fimplicit-none"],
    "launch": {
        "version": "0.2.0",
        "configurations": [
            {
                "name": "Fortran: Debug (lldb)",
                "type": "lldb",
                "request": "launch",
                "program": "${fileDirname}/Build/${fileBasenameNoExtension}.o",
                "cwd": "${fileDirname}",
                "preLaunchTask": "Fortran: Debug Build"
            }
        ]
    }
}
EOF

# Merge our configuration into existing user files (never overwrite them).
# Existing files are backed up first. If a file can not be parsed (e.g. it
# contains comments), we leave it untouched and store ours next to it.
python3 - "$CE_TMP" "$VSCODE_USER_DIR" <<'EOF'
import json, os, re, shutil, sys

tmp, user_dir = sys.argv[1], sys.argv[2]

def load(path):
    with open(path) as f:
        text = f.read()
    if not text.strip():
        return {}
    try:
        return json.loads(text)
    except ValueError:
        # tolerate comments and trailing commas (vscode uses JSONC)
        text = re.sub(r'^\s*//.*$', '', text, flags=re.M)
        text = re.sub(r',(\s*[}\]])', r'\1', text)
        return json.loads(text)

def merge_by_key(old, new, key):
    names = {item[key] for item in new}
    return [item for item in old if item.get(key) not in names] + new

for name in ("tasks.json", "settings.json"):
    target = os.path.join(user_dir, name)
    ours = load(os.path.join(tmp, name))

    if not os.path.exists(target):
        merged = ours
    else:
        try:
            current = load(target)
        except ValueError:
            side = os.path.join(user_dir, "ce-fortran-" + name)
            shutil.copy(os.path.join(tmp, name), side)
            print("WARNING: could not parse %s - left untouched." % target)
            print("         Our configuration was saved to %s" % side)
            continue
        shutil.copy(target, target + ".bak")
        merged = current
        if name == "tasks.json":
            merged.setdefault("version", ours["version"])
            merged["tasks"] = merge_by_key(merged.get("tasks", []), ours["tasks"], "label")
        else:
            launch = merged.setdefault("launch", {"version": "0.2.0", "configurations": []})
            launch["configurations"] = merge_by_key(
                launch.get("configurations", []), ours["launch"]["configurations"], "name")
            for key, value in ours.items():
                if key != "launch":
                    if isinstance(value, dict) and isinstance(merged.get(key), dict):
                        merged[key].update(value)
                    else:
                        merged[key] = value

    with open(target, "w") as f:
        json.dump(merged, f, indent=4)
        f.write("\n")
    print("Updated %s" % target)
EOF

rm -rf "$CE_TMP"
fi


## INSTALL THE TOOLBOX

# compile the toolbox
gfortran -c -Werror -fopenmp -Wno-unused -ffree-line-length-none -fimplicit-none -Wall -fcheck=bound,do -ffpe-trap=invalid,zero,overflow -frecursive -g ./../toolbox/toolbox.f90 -o toolbox_debug.o
gfortran -c -O3 -fopenmp -ffree-line-length-none ./../toolbox/toolbox.f90 -o toolbox.o

# copy the toolbox to the working directory
sudo mkdir -p /usr/local/include
sudo mv toolbox.mod /usr/local/include/
sudo mv toolbox.o /usr/local/include/
sudo mv toolbox_debug.o /usr/local/include/


# IF EVERYTHING RAN CORRECTLY, AT THIS POINT EVERYTHING SHOULD BE INSTALLED PROPERLY
echo
echo ...INSTALLATION COMPLETED.
echo
if [ "$editor_choice" = "2" ]; then
    echo "You can now open Geany to start writing and running Fortran programs."
    echo "Use the Build menu (or F8 / F9 / F5) to compile, build, and run your code."
else
    echo "In VS Code, open a .f90 file and press Cmd+Shift+B to compile and build it."
    echo "Use \"Terminal > Run Task...\" for \"Fortran: Run Program\" and \"Fortran: Debug Build\","
    echo "or press F5 to start the debugger."
fi
echo
echo In case you encountered any problem, check on www.ce-fortran.com for help.
echo
echo
read -rsp $'Press RETURN to end...\n' -n 1 key
