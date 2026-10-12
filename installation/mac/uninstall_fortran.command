#!/bin/bash

# A SHELL SCRIPT FOR UNINSTALLING FORTRAN FROM MACOS
#
# This code is published under the GNU General Public License v3
#                         (https://www.gnu.org/licenses/gpl-3.0.en.html)
#
# Author: Fabian Kindermann (contact@ce-fortran.com)


# set the current directory as running directory
cd "$( cd "$( dirname "$0" )" && pwd )"


# ASK FOR UNINSTALLATION CONFIRMATION
echo
echo "This script uninstalls Fortran/GNU Plot and configured editor environments from your system."
echo 
read -rsp $'Do you want to continue (y/n)?' -n 1 key
echo

if [ "$key" != "y" ]; then
    exit 0
fi


# ASK WHICH EDITOR CONFIGURATION TO UNINSTALL
echo
echo "Which editor configuration would you like to remove?"
echo "  1) Visual Studio Code (removes Fortran tasks & debug settings)"
echo "  2) Geany (removes Geany application & configuration)"
echo "  3) Both"
echo "  4) Neither (keep editor settings, remove Fortran/Toolbox only)"
echo
read -rp $'Select an option [1-4, default: 1]: ' editor_uninstall_choice
if [ -z "$editor_uninstall_choice" ]; then
    editor_uninstall_choice=1
fi


# REMOVE THE TOOLBOX FILES
sudo rm -f /usr/local/include/toolbox.mod
sudo rm -f /usr/local/include/toolbox.o
sudo rm -f /usr/local/include/toolbox_debug.o
sudo rm -f /usr/local/include/toolbox_version.command


# REMOVE COMPILER AND GNUPLOT PACKAGES
if command -v brew >/dev/null 2>&1; then
    brew uninstall gcc gnuplot 2>/dev/null || true
fi


# UNINSTALL SELECTED EDITOR CONFIGURATIONS
if [ "$editor_uninstall_choice" = "1" ] || [ "$editor_uninstall_choice" = "3" ]; then
    # REMOVE THE FORTRAN CONFIGURATION FROM VSCODE (VSCODE ITSELF IS NOT REMOVED)
    python3 - <<'EOF'
import json, os, re

user_dir = os.path.expanduser("~/Library/Application Support/Code/User")

def load(path):
    text = open(path).read()
    try:
        return json.loads(text)
    except ValueError:
        text = re.sub(r'^\s*//.*$', '', text, flags=re.M)
        text = re.sub(r',(\s*[}\]])', r'\1', text)
        return json.loads(text)

def save(path, data):
    with open(path, "w") as f:
        json.dump(data, f, indent=4)
        f.write("\n")

try:
    path = os.path.join(user_dir, "tasks.json")
    if os.path.exists(path):
        data = load(path)
        data["tasks"] = [t for t in data.get("tasks", []) if not t.get("label", "").startswith("Fortran: ")]
        save(path, data)
        print("Removed Fortran tasks from VS Code.")

    path = os.path.join(user_dir, "settings.json")
    if os.path.exists(path):
        data = load(path)
        launch = data.get("launch", {})
        launch["configurations"] = [c for c in launch.get("configurations", []) if not c.get("name", "").startswith("Fortran: ")]
        data.pop("[fortran_free-form]", None)
        save(path, data)
        print("Removed Fortran settings from VS Code.")
except Exception as e:
    print("Could not parse your vscode settings - please remove the 'Fortran: ' entries manually: %s" % e)
EOF
fi

if [ "$editor_uninstall_choice" = "2" ] || [ "$editor_uninstall_choice" = "3" ]; then
    # REMOVE GEANY APPLICATION AND CONFIGURATION
    if command -v brew >/dev/null 2>&1; then
        brew uninstall --cask geany 2>/dev/null || true
    fi
    rm -rf /Applications/Geany.app/
    rm -rf ~/.config/geany/
    echo "Removed Geany application and configuration."
fi


# IF EVERYTHING RAN CORRECTLY, AT THIS POINT EVERYTHING SHOULD BE UNINSTALLED PROPERLY
echo  
echo ...UNINSTALLATION PROCESS COMPLETED.
echo 
echo
echo In case you encountered any problem, check on www.ce-fortran.com for help.
echo
echo
read -rsp $'Press RETURN to end...\n' -n 1 key