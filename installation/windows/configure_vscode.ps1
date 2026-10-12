# configure_vscode.ps1 - Configure VS Code for Fortran on Windows
param(
    [string]$Location = "C:\cygwin"
)

$ErrorActionPreference = "Continue"

$userDir = Join-Path $env:APPDATA "Code\User"
if (-not (Test-Path $userDir)) {
    New-Item -ItemType Directory -Path $userDir -Force | Out-Null
}

# Normalize location for posix/gcc flags
$incPath = ($Location.Replace('\', '/') + "/include")

# Helper to load JSON safely tolerating comments and trailing commas
function Load-JsonSafe($filePath) {
    if (-not (Test-Path $filePath)) { return $null }
    $text = Get-Content -Path $filePath -Raw -Encoding UTF8
    if ([string]::IsNullOrWhiteSpace($text)) { return $null }
    try {
        return ($text | ConvertFrom-Json)
    } catch {
        # Strip single line comments and trailing commas
        $cleaned = $text -replace '(?m)^\s*//.*$', '' -replace ',(\s*[}\]])', '$1'
        try {
            return ($cleaned | ConvertFrom-Json)
        } catch {
            return "PARSE_ERROR"
        }
    }
}

# Tasks definition
$newTasks = @(
    [PSCustomObject]@{
        label = "Fortran: Compile"
        type = "shell"
        command = "cmd /c ""if not exist Build mkdir Build"" && gfortran -O3 -fopenmp -frecursive -ffree-line-length-none -Wno-unused -fimplicit-none -Wall -fcheck=bound,do -ffpe-trap=invalid,zero,overflow -I$incPath -JBuild -c ""${file}"" -o ""Build/${fileBasenameNoExtension}.o"""
        options = [PSCustomObject]@{ cwd = "${fileDirname}" }
        problemMatcher = "$gcc"
        presentation = [PSCustomObject]@{ reveal = "always"; clear = $true }
    },
    [PSCustomObject]@{
        label = "Fortran: Link"
        type = "shell"
        command = "gfortran -O3 -fopenmp -JBuild $incPath/toolbox.o ""Build/${fileBasenameNoExtension}.o"" -o Build/prog.exe"
        options = [PSCustomObject]@{ cwd = "${fileDirname}" }
        problemMatcher = "$gcc"
        presentation = [PSCustomObject]@{ reveal = "always" }
    },
    [PSCustomObject]@{
        label = "Fortran: Build"
        dependsOn = @("Fortran: Compile", "Fortran: Link")
        dependsOrder = "sequence"
        group = [PSCustomObject]@{ kind = "build"; isDefault = $true }
        problemMatcher = @()
    },
    [PSCustomObject]@{
        label = "Fortran: Debug Build"
        type = "shell"
        command = "cmd /c ""if not exist Build mkdir Build"" && gfortran -fopenmp -frecursive -ffree-line-length-none -Wno-unused -fimplicit-none -Wall -fcheck=bound,do -ffpe-trap=invalid,zero,overflow -I$incPath -JBuild -g ""${file}"" -o ""Build/${fileBasenameNoExtension}.o"" $incPath/toolbox_debug.o"
        options = [PSCustomObject]@{ cwd = "${fileDirname}" }
        problemMatcher = "$gcc"
        presentation = [PSCustomObject]@{ reveal = "always"; clear = $true }
    },
    [PSCustomObject]@{
        label = "Fortran: Run Program"
        type = "shell"
        command = "cls && ""Build/prog.exe"""
        options = [PSCustomObject]@{ cwd = "${fileDirname}" }
        group = [PSCustomObject]@{ kind = "test"; isDefault = $true }
        problemMatcher = @()
        presentation = [PSCustomObject]@{ reveal = "always"; focus = $true }
    }
)

# 1. Update tasks.json
$tasksPath = Join-Path $userDir "tasks.json"
$existingTasks = Load-JsonSafe $tasksPath

if ($existingTasks -eq "PARSE_ERROR") {
    $side = Join-Path $userDir "ce-fortran-tasks.json"
    [PSCustomObject]@{ version = "2.0.0"; tasks = $newTasks } | ConvertTo-Json -Depth 10 | Set-Content -Path $side -Encoding UTF8
    Write-Host "WARNING: could not parse $tasksPath - saved our tasks to $side"
} else {
    if (Test-Path $tasksPath) {
        Copy-Item $tasksPath "$tasksPath.bak" -Force
    }
    if ($null -eq $existingTasks) {
        $finalTasksObj = [PSCustomObject]@{
            version = "2.0.0"
            tasks = $newTasks
        }
    } else {
        $finalTasksObj = $existingTasks
        if (-not $finalTasksObj.version) {
            $finalTasksObj | Add-Member -NotePropertyName version -NotePropertyValue "2.0.0" -Force
        }
        $otherTasks = @()
        if ($finalTasksObj.tasks) {
            $otherTasks = @($finalTasksObj.tasks | Where-Object { $_.label -notlike "Fortran: *" })
        }
        $finalTasksObj.tasks = @($otherTasks + $newTasks)
    }
    $finalTasksObj | ConvertTo-Json -Depth 10 | Set-Content -Path $tasksPath -Encoding UTF8
    Write-Host "Updated $tasksPath"
}

# 2. Update settings.json
$settingsPath = Join-Path $userDir "settings.json"
$existingSettings = Load-JsonSafe $settingsPath

$launchConfig = [PSCustomObject]@{
    name = "Fortran: Debug (lldb)"
    type = "lldb"
    request = "launch"
    program = "${fileDirname}/Build/${fileBasenameNoExtension}.o"
    cwd = "${fileDirname}"
    preLaunchTask = "Fortran: Debug Build"
}

if ($existingSettings -eq "PARSE_ERROR") {
    $side = Join-Path $userDir "ce-fortran-settings.json"
    [PSCustomObject]@{
        "[fortran_free-form]" = [PSCustomObject]@{ "editor.insertSpaces" = $true }
        "fortran.linter.includePaths" = @($incPath)
        "fortran.linter.extraArgs" = @("-fopenmp", "-ffree-line-length-none", "-fimplicit-none")
        "launch" = [PSCustomObject]@{
            version = "0.2.0"
            configurations = @($launchConfig)
        }
    } | ConvertTo-Json -Depth 10 | Set-Content -Path $side -Encoding UTF8
    Write-Host "WARNING: could not parse $settingsPath - saved our settings to $side"
} else {
    if (Test-Path $settingsPath) {
        Copy-Item $settingsPath "$settingsPath.bak" -Force
    }
    if ($null -eq $existingSettings) {
        $finalSettingsObj = [PSCustomObject]@{
            "[fortran_free-form]" = [PSCustomObject]@{ "editor.insertSpaces" = $true }
            "fortran.linter.includePaths" = @($incPath)
            "fortran.linter.extraArgs" = @("-fopenmp", "-ffree-line-length-none", "-fimplicit-none")
            "launch" = [PSCustomObject]@{
                version = "0.2.0"
                configurations = @($launchConfig)
            }
        }
    } else {
        $finalSettingsObj = $existingSettings
        $finalSettingsObj | Add-Member -NotePropertyName "[fortran_free-form]" -NotePropertyValue ([PSCustomObject]@{ "editor.insertSpaces" = $true }) -Force
        $finalSettingsObj | Add-Member -NotePropertyName "fortran.linter.includePaths" -NotePropertyValue @($incPath) -Force
        $finalSettingsObj | Add-Member -NotePropertyName "fortran.linter.extraArgs" -NotePropertyValue @("-fopenmp", "-ffree-line-length-none", "-fimplicit-none") -Force

        if (-not $finalSettingsObj.launch) {
            $finalSettingsObj | Add-Member -NotePropertyName "launch" -NotePropertyValue ([PSCustomObject]@{ version = "0.2.0"; configurations = @($launchConfig) }) -Force
        } else {
            $otherConfigs = @()
            if ($finalSettingsObj.launch.configurations) {
                $otherConfigs = @($finalSettingsObj.launch.configurations | Where-Object { $_.name -notlike "Fortran: *" })
            }
            $finalSettingsObj.launch.configurations = @($otherConfigs + $launchConfig)
        }
    }
    $finalSettingsObj | ConvertTo-Json -Depth 10 | Set-Content -Path $settingsPath -Encoding UTF8
    Write-Host "Updated $settingsPath"
}
