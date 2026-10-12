# cleanup_vscode.ps1 - Remove Fortran configurations from VS Code on Windows
$ErrorActionPreference = "Continue"

$userDir = Join-Path $env:APPDATA "Code\User"
if (-not (Test-Path $userDir)) {
    exit 0
}

function Load-JsonSafe($filePath) {
    if (-not (Test-Path $filePath)) { return $null }
    $text = Get-Content -Path $filePath -Raw -Encoding UTF8
    if ([string]::IsNullOrWhiteSpace($text)) { return $null }
    try {
        return ($text | ConvertFrom-Json)
    } catch {
        $cleaned = $text -replace '(?m)^\s*//.*$', '' -replace ',(\s*[}\]])', '$1'
        try {
            return ($cleaned | ConvertFrom-Json)
        } catch {
            return $null
        }
    }
}

# 1. Clean tasks.json
$tasksPath = Join-Path $userDir "tasks.json"
$tasksObj = Load-JsonSafe $tasksPath
if ($null -ne $tasksObj -and $null -ne $tasksObj.tasks) {
    $tasksObj.tasks = @($tasksObj.tasks | Where-Object { $_.label -notlike "Fortran: *" })
    $tasksObj | ConvertTo-Json -Depth 10 | Set-Content -Path $tasksPath -Encoding UTF8
    Write-Host "Removed Fortran tasks from VS Code."
}

# 2. Clean settings.json
$settingsPath = Join-Path $userDir "settings.json"
$settingsObj = Load-JsonSafe $settingsPath
if ($null -ne $settingsObj) {
    if ($null -ne $settingsObj.launch -and $null -ne $settingsObj.launch.configurations) {
        $settingsObj.launch.configurations = @($settingsObj.launch.configurations | Where-Object { $_.name -notlike "Fortran: *" })
    }
    if ($settingsObj.PSObject.Properties['[fortran_free-form]']) {
        $settingsObj.PSObject.Properties.Remove('[fortran_free-form]')
    }
    $settingsObj | ConvertTo-Json -Depth 10 | Set-Content -Path $settingsPath -Encoding UTF8
    Write-Host "Removed Fortran settings from VS Code."
}
