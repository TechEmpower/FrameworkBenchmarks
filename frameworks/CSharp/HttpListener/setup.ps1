param($action)

$ErrorActionPreference = 'Stop'

# From http://zduck.com/2012/powershell-batch-files-exit-codes/
function Exec
{
    [CmdletBinding()]
    param (
        [Parameter(Position=0, Mandatory=1)]
        [scriptblock]$Command,
        [Parameter(Position=1, Mandatory=0)]
        [string]$ErrorMessage = "Execution of command failed.`n$Command"
    )
    & $Command
    if ($LastExitCode -ne 0) {
        throw "Exec: $ErrorMessage"
    }
}

$root = "C:\FrameworkBenchmarks\HttpListener"
$msbuild = "C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"

# Stop
Get-Process | Where-Object { $_.Name -ieq "HttpListener" } | Stop-Process

if ($action -eq 'start') {
    # Build the project
    Exec { & $msbuild "$root\HttpListener.sln" /p:DownloadNuGetExe=true /p:RequireRestoreConsent=false /p:Configuration=Release /t:Rebuild }
    
    Start-Process "$root\HttpListener\bin\Release\HttpListener.exe"
}