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

$source = "C:\FrameworkBenchmarks\servicestack\src"
$msbuild = "C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"

# Stop
Get-Process | Where-Object { $_.Name -ieq "servicestack" } | Stop-Process

if ($action -eq 'start') {
    # Build the project
    Exec { & $msbuild "$source\ServiceStackBenchmark.sln" /p:DownloadNuGetExe=true /p:RequireRestoreConsent=false /p:Configuration=Release /p:Platform="x64" /t:Rebuild }
        
    Start-Process "$source\SelfHost\bin\Release\ServiceStackBenchmark.SelfHost.exe http://*:8080"
}
