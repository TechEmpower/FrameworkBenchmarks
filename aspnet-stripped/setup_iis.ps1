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

$source = "C:\FrameworkBenchmarks\aspnet-stripped\src"

# Stop
if (Get-WebSite -Name Benchmarks) { Remove-WebSite -Name Benchmarks }

if ($action -eq 'start') {
    # Because we don't use msbuild to compile the code, we do this all manually.
    
    Exec { & .\NuGet.exe install -o src\packages src\packages.config }

    if (-Not (Test-Path src\bin)) { New-Item -Path src\bin -ItemType directory | Out-Null }

    $dlls = Get-ChildItem -path src\packages -recurse -include *.dll
    foreach ($dll in $dlls) {
        Copy-Item $dll src\bin
    }

    # Create a website in IIS
    New-WebSite -Name Benchmarks -Port 8080 -PhysicalPath $source
}
