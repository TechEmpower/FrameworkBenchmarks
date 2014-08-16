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

$wwwroot = "C:\FrameworkBenchmarks\aspnet\www"
$source = "C:\FrameworkBenchmarks\aspnet\src"
$msbuild = "C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"

# Stop
if (Get-WebSite -Name Benchmarks) { Remove-WebSite -Name Benchmarks }
Get-ChildItem -Path $wwwroot -Recurse -ErrorAction 'SilentlyContinue' | Remove-Item -Force -Recurse -ErrorAction 'SilentlyContinue' | Out-Null 
Remove-Item -Force -Recurse $wwwroot -ErrorAction 'SilentlyContinue' | Out-Null

if ($action -eq 'start') {
    # Create a website in IIS
    New-Item -Path $wwwroot -Type Directory -ErrorAction 'SilentlyContinue' | Out-Null
    New-WebSite -Name Benchmarks -Port 8080 -PhysicalPath $wwwroot
    
    # Build the project
    Exec { & $msbuild "$source\Benchmarks.AspNet.csproj" /t:RestorePackages }
    Exec { & $msbuild "$source\Benchmarks.AspNet.csproj" /p:Configuration=Release /p:Platform=x64 /t:Clean }
    Exec { & $msbuild "$source\Benchmarks.AspNet.csproj" /p:Configuration=Release /p:Platform=x64 /p:DeployOnBuild=true /p:PublishProfile=IIS }
}
