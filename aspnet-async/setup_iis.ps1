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

$wwwroot = "C:\FrameworkBenchmarks\aspnet-async\www"
$source = "C:\FrameworkBenchmarks\aspnet-async\src"
$msbuild = "C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"

# Stop
if (Get-WebSite -Name Benchmarks) { Remove-WebSite -Name Benchmarks }
Get-ChildItem -Path $wwwroot -Recurse -ErrorAction 'SilentlyContinue' | Remove-Item -Force -Recurse -ErrorAction 'SilentlyContinue' | Out-Null 
Remove-Item -Force -Recurse $wwwroot -ErrorAction 'SilentlyContinue' | Out-Null

if (Test-Path IIS:\AppPools\BenchmarksAppPool) { Remove-Item IIS:\AppPools\BenchmarksAppPool -Force -Recurse }

if ($action -eq 'start') {
	# Create and start application pool
	$appPool = New-Item IIS:\AppPools\BenchmarksAppPool
	$appPool.managedRuntimeVersion = "4.0"
	$appPool.managedPipelineMode = 0 # Integrated
	$appPool.queueLength = 9000 # max value = 9000, default = 1000
	$appPool | Set-Item
	Start-WebAppPool -Name BenchmarksAppPool

    # Create a website in IIS
    New-Item -Path $wwwroot -Type Directory -ErrorAction 'SilentlyContinue' | Out-Null
    New-WebSite -Name Benchmarks -Port 8080 -PhysicalPath $wwwroot -ApplicationPool BenchmarksAppPool
	
    # Build the project
    Exec { & $msbuild "$source\AspNetAsyncBenchmark\AspNetAsyncBenchmark.csproj" /t:RestorePackages }
    Exec { & $msbuild "$source\AspNetAsyncBenchmark\AspNetAsyncBenchmark.csproj" /p:Configuration=Release /t:Clean }
    Exec { & $msbuild "$source\AspNetAsyncBenchmark\AspNetAsyncBenchmark.csproj" /p:Configuration=Release /p:DeployOnBuild=true /p:PublishProfile=prod }
}
