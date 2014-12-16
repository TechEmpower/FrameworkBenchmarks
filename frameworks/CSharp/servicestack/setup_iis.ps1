param($action)

$wwwroot = "C:\FrameworkBenchmarks\servicestack\www"
$source = "C:\FrameworkBenchmarks\servicestack\src"
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
    &$msbuild "$source\ServiceStackBenchmark.csproj" /t:RestorePackages
    &$msbuild "$source\ServiceStackBenchmark.csproj" /p:Configuration=Release /p:Platform="x64" /t:Clean
    &$msbuild "$source\ServiceStackBenchmark.csproj" /p:Configuration=Release /p:Platform="x64" /p:DeployOnBuild=true /p:PublishProfile=IIS
}

