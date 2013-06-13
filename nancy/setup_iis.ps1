param($action)

$wwwroot = "C:\FrameworkBenchmarks\nancy\www"
$source = "C:\FrameworkBenchmarks\nancy\src"

# Stop
if (Get-WebSite -Name Benchmarks) { Remove-WebSite -Name Benchmarks }
Get-ChildItem -Path $wwwroot -Recurse -ErrorAction 'SilentlyContinue' | Remove-Item -Force -Recurse -ErrorAction 'SilentlyContinue'; 
Remove-Item -Force -Recurse $wwwroot -ErrorAction 'SilentlyContinue'

if ($action -eq 'start') {
    # Create a website in IIS
    New-Item -Path $wwwroot -Type directory | Out-Null
    New-WebSite -Name Benchmarks -Port 8080 -PhysicalPath $wwwroot
    
    # Build the project
    C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe "$source\NancyBenchmark.csproj" /p:Configuration=Release /p:Platform="x64" /t:Clean
    C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe "$source\NancyBenchmark.csproj" /p:Configuration=Release /p:Platform="x64" /p:DeployOnBuild=true /p:PublishProfile=IIS
}
