param($action)

$wwwroot = "C:\FrameworkBenchmarks\aspnet-katana\www"
$source = "C:\FrameworkBenchmarks\aspnet-katana\src"
$msbuild = "C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"
$owinhost = "$source\packages\OwinHost.2.0.1\tools\OwinHost.exe"

if ($action -eq 'start') {
    # Create a website in IIS
    New-Item -Path $wwwroot -Type Directory -ErrorAction 'SilentlyContinue' | Out-Null
    New-WebSite -Name Benchmarks -Port 8080 -PhysicalPath $wwwroot
    
    # Build the project
    &$msbuild "$source\Benchmarks.Katana.csproj" /t:RestorePackages
    &$msbuild "$source\Benchmarks.Katana.csproj" /p:Configuration=Release /p:Platform=x64 /t:Clean
    
    # start owinhost.exe
    &$owinhost -p 8080
}
