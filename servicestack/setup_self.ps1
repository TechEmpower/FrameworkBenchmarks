param($action)

$source = "C:\FrameworkBenchmarks\servicestack\src"
$msbuild = "C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"

# Stop
Stop-Process -Name servicestack -ErrorAction SilentlyContinue

if ($action -eq 'start') {
    # Build the project
    &$msbuild "$source\ServiceStackBenchmark.sln" /p:DownloadNuGetExe=true /p:RequireRestoreConsent=false /p:Configuration=Release /p:Platform="x64" /t:Rebuild
        
    Start-Process "$source\SelfHost\bin\Release\ServiceStackBenchmark.SelfHost.exe http://*:8080"
}
