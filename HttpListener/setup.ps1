param($action)

$root = "C:\FrameworkBenchmarks\HttpListener"
$msbuild = "C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"

# Stop
Stop-Process -Name HttpListener -ErrorAction SilentlyContinue

if ($action -eq 'start') {
    # Build the project
    &$msbuild "$root\HttpListener.sln" /p:DownloadNuGetExe=true /p:RequireRestoreConsent=false /p:Configuration=Release /t:Rebuild
    
    Start-Process "$root\HttpListener\bin\Release\HttpListener.exe"
}