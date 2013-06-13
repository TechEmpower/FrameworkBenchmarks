param($action)

$source = "C:\FrameworkBenchmarks\aspnet-stripped\src"

# Stop
if (Get-WebSite -Name Benchmarks) { Remove-WebSite -Name Benchmarks }

if ($action -eq 'start') {
    # Create a website in IIS
    New-WebSite -Name Benchmarks -Port 8080 -PhysicalPath $source
}
