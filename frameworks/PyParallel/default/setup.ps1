param($action)

$proc = Get-WmiObject Win32_Process -Filter "ExecutablePath like '%PyParallel33\\python.exe%'"
if ($proc) {
    $proc.Terminate()
}

if ($action -eq 'start') {
    Start-Process "cmd.exe" "/c tefb.bat js"
}
