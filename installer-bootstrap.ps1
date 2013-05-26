param($noexit)

if (!$noexit) {
    Write-Host "`nRestarting PowerShell with -NoExit...`n"
    powershell -NoExit -File $MyInvocation.MyCommand.Path 1
    return
}

$basedir = "C:\FrameworkBenchmarks"
$workdir = $basedir + "\installs"

$repo = "https://github.com/TechEmpower/FrameworkBenchmarks"
$installer = $basedir + "\installer.ps1"

$git = "C:\Git\bin\git.exe"
$gitinstaller_file = "Git-1.8.1.2-preview20130201.exe"
$gitinstaller_url = "https://msysgit.googlecode.com/files/" + $gitinstaller_file
$gitinstaller_local = $workdir + "\" + $gitinstaller_file

Write-Host "Creating work directory: $workdir `n"
New-Item -Path $workdir -Type Directory -Force | Out-Null

Write-Host "Downloading git...`n"
(New-Object System.Net.WebClient).DownloadFile($gitinstaller_url, $gitinstaller_local)

Write-Host "Installing git...`n"
Start-Process $gitinstaller_local '/silent /dir="C:\Git"' -Wait
$env:Path += ";C:\Git\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

Write-Host "Removing git installation files...`n"
Remove-Item -Recurse -Force $basedir

Write-Host "Downloading FrameworkBenchmarks from git...`n"
&$git "clone" $repo $basedir | Out-Host

Write-Host "`nLaunching installer...`n"
Set-ExecutionPolicy -ExecutionPolicy Bypass -ErrorAction 'SilentlyContinue'
powershell -NoExit -File $installer
