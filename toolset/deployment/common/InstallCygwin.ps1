#
# PowerShell script to Install Cygwin on Windows (64 bits)
#
# Instructions: Run this script as administrator.
#

Write-Host "Installing Cygwin..."

$cygwin_installer_dir = $env:TEMP + "\cygwin-installer"
Write-Host "Creating temporary directory at $cygwin_installer_dir"
New-Item -Path $cygwin_installer_dir -Type directory -Force | Out-Null

$cygwin_installer_file = "setup-x86_64.exe"
$cygwin_installer_url = "http://cygwin.com/$cygwin_installer_file"
$cygwin_installer_local = "$cygwin_installer_dir\$cygwin_installer_file"
Write-Host "Downloading setup file to $cygwin_installer_local"
(New-Object System.Net.WebClient).DownloadFile($cygwin_installer_url, $cygwin_installer_local)

$cygwin_install_dir = "C:\Cygwin"
Write-Host "Installing Cygwin at $cygwin_install_dir"
Write-Host "Installation log will be saved at $cygwin_installer_dir\install.log"
Start-Process $cygwin_installer_local "-q -n -l $cygwin_installer_dir -s http://mirrors.kernel.org/sourceware/cygwin/ -R $cygwin_install_dir -P openssh,openssl,ncurses" -WorkingDirectory "$cygwin_installer_dir" -Wait -RedirectStandardOutput $cygwin_installer_dir\install.log

Write-Host "Adding to PATH $cygwin_install_dir;$cygwin_install_dir\bin"
$env:Path += ";$cygwin_install_dir;$cygwin_install_dir\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

Write-Host "Verifying installation"
Invoke-Expression -Command "C:\Cygwin\bin\echo.exe Done"
