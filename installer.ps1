$basedir = "C:\FrameworkBenchmarks"
$workdir = $basedir + "\installs"
New-Item -Path $workdir -Type directory -Force | Out-Null

#
# ASP.NET
#
Write-Host "Installing IIS, .NET and ASP.NET..."

# Enable Windows Update to get rid of the yellow warnings
# But this is not strictly neccessary
$Updates = (New-Object -ComObject "Microsoft.Update.AutoUpdate").Settings
$Updates.NotificationLevel = 2 # Notify before download
$Updates.Save()
$Updates.Refresh()

Install-WindowsFeature Web-Server
Install-WindowsFeature Web-Mgmt-Console
Install-WindowsFeature NET-Framework-45-ASPNET
Install-WindowsFeature Web-Asp-Net45

#
# Tools for building .NET projects on the server
#
Write-Host "`nInstalling .NET build tools...`n"

# .NET Framework 4.5 SDK
$sdktools_url = "http://download.microsoft.com/download/F/1/3/F1300C9C-A120-4341-90DF-8A52509B23AC/standalonesdk/sdksetup.exe"
$sdktools_local = "$workdir\sdksetup.exe"
(New-Object System.Net.WebClient).DownloadFile($sdktools_url, $sdktools_local)
Start-Process "$workdir\sdksetup.exe" "/features OptionId.NetFxSoftwareDevelopmentKit /q /layout $workdir\sdksetup" -Wait
Start-Process "msiexec" "/i $workdir\sdksetup\Redistributable\4.5.50710\sdk_tools4.msi VSEXTUI=1" -Wait

# Web Deploy 3.0
$webdeploy_url = "http://download.microsoft.com/download/1/B/3/1B3F8377-CFE1-4B40-8402-AE1FC6A0A8C3/WebDeploy_amd64_en-US.msi"
$webdeploy_local = "$workdir\WebDeploy_amd64_en-US.msi"
(New-Object System.Net.WebClient).DownloadFile($webdeploy_url, $webdeploy_local)
Start-Process "msiexec" "/i $webdeploy_local /passive" -Wait

#
# node.js
#
Write-Host "Installing node.js...`n"
$nodeinstaller_file = "node-v0.10.5-x64.msi"
$nodeinstaller_url = "http://nodejs.org/dist/v0.10.5/x64/" + $nodeinstaller_file
$nodeinstaller_local = $workdir + "\" + $nodeinstaller_file
(New-Object System.Net.WebClient).DownloadFile($nodeinstaller_url, $nodeinstaller_local)

Start-Process $nodeinstaller_local '/passive' -Wait
$env:Path += ";C:\Program Files\nodejs"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

#
# Python
#
Write-Host "Installing Python...`n"
$pythoninstaller_file = "python-2.7.4.amd64.msi"
$pythoninstaller_url = "http://www.python.org/ftp/python/2.7.4/" + $pythoninstaller_file
$pythoninstaller_local = $workdir + "\" + $pythoninstaller_file
(New-Object System.Net.WebClient).DownloadFile($pythoninstaller_url, $pythoninstaller_local)

Start-Process $pythoninstaller_local '/passive' -Wait
$env:Path += ";C:\Python27"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

#
# Firewall
#
Write-Host "Configuring firewall...`n"
New-NetFirewallRule -DisplayName "HTTP 8080" -Action Allow -Direction Inbound -LocalPort 8080 -Protocol TCP | Out-Null

cd $basedir