$basedir = "C:\FrameworkBenchmarks"
$workdir = "$basedir\installs"
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

# Enable detailed error pages
$env:Path += ";C:\Windows\system32\inetsrv"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)
appcmd set config -section:system.webServer/httpErrors -errorMode:Detailed | Out-Null

# URL Rewrite
$rewrite_url = "http://download.microsoft.com/download/6/7/D/67D80164-7DD0-48AF-86E3-DE7A182D6815/rewrite_2.0_rtw_x64.msi"
$rewrite_local = "$workdir\rewrite_2.0_rtw_x64.msi"
(New-Object System.Net.WebClient).DownloadFile($rewrite_url, $rewrite_local)
Start-Process "msiexec" "/i $rewrite_local /passive" -Wait

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
$node_installer_file = "node-v0.10.5-x64.msi"
$node_installer_url = "http://nodejs.org/dist/v0.10.5/x64/$node_installer_file"
$node_installer_local = "$workdir\$node_installer_file"
(New-Object System.Net.WebClient).DownloadFile($node_installer_url, $node_installer_local)

Start-Process $node_installer_local '/passive' -Wait
$env:Path += ";C:\Program Files\nodejs"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

#
# Python
#
Write-Host "Installing Python...`n"
$python_installer_file = "python-2.7.4.amd64.msi"
$python_installer_url = "http://www.python.org/ftp/python/2.7.4/$python_installer_file"
$python_installer_local = "$workdir\$python_installer_file"
(New-Object System.Net.WebClient).DownloadFile($python_installer_url, $python_installer_local)

Start-Process $python_installer_local '/passive' -Wait
$env:Path += ";C:\Python27"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

#
# PHP
#
Write-Host "Installing PHP...`n"

# Download PHP
$php_installer_file = "php-5.4.14-nts-Win32-VC9-x86.zip"
$php_installer_url = "http://windows.php.net/downloads/releases/archives/$php_installer_file"
$php_installer_local = "$workdir\$php_installer_file"
(New-Object System.Net.WebClient).DownloadFile($php_installer_url, $php_installer_local)

# Install PHP
$php = "C:\PHP"
[System.Reflection.Assembly]::LoadWithPartialName("System.IO.Compression.FileSystem") | Out-Null
[System.IO.Compression.ZipFile]::ExtractToDirectory($php_installer_local, $php) | Out-Null
$env:Path += ";" + $php; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

# php.ini
$phpini = "$php\php.ini"
Copy-Item "$php\php.ini-production" $phpini
(Get-Content $phpini) -Replace ";date.timezone =", "date.timezone = UTC" | Set-Content $phpini
(Get-Content $phpini) -Replace "display_errors = Off", "display_errors = On" | Set-Content $phpini
(Get-Content $phpini) -Replace "short_open_tag = Off", "short_open_tag = On" | Set-Content $phpini
(Get-Content $phpini) -Replace '; extension_dir = "./"', "extension_dir = `"$php\ext`"" | Set-Content $phpini
(Get-Content $phpini) -Replace ";extension=", "extension=" | Set-Content $phpini
(Get-Content $phpini) -Replace "extension=php_(interbase|oci8|oci8_11g|firebird|oci|pspell|sybase_ct|zip|pdo_firebird|pdo_oci|snmp).dll.*", "" | Set-Content $phpini

# IIS with PHP via FastCGI
Install-WindowsFeature Web-CGI | Out-Null
appcmd set config -section:system.webServer/fastCgi /+"[fullPath='C:\PHP\php-cgi.exe', arguments='', maxInstances='4', instanceMaxRequests='10000', queueLength='1000', rapidFailsPerMinute='1000', idleTimeout='300', activityTimeout='30', requestTimeout='90',protocol='NamedPipe', flushNamedPipe='False']" /commit:apphost | Out-Null
appcmd set config -section:system.webServer/fastCgi /+"[fullPath='C:\PHP\php-cgi.exe'].environmentVariables.[name='PHPRC', value='C:\PHP\php.ini']" /commit:apphost | Out-Null
appcmd set config -section:system.webServer/handlers /+"[name='PHP FastCGI', path='*.php', modules='FastCgiModule', verb='*', scriptProcessor='C:\PHP\php-cgi.exe', resourceType='File', requireAccess='Script']" /commit:apphost | Out-Null

# phpinfo() test file
Set-Content "c:\inetpub\wwwroot\phpinfo.php" "<?php phpinfo(); ?>"

# wincache
$wincache_url = "http://heanet.dl.sourceforge.net/project/wincache/wincache-1.3.4/wincache-1.3.4-5.4-nts-vc9-x86.exe"
$wincache_local = "$workdir\wincache-1.3.4-5.4-nts-vc9-x86.exe"
(New-Object System.Net.WebClient).DownloadFile($wincache_url, $wincache_local)
Start-Process $wincache_local "/q /T:$php\ext" -Wait
Move-Item "$php\ext\wincache*" "c:\inetpub\wwwroot"
Set-ItemProperty "c:\inetpub\wwwroot\wincache.php" -name IsReadOnly -value $false
(Get-Content "c:\inetpub\wwwroot\wincache.php") -Replace "'USE_AUTHENTICATION', 1", "'USE_AUTHENTICATION', 0" | Set-Content "c:\inetpub\wwwroot\wincache.php"
Add-Content $phpini "`n`n[PHP]`n"
Add-Content $phpini "extension=php_wincache.dll"

# composer
$composer_url = "https://getcomposer.org/Composer-Setup.exe"
$composer_local = "$workdir\Composer-Setup.exe"
(New-Object System.Net.WebClient).DownloadFile($composer_url, $composer_local)
Start-Process $composer_local "/silent" -Wait
$env:Path += ";C:\ProgramData\Composer\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

#
# Go
#
Write-Host "Installing Go...`n"
$go_url = "https://go.googlecode.com/files/go1.1rc3.windows-amd64.msi"
$go_local = "$workdir\go1.1rc3.windows-amd64.msi"
(New-Object System.Net.WebClient).DownloadFile($go_url, $go_local)
Start-Process $go_local "/passive" -Wait
$env:Path += ";C:\Go\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

#
# Java
#
Write-Host "Installing Java...`n"

# jre
#$jre_url = "http://img.cs.montana.edu/windows/jre-7u21-windows-x64.exe"
#$jre_local = "$workdir\jre-7u21-windows-x64.exe"
#$jre_dir = "C:\Java\jre"
#(New-Object System.Net.WebClient).DownloadFile($jre_url, $jre_local)
#Start-Process $jre_local "/s INSTALLDIR=$jre_dir" -Wait
#$env:Path += ";$jre_dir\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)
#$env:JAVA_HOME = $jre_dir; [Environment]::SetEnvironmentVariable("JAVA_HOME", $jre_dir, [System.EnvironmentVariableTarget]::Machine)

# jdk
$jdk_url = "http://uni-smr.ac.ru/archive/dev/java/SDKs/sun/j2se/7/jdk-7u21-windows-x64.exe"
$jdk_local = "$workdir\jdk-7u21-windows-x64.exe"
$jdk_dir = "C:\Java\jdk"
(New-Object System.Net.WebClient).DownloadFile($jdk_url, $jdk_local)
Start-Process $jdk_local "/s INSTALLDIR=$jdk_dir" -Wait
$env:Path += ";$jdk_dir\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)
$env:JAVA_HOME = $jdk_dir; [Environment]::SetEnvironmentVariable("JAVA_HOME", $jre_dir, [System.EnvironmentVariableTarget]::Machine)

# resin
$resin_url = "http://www.caucho.com/download/resin-4.0.36.zip"
$resin_local = "$workdir\resin-4.0.36.zip"
$resin_dir = "C:\Java\resin"
(New-Object System.Net.WebClient).DownloadFile($resin_url, $resin_local)
[System.Reflection.Assembly]::LoadWithPartialName("System.IO.Compression.FileSystem") | Out-Null
[System.IO.Compression.ZipFile]::ExtractToDirectory($resin_local, $workdir) | Out-Null
Move-Item "$workdir\resin-4.0.36" $resin_dir
Copy-Item "$basedir\config\resin.properties" "$resin_dir\conf\resin.properties"
#$env:Path += ";$resin_dir\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

# ant
#$ant_url = "http://apache.mirrors.hoobly.com//ant/binaries/apache-ant-1.9.0-bin.zip"
#$ant_local = "$workdir\apache-ant-1.9.0-bin.zip"
#$ant_dir = "C:\Java\ant"
#(New-Object System.Net.WebClient).DownloadFile($ant_url, $ant_local)
#[System.Reflection.Assembly]::LoadWithPartialName("System.IO.Compression.FileSystem") | Out-Null
#[System.IO.Compression.ZipFile]::ExtractToDirectory($ant_local, $workdir) | Out-Null
#Move-Item "$workdir\apache-ant-1.9.0" $ant_dir
#$env:Path += ";$ant_dir\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

# maven
$maven_url = "http://mirror.cc.columbia.edu/pub/software/apache/maven/maven-3/3.0.5/binaries/apache-maven-3.0.5-bin.zip"
$maven_local = "$workdir\apache-maven-3.0.5-bin.zip"
$maven_dir = "C:\Java\maven"
(New-Object System.Net.WebClient).DownloadFile($maven_url, $maven_local)
[System.Reflection.Assembly]::LoadWithPartialName("System.IO.Compression.FileSystem") | Out-Null
[System.IO.Compression.ZipFile]::ExtractToDirectory($maven_local, $workdir) | Out-Null
Move-Item "$workdir\apache-maven-3.0.5" $maven_dir
$env:Path += ";$maven_dir\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

#
# Firewall
#
Write-Host "Configuring firewall...`n"
New-NetFirewallRule -DisplayName "HTTP 8080" -Action Allow -Direction Inbound -LocalPort 8080 -Protocol TCP | Out-Null

#
# Mercurial
#
Write-Host "Installing Mercurial...`n"

$hg_installer_file = "mercurial-2.6.1-x64.msi"
$hg_installer_url = "https://bitbucket.org/tortoisehg/files/downloads/$hg_installer_file"
$hg_installer_local = "$workdir\$hg_installer_file"
(New-Object System.Net.WebClient).DownloadFile($hg_installer_url, $hg_installer_local)

Start-Process $hg_installer_local '/passive' -Wait
$env:Path += ";C:\Program Files\Mercurial"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

cd $basedir