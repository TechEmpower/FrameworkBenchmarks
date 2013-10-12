#
# Versions of software (will need to be updated from time to time)
#

$node_installer_file      = "node-v0.10.13-x64.msi"
$node_installer_path      = "v0.10.13/x64/$node_installer_file"
$python_installer_file    = "python-2.7.5.amd64.msi"
$python_installer_path    = "2.7.5/$python_installer_file"
$python_version           = "27"
$wincache_installer_file  = "wincache-1.3.4-5.4-nts-vc9-x86.exe"
$wincache_installer_path  = "wincache-1.3.4/$wincache_installer_file"
$go_installer_file        = "go1.1.1.windows-amd64.msi"
$jre_installer_file       = "jre-7u25-windows-x64.exe"
$jdk_installer_file       = "jdk-7u40-windows-x64.exe"
$jdk_master_hash          = "7412ccc2ac8a0f418eb58c5f170742a3" 
# http://www.oracle.com/technetwork/java/javase/downloads/java-se-binaries-checksum-1956892.html
$resin_version            = "resin-4.0.36"
$resin_installer_file     = "$resin_version.zip"
$ant_version              = "apache-ant-1.9.2"
$ant_installer_file       = "$ant_version-bin.zip"
$maven_version            = "apache-maven-3.0.5"
$maven_installer_file     = "$maven_version-bin.zip"
$maven_installer_path     = "maven-3/3.0.5/binaries/$maven_installer_file"
$scala_version            = "2.10.2"
$play_version             = "2.2.0"
$play_installer_file      = "play-$play_version.zip"
$mercurial_installer_file = "mercurial-2.6.1-x64.msi"
$cygwin_installer_file    = "setup-x86_64.exe"

$basedir = "C:\FrameworkBenchmarks"
$workdir = "$basedir\installs"
New-Item -Path $workdir -Type directory -Force | Out-Null

function GetMd5FileHash($fileName) {
    [Reflection.Assembly]::LoadWithPartialName("System.Security") | out-null
    $md5 = [System.Security.Cryptography.MD5]::Create()

    $file = [System.IO.File]::OpenRead($fileName)
    $hash = $md5.ComputeHash($file)
    $file.Dispose()

    $sb = New-Object System.Text.StringBuilder
    $hash | % { [Void]$sb.Append($_.ToString("x2")) }
    $sb.ToString()
}

#
# Chocolatey package manager
#
Write-Host "Installing Chocolatey package manager"
Invoke-Expression ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))

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

$env:Path += ";C:\Windows\system32\inetsrv"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)
# Optimize performance
appcmd set config -section:httpProtocol /allowKeepAlive:true | Out-Null
appcmd set config -section:httpLogging /dontLog:True | Out-Null
# Enable detailed error pages
#appcmd set config -section:system.webServer/httpErrors -errorMode:Detailed | Out-Null

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
$node_installer_url = "http://nodejs.org/dist/$node_installer_path"
$node_installer_local = "$workdir\$node_installer_file"
(New-Object System.Net.WebClient).DownloadFile($node_installer_url, $node_installer_local)

Start-Process $node_installer_local '/passive' -Wait
$env:Path += ";C:\Program Files\nodejs"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

#
# Python
#
Write-Host "Installing Python...`n"
$python_installer_url = "http://www.python.org/ftp/python/$python_installer_path"
$python_installer_local = "$workdir\$python_installer_file"
(New-Object System.Net.WebClient).DownloadFile($python_installer_url, $python_installer_local)

Start-Process $python_installer_local '/passive' -Wait
$env:Path += ";C:\Python$python_version"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

#
# PHP
#
Write-Host "Installing PHP..."

# Locate current PHP 5.4 release
Write-Host "Looking for current PHP 5.4 release"
$php_download_page_url = 'http://windows.php.net/download/'
$php_download_page_file = [IO.Path]::GetTempFileName()
Write-Host "Downloading from $php_download_page_url into $php_download_page_file"
Try {
    (New-Object System.Net.WebClient).DownloadFile($php_download_page_url, $php_download_page_file)
} Catch {
    Write-Host "ERROR: Could not download from $php_download_page_url."
    Write-Host $_.Exception.Message
    Exit 1
}
$file = (cat $php_download_page_file) -join ""
if ($file -match '(?s)h4 id="php-5.4-nts-VC9-x86".*?href="/downloads/releases/(.*?)">Zip</a>') {
    $php_installer_file = $matches[1]
    $php_installer_url = "http://windows.php.net/downloads/releases/$php_installer_file"
    Write-Host "Current PHP 5.4 release found at $php_installer_url"
}
else {
    Write-Host "ERROR: Current PHP release was not found. Aborting."
    Exit 1
}

# Download PHP
$php_installer_local = "$workdir\$php_installer_file"
Try {
    (New-Object System.Net.WebClient).DownloadFile($php_installer_url, $php_installer_local)
} Catch {
    Write-Host "ERROR: Could not download from $php_installer_url. "
    Write-Host $_.Exception.Message
    Exit 1
}

# Install PHP
$php = "C:\PHP"
[System.Reflection.Assembly]::LoadWithPartialName("System.IO.Compression.FileSystem") | Out-Null
[System.IO.Compression.ZipFile]::ExtractToDirectory($php_installer_local, $php) | Out-Null
$env:Path += ";" + $php; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

# php.ini
$phpini = "$php\php.ini"
Copy-Item "$php\php.ini-production" $phpini
(Get-Content $phpini) -Replace ";date.timezone =", "date.timezone = UTC" | Set-Content $phpini
(Get-Content $phpini) -Replace "short_open_tag = Off", "short_open_tag = On" | Set-Content $phpini
(Get-Content $phpini) -Replace "display_errors = Off", "display_errors = Off" | Set-Content $phpini
(Get-Content $phpini) -Replace "log_errors = On", "log_errors = Off" | Set-Content $phpini
(Get-Content $phpini) -Replace "output_buffering = 4096", "output_buffering = Off" | Set-Content $phpini
(Get-Content $phpini) -Replace ";cgi.force_redirect = 1", "cgi.force_redirect = 0" | Set-Content $phpini
(Get-Content $phpini) -Replace ";fastcgi.impersonate = 1", "fastcgi.impersonate = 0" | Set-Content $phpini
(Get-Content $phpini) -Replace ";fastcgi.logging = 0", "fastcgi.logging = 0" | Set-Content $phpini
(Get-Content $phpini) -Replace '; extension_dir = "./"', "extension_dir = `"$php\ext`"" | Set-Content $phpini
(Get-Content $phpini) -Replace ";extension=", "extension=" | Set-Content $phpini
(Get-Content $phpini) -Replace "extension=php_(interbase|oci8|oci8_11g|firebird|oci|pspell|sybase_ct|zip|pdo_firebird|pdo_oci|snmp).dll.*", "" | Set-Content $phpini

# IIS with PHP via FastCGI
Install-WindowsFeature Web-CGI | Out-Null
appcmd set config -section:system.webServer/fastCgi /+"[fullPath='C:\PHP\php-cgi.exe', arguments='', maxInstances='0', instanceMaxRequests='10000', queueLength='1000', rapidFailsPerMinute='10', idleTimeout='300', activityTimeout='30', requestTimeout='90', protocol='NamedPipe', flushNamedPipe='False']" /commit:apphost | Out-Null
appcmd set config -section:system.webServer/fastCgi /+"[fullPath='C:\PHP\php-cgi.exe'].environmentVariables.[name='PHPRC', value='C:\PHP\php.ini']" /commit:apphost | Out-Null
appcmd set config -section:system.webServer/handlers /+"[name='PHP FastCGI', path='*.php', modules='FastCgiModule', verb='*', scriptProcessor='C:\PHP\php-cgi.exe', resourceType='File', requireAccess='Script']" /commit:apphost | Out-Null

# phpinfo() test file
Set-Content "c:\inetpub\wwwroot\phpinfo.php" "<?php phpinfo(); ?>"

# wincache
$wincache_url = "http://heanet.dl.sourceforge.net/project/wincache/$wincache_installer_path"
$wincache_local = "$workdir\$wincache_installer_file"
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

Write-Host ""

#
# Go
#
Write-Host "Installing Go...`n"
$go_url = "http://go.googlecode.com/files/$go_installer_file"
$go_local = "$workdir\$go_installer_file"
(New-Object System.Net.WebClient).DownloadFile($go_url, $go_local)
Start-Process $go_local "/passive" -Wait
$env:Path += ";C:\Go\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

#
# Java
#
Write-Host "Installing Java...`n"

# jre
#Write-Host "Installing JRE...`n"
#$jre_url = "http://img.cs.montana.edu/windows/$jre_installer_file"
#$jre_local = "$workdir\$jre_installer_file"
#$jre_dir = "C:\Java\jre"
#(New-Object System.Net.WebClient).DownloadFile($jre_url, $jre_local)
#Start-Process $jre_local "/s INSTALLDIR=$jre_dir" -Wait
#$env:Path += ";$jre_dir\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)
#$env:JAVA_HOME = $jre_dir; [Environment]::SetEnvironmentVariable("JAVA_HOME", $jre_dir, [System.EnvironmentVariableTarget]::Machine)

# jdk
Write-Host "Installing JDK...`n"
$jdk_url = "http://ghaffarian.net/downloads/Java/JDK/$jdk_installer_file"
$jdk_local = "$workdir\$jdk_installer_file"
$jdk_dir = "C:\Java\jdk"
(New-Object System.Net.WebClient).DownloadFile($jdk_url, $jdk_local)

$jdk_local_hash = GetMd5FileHash($jdk_local)
if ($jdk_master_hash -ne $jdk_local_hash)
{
    Write-Host $jdk_master_hash
    Write-Host $jdk_local_hash
    Write-Host "JDK file checksum mismatch. Aborting!"
    Exit 1
}

Start-Process $jdk_local "/s INSTALLDIR=$jdk_dir" -Wait
$env:Path += ";$jdk_dir\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)
$env:JAVA_HOME = $jdk_dir; [Environment]::SetEnvironmentVariable("JAVA_HOME", $jdk_dir, [System.EnvironmentVariableTarget]::Machine)

# resin
Write-Host "Installing Resin...`n"
$resin_url = "http://www.caucho.com/download/$resin_installer_file"
$resin_local = "$workdir\$resin_installer_file"
$resin_dir = "C:\Java\resin"
(New-Object System.Net.WebClient).DownloadFile($resin_url, $resin_local)
[System.Reflection.Assembly]::LoadWithPartialName("System.IO.Compression.FileSystem") | Out-Null
[System.IO.Compression.ZipFile]::ExtractToDirectory($resin_local, $workdir) | Out-Null
Move-Item "$workdir\$resin_version" $resin_dir
Copy-Item "$basedir\config\resin.properties" "$resin_dir\conf\resin.properties"
[Environment]::SetEnvironmentVariable("RESIN_HOME", $resin_dir, [System.EnvironmentVariableTarget]::Machine)
#$env:Path += ";$resin_dir\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

# ant
#Write-Host "Installing Ant...`n"
#$ant_url = "http://apache.mirrors.hoobly.com//ant/binaries/$ant_installer_file"
#$ant_local = "$workdir\$ant_installer_file"
#$ant_dir = "C:\Java\ant"
#(New-Object System.Net.WebClient).DownloadFile($ant_url, $ant_local)
#[System.Reflection.Assembly]::LoadWithPartialName("System.IO.Compression.FileSystem") | Out-Null
#[System.IO.Compression.ZipFile]::ExtractToDirectory($ant_local, $workdir) | Out-Null
#Move-Item "$workdir\$ant_version" $ant_dir
#$env:Path += ";$ant_dir\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

# maven
Write-Host "Installing Maven...`n"
$maven_url = "http://mirror.cc.columbia.edu/pub/software/apache/maven/$maven_installer_path"
$maven_local = "$workdir\$maven_installer_file"
$maven_dir = "C:\Java\maven"
(New-Object System.Net.WebClient).DownloadFile($maven_url, $maven_local)
[System.Reflection.Assembly]::LoadWithPartialName("System.IO.Compression.FileSystem") | Out-Null
[System.IO.Compression.ZipFile]::ExtractToDirectory($maven_local, $workdir) | Out-Null
Move-Item "$workdir\$maven_version" $maven_dir
$env:Path += ";$maven_dir\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

# scala
cinst scala -version $scala_version

# play
$play_url = "http://downloads.typesafe.com/play/$play_version/$play_installer_file"
$play_local = "$workdir\$play_installer_file"
$play_dir = "C:\Java\play"
(New-Object System.Net.WebClient).DownloadFile($play_url, $play_local)
[System.Reflection.Assembly]::LoadWithPartialName("System.IO.Compression.FileSystem") | Out-Null
[System.IO.Compression.ZipFile]::ExtractToDirectory($play_local, $workdir) | Out-Null
Move-Item "$workdir\play-$play_version" $play_dir
$env:Path += ";$play_dir"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

#
# Firewall
#
Write-Host "Configuring firewall...`n"
New-NetFirewallRule -DisplayName "HTTP 8080" -Action Allow -Direction Inbound -LocalPort 8080 -Protocol TCP | Out-Null

#
# Mercurial
#
Write-Host "Installing Mercurial...`n"
$hg_installer_url = "https://bitbucket.org/tortoisehg/files/downloads/$mercurial_installer_file"
$hg_installer_local = "$workdir\$mercurial_installer_file"
(New-Object System.Net.WebClient).DownloadFile($hg_installer_url, $hg_installer_local)

Start-Process $hg_installer_local '/passive' -Wait
$env:Path += ";C:\Program Files\Mercurial"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

#
# Cygwin (including sftp)
#
Write-Host "Installing Cygwin...`n"
$cygwin_installer_url = "http://cygwin.com/$cygwin_installer_file"
$cygwin_installer_dir = $workdir + "\cygwin-installer"
New-Item -Path $cygwin_installer_dir -Type directory -Force | Out-Null
$cygwin_installer_local = "$cygwin_installer_dir\$cygwin_installer_file"
(New-Object System.Net.WebClient).DownloadFile($cygwin_installer_url, $cygwin_installer_local)

$cygwin_install_dir = "C:\Cygwin"
Start-Process $cygwin_installer_local "-q -n -l $cygwin_installer_dir -s http://mirrors.kernel.org/sourceware/cygwin/ -R $cygwin_install_dir -P openssh" -WorkingDirectory "$cygwin_installer_dir" -Wait -RedirectStandardOutput $cygwin_installer_dir\install.log
$env:Path += ";$cygwin_install_dir;$cygwin_install_dir\bin"; [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::Machine)

cd $basedir
