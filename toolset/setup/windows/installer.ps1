$basedir = "C:\FrameworkBenchmarks"
$workdir = "$basedir\installs"
$chocoinstallroot = "c:\tools\"
New-Item -Path $workdir -Type directory -Force | Out-Null

function RefreshPath()
{
    $env:Path = [Environment]::GetEnvironmentVariable("Path", [System.EnvironmentVariableTarget]::Machine)+";"+[Environment]::GetEnvironmentVariable("Path", [System.EnvironmentVariableTarget]::User)
}

function UpdatePath([string]$newPath)
{
    [Environment]::SetEnvironmentVariable("Path", $newPath, [System.EnvironmentVariableTarget]::Machine)
    
    RefreshPath
}

function Touch([string] $path)
{
    if(Test-Path $path)
    {
        (Get-ChildItem $path).LastWriteTime = Get-Date
    }
    else
    {
        echo $null > $path
    }
}

function UpdateWithChocoInstall([string]$toolName, [switch]$addBin)
{
    $newPath = $env:Path
    $installPath = "$chocoinstallroot\$toolName"
    $newPath += ";$installPath"
    if($addBin.IsPresent)
    {
        $newPath += ";$installPath\bin"
    }
    
    UpdatePath $newPath
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

UpdatePath "$env:Path;C:\Windows\system32\inetsrv"
# Optimize performance
appcmd set config -section:httpProtocol /allowKeepAlive:true | Out-Null
appcmd set config -section:httpLogging /dontLog:True | Out-Null
# Enable detailed error pages
#appcmd set config -section:system.webServer/httpErrors -errorMode:Detailed | Out-Null
# Increase queue length for DefaultAppPool to avoid HTTP 503 errors coming from HTTP.SYS
appcmd set apppool DefaultAppPool /queueLength:65535 | Out-Null
# Increase appConcurrentRequestLimit to avoid HTTP 503.2 errors from IIS http://support.microsoft.com/kb/943891
appcmd set config -section:system.webServer/serverRuntime /appConcurrentRequestLimit:65535 | Out-Null

# URL Rewrite
choco install urlrewrite -y

#
# Tools for building .NET projects on the server
#
Write-Host "`nInstalling Windows SDK and VC++ redist...`n"

# .NET 
choco install windows-sdk-10.0 -y
choco install vcredist2015 -y
choco install netfx-4.5.1-devpack -y
Touch "$workdir\dotnetcore-prereqs.installed"

# Web Deploy 3.0
choco install webdeploy -y

#
# node.js
#
Write-Host "`nInstalling Node...`n"
choco install nodejs -y
RefreshPath
Touch "$workdir\node.installed"

#
# Python
#
Write-Host "`nInstalling Python...`n"
choco install python2 -y
RefreshPath
Touch "$workdir\py2.installed"

python -m pip install --upgrade pip
python -m pip install colorama==0.3.1
python -m pip install progressbar==2.2
python -m pip install requests

#
# PHP
#
Write-Host "Installing PHP..."
$php = "$chocoinstallroot\php"
choco install php -y
RefreshPath

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
(Get-Content $phpini) -Replace "extension=php_(interbase|oci8|oci8_11g|oci8_12c|firebird|oci|pspell|sybase_ct|zip|pdo_firebird|pdo_oci|snmp).dll.*", "" | Set-Content $phpini

# IIS with PHP via FastCGI
Install-WindowsFeature Web-CGI | Out-Null
appcmd set config -section:system.webServer/fastCgi /+"[fullPath='C:\tools\PHP\php-cgi.exe', arguments='', maxInstances='0', instanceMaxRequests='10000', queueLength='1000', rapidFailsPerMinute='10', idleTimeout='300', activityTimeout='30', requestTimeout='90', protocol='NamedPipe', flushNamedPipe='False']" /commit:apphost | Out-Null
appcmd set config -section:system.webServer/fastCgi /+"[fullPath='C:\tools\PHP\php-cgi.exe'].environmentVariables.[name='PHPRC', value='C:\tools\PHP\php.ini']" /commit:apphost | Out-Null
appcmd set config -section:system.webServer/handlers /+"[name='PHP FastCGI', path='*.php', modules='FastCgiModule', verb='*', scriptProcessor='C:\tools\PHP\php-cgi.exe', resourceType='File', requireAccess='Script']" /commit:apphost | Out-Null

# phpinfo() test file
Set-Content "c:\inetpub\wwwroot\phpinfo.php" "<?php phpinfo(); ?>"
Touch "$workdir\php.installed"

# wincache
$wincache_installer_file  = "wincache-1.3.4-5.4-nts-vc9-x86.exe"
$wincache_installer_path  = "wincache-1.3.4/$wincache_installer_file"
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
choco install composer -y
RefreshPath
Touch "$workdir\composer.installed"

#
# Go
#
Write-Host "Installing Go...`n"
choco install golang -y
RefreshPath
Touch "$workdir\go.installed"

#
# Java
#
Write-Host "Installing JDK...`n"
choco install jdk8 -y
RefreshPath
$env:JAVA_HOME = [Environment]::GetEnvironmentVariable("JAVA_HOME", [System.EnvironmentVariableTarget]::Machine)
$env:CLASSPATH = [Environment]::GetEnvironmentVariable("CLASSPATH", [System.EnvironmentVariableTarget]::Machine)
$jdk8Home = $env:JAVA_HOME
Touch "$workdir\java.installed"

# resin
Write-Host "Installing Resin...`n"
$resin_version            = "resin-4.0.48"
$resin_installer_file     = "$resin_version.zip"
$resin_url = "http://www.caucho.com/download/$resin_installer_file"
$resin_local = "$workdir\$resin_installer_file"
$resin_dir = "$env:JAVA_HOME\resin"
(New-Object System.Net.WebClient).DownloadFile($resin_url, $resin_local)
[System.Reflection.Assembly]::LoadWithPartialName("System.IO.Compression.FileSystem") | Out-Null
[System.IO.Compression.ZipFile]::ExtractToDirectory($resin_local, $workdir) | Out-Null
Move-Item "$workdir\$resin_version" $resin_dir
Copy-Item "$basedir\config\resin.properties" "$resin_dir\conf\resin.properties"
$env:RESIN_HOME = $resin_dir
[Environment]::SetEnvironmentVariable("RESIN_HOME", $resin_dir, [System.EnvironmentVariableTarget]::Machine)
Touch "$workdir\resin.installed"


# maven
Write-Host "Installing Maven...`n"
choco install maven -y
RefreshPath
Touch "$workdir\maven.installed"

# scala
Write-Host "Installing Scala...`n"
choco install scala -y
RefreshPath
Touch "$workdir\scala.installed"

# scala installs jre7 which stomps some settings
choco uninstall javaruntime -version 7.0.75 -y
(Get-WmiObject -Class Win32_Product -Filter "Name = 'Java 7 Update 75'").Uninstall()
(Get-WmiObject -Class Win32_Product -Filter "Name = 'Java 7 Update 75 (64-bit)'").Uninstall()
$env:JAVA_HOME = $jdk8Home
[Environment]::SetEnvironmentVariable("JAVA_HOME", $jdk8Home, [System.EnvironmentVariableTarget]::Machine)

# play
Write-Host "Installing Play...`n"
choco install play -y
RefreshPath
Touch "$workdir\play1.installed"

# sbt
Write-Host "Installing Sbt...`n"
choco install sbt -y
RefreshPath
Touch "$workdir\sbt.installed"

#
# Mercurial
#
Write-Host "Installing Mercurial...`n"
choco install tortoisehg -y
RefreshPath

#
# Cygwin (including sftp)
#
Write-Host "Installing Cygwin...`n"
choco install cygwin -y
UpdateWithChocoInstall "cygwin" -addBin

#
# Firewall
#
Write-Host "Configuring firewall...`n"
New-NetFirewallRule -DisplayName "HTTP 8080" -Action Allow -Direction Inbound -LocalPort 8080 -Protocol TCP | Out-Null

Touch "$workdir\prerequisites.installed"
cd $basedir
