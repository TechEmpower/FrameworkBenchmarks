# To download and run this script, open an elevated Command Prompt and then run:
#
# powershell -ExecutionPolicy Bypass -Command "iex (New-Object Net.WebClient).DownloadString('https://raw.github.com/TechEmpower/FrameworkBenchmarks/master/setup-sqlserver-bootstrap.ps1')"

$basedir = "C:\FrameworkBenchmarks"
$rawRepo = "https://raw.github.com/TechEmpower/FrameworkBenchmarks/master"

$config_url = $rawRepo + "/config"
$config_local = $basedir + "\config"
$setup_sqlserver_url = $rawRepo + "/setup-sqlserver.ps1"
$setup_sqlserver_local = $basedir + "\setup-sqlserver.ps1"
$create_sqlserver_login_and_database_url = $config_url + "/create-sqlserver-login-and-database.sql"
$create_sqlserver_login_and_database_local = $config_local + "/create-sqlserver-login-and-database.sql"
$create_sqlserver_url = $config_url + "/create-sqlserver.sql"
$create_sqlserver_local = $config_local + "/create-sqlserver.sql"

Write-Host "Creating directory: $config`n"
New-Item -Path $config_local -Type Directory -Force | Out-Null

Write-Host "Downloading setup files...`n"
(New-Object System.Net.WebClient).DownloadFile($setup_sqlserver_url, $setup_sqlserver_local)
(New-Object System.Net.WebClient).DownloadFile($create_sqlserver_login_and_database_url, $create_sqlserver_login_and_database_local)
(New-Object System.Net.WebClient).DownloadFile($create_sqlserver_url, $create_sqlserver_local)

powershell -ExecutionPolicy Bypass -File $setup_sqlserver_local
