# This script downloads and installs SQL Server and opens it on port 1433.
#
# To run this script, run an elevated Command Prompt and enter:
#
# powershell -ExecutionPolicy Bypass -File <this script's filename>

$basedir = "C:\FrameworkBenchmarks"
$workdir = "$basedir\installs"
New-Item -Path $workdir -Type directory -Force | Out-Null

Write-Host "Downloading SQL Server (several GBs)...`n"

# URLs from http://www.microsoft.com/en-us/download/details.aspx?id=35575

$sqlserver_exe_url = "http://download.microsoft.com/download/3/B/D/3BD9DD65-D3E3-43C3-BB50-0ED850A82AD5/SQLServer2012SP1-FullSlipstream-x64-ENU.exe"
$sqlserver_exe_local = "$workdir\SQLServer2012SP1-FullSlipstream-x64-ENU.exe"
(New-Object System.Net.WebClient).DownloadFile($sqlserver_exe_url, $sqlserver_exe_local)

$sqlserver_box_url = "http://download.microsoft.com/download/3/B/D/3BD9DD65-D3E3-43C3-BB50-0ED850A82AD5/SQLServer2012SP1-FullSlipstream-x64-ENU.box"
$sqlserver_box_local = "$workdir\SQLServer2012SP1-FullSlipstream-x64-ENU.box"
(New-Object System.Net.WebClient).DownloadFile($sqlserver_box_url, $sqlserver_box_local)

Write-Host "Installing SQL Server...`n"

# Install only the SQL Server database engine.
# Use a default instance name.
# Make %COMPUTERNAME%\Administrators have administrative rights.
# Allow Windows Authentication or old-style SQL authentication.
# The password of the sa account is specified.
# SQL Server will be listening on TCP port 1433.
#
Start-Process "$sqlserver_exe_local" "/q /action=install /features=SQLEngine /INSTANCENAME=MSSQLSERVER /SQLSYSADMINACCOUNTS=Administrators /securitymode=SQL /sapwd=S3cr3tS3cr3t /TCPENABLED=1 /IACCEPTSQLSERVERLICENSETERMS" -Wait

Write-Host "Configuring firewall...`n"
New-NetFirewallRule -DisplayName "SQL 1433" -Action Allow -Direction Inbound -LocalPort 1433 -Protocol TCP | Out-Null

Write-Host "Creating SQL Server login and populated database...`n"

Import-Module sqlps

Invoke-Sqlcmd -InputFile "$basedir\config\create-sqlserver-login-and-database.sql" -OutputSqlErrors $True

Invoke-Sqlcmd -Username benchmarkdbuser -Password B3nchmarkDBPass -Database hello_world -InputFile "$basedir\config\create-sqlserver.sql" -OutputSqlErrors $True
