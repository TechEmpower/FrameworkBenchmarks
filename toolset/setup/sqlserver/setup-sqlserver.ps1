# This script downloads and installs SQL Server and opens it on port 1433.
#
# To run this script, run an elevated Command Prompt and enter:
#
# powershell -ExecutionPolicy Bypass -File <this script's filename>

$basedir = "C:\FrameworkBenchmarks"
$workdir = "$basedir\installs"
New-Item -Path $workdir -Type directory -Force | Out-Null

If (-Not (Get-Service | ? Name -Eq "MSSQLSERVER")) {

  Write-Host "Could not find default SQL Server instance, MSSQLSERVER."
  Write-Host "Downloading SQL Server (several GBs)..."

  # URLs from http://www.microsoft.com/en-us/download/details.aspx?id=35575

  $sqlserver_exe_url = "http://download.microsoft.com/download/3/B/D/3BD9DD65-D3E3-43C3-BB50-0ED850A82AD5/SQLServer2012SP1-FullSlipstream-x64-ENU.exe"
  $sqlserver_exe_local = "$workdir\SQLServer2012SP1-FullSlipstream-x64-ENU.exe"
  (New-Object System.Net.WebClient).DownloadFile($sqlserver_exe_url, $sqlserver_exe_local)

  $sqlserver_box_url = "http://download.microsoft.com/download/3/B/D/3BD9DD65-D3E3-43C3-BB50-0ED850A82AD5/SQLServer2012SP1-FullSlipstream-x64-ENU.box"
  $sqlserver_box_local = "$workdir\SQLServer2012SP1-FullSlipstream-x64-ENU.box"
  (New-Object System.Net.WebClient).DownloadFile($sqlserver_box_url, $sqlserver_box_local)

  Write-Host "Installing SQL Server..."

  # Install only the SQL Server database engine.
  # Use a default instance name.
  # Make %COMPUTERNAME%\Administrators have administrative rights.
  
  # The following is not used because this is done in PowerShell below.
  #   /securitymode=SQL /sapwd=S3cr3tS3cr3t /TCPENABLED=1
  #   Allow Windows Authentication or old-style SQL authentication.
  #   The password of the sa account is specified.
  #   SQL Server will be listening on TCP port 1433.
  #
  Start-Process "$sqlserver_exe_local" "/q /action=install /features=SQLEngine /INSTANCENAME=MSSQLSERVER /SQLSYSADMINACCOUNTS=Administrators /IACCEPTSQLSERVERLICENSETERMS" -Wait
}

# In case we just installed SQL Server and the environment variables haven't been refreshed, manually
# refresh PSModulePath so that Import-Module sqlps will work.

$env:PSModulePath = [Environment]::GetEnvironmentVariable("PSModulePath", [System.EnvironmentVariableTarget]::Machine)

Import-Module sqlps

Write-Host "Setting SQL Server to start on boot..."

Set-Service MSSQLSERVER -StartupType Automatic

Write-Host "Ensuring that SQL Server is started..."

Start-Service MSSQLSERVER

Write-Host "Enabling SQL authentication..."

# Enable SQL authentication
$s = New-Object ('Microsoft.SqlServer.Management.Smo.Server')
$s.Settings.LoginMode = [Microsoft.SqlServer.Management.SMO.ServerLoginMode]::Mixed
$s.Alter()

Write-Host "Configuring SQL Server to listen on TCP (default port 1433)..."

# Enable the TCP protocol on the default instance.
$wmi = New-Object ('Microsoft.SqlServer.Management.Smo.Wmi.ManagedComputer')
$uri = "ManagedComputer[@Name='" + (Get-Content env:computername) + "']/ ServerInstance[@Name='MSSQLSERVER']/ServerProtocol[@Name='Tcp']"
$Tcp = $wmi.GetSmoObject($uri)
$Tcp.IsEnabled = $true
$Tcp.Alter()

Write-Host "Restarting SQL Server..."

Restart-Service -Name MSSQLSERVER

If (-Not (Get-NetFirewallPortFilter | ? LocalPort -Eq "1433")) {
  Write-Host "Opening port 1433 in firewall..."
  New-NetFirewallRule -DisplayName "SQL 1433" -Action Allow -Direction Inbound -LocalPort 1433 -Protocol TCP | Out-Null
} else {
  Write-Host "Port 1433 is already configured in firewall."
}

Write-Host "Creating SQL Server login and populated database..."

# Connect with Windows Authentication, assuming that we have access.
Invoke-Sqlcmd -InputFile "$basedir\config\create-sqlserver-login-and-database.sql" -OutputSqlErrors $True -QueryTimeout 180

# Now that benchmarkdbuser has been created, we can connect with those credentials.
Invoke-Sqlcmd -Username benchmarkdbuser -Password B3nchmarkDBPass -Database hello_world -InputFile "$basedir\config\create-sqlserver.sql" -OutputSqlErrors $True -QueryTimeout 180

Write-Host "Done."
