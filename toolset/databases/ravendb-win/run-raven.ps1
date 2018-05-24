$ErrorActionPreference='Stop'
$ProgressPreference = "SilentlyContinue";

if ([string]::IsNullOrEmpty($env:RAVEN_SETTINGS) -eq $False) {
    Set-Content -Path "settings.json" -Value "$env:RAVEN_SETTINGS"
}

$hostname = & "hostname.exe"
$env:RAVEN_ServerUrl = "http://$($hostname):8080"

$COMMAND=".\Raven.Server.exe"
$ravenProcess = Start-Process -FilePath $COMMAND -NoNewWindow

write-host "Started RavenDB."
Start-Sleep -Seconds 2

function CreateDatabase($dbName) {
Invoke-WebRequest -Uri "$($env:RAVEN_ServerUrl)/admin/databases?name=$dbName&replicationFactor=1" `
    -Method "PUT" `
    -Body "{`"DatabaseName`":`"$dbName`",`"Settings`":{},`"Disabled`":false,`"Encrypted`":false,`"Topology`":{`"DynamicNodesDistribution`":false}}"
}

Add-Type -AssemblyName System.Web
function GetImportUri($dbName, $filePath) {
    $encodedFilePath = [System.Net.WebUtility]::UrlEncode($filePath);
    return "$($env:RAVEN_ServerUrl)/databases/$dbName/admin/smuggler/import?file=$encodedFilePath"
}

# import dbs
CreateDatabase("world")
$worldImportUri = GetImportUri "world" "C:\datadumps\TechEmpower-World.ravendbdump"
Invoke-WebRequest -Uri $worldImportUri 

CreateDatabase("fortunes")
$fortunesImportUri = GetImportUri "fortunes" "C:\datadumps\TechEmpower-Fortunes.ravendbdump"
Invoke-WebRequest -Uri $fortunesImportUri

write-host "All data imported."

while (!$ravenProcess.HasExited) {
    Start-Sleep -Seconds 5
}
