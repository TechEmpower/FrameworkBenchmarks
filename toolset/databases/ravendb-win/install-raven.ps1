New-Item -Type Directory -Force -Path c:/RavenDB | Out-Null
Expand-Archive c:/ravendb.zip -DestinationPath c:/RavenDB -Force
Remove-Item c:\ravendb.zip
