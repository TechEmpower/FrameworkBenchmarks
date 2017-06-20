param([string]$scenarios="[default]", [string]$server="kestrel")

$scenarios = (-split $scenarios) -join ","

cd Benchmarks
dotnet restore
dotnet publish --configuration Release --output bin\Release\publish
Start-Process -NoNewWindow dotnet -ArgumentList "bin\Release\publish\Benchmarks.dll", "server.urls=http://*:8080", "server=$server", "threadCount=1", "NonInteractive=true", "scenarios=$scenarios"
