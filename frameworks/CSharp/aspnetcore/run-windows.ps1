param([string]$scenarios="[default]", [string]$server="kestrel")

$scenarios = (-split $scenarios) -join ","

cd Benchmarks
dotnet restore
dotnet build -c Release -f netcoreapp1.0
Start-Process -NoNewWindow dotnet "run -c Release server.urls=http://*:8080 server=$server threadCount=1 NonInteractive=true scenarios=$scenarios"
