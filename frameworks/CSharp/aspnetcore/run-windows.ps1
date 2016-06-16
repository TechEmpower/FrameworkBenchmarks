param([string]$scenarios="[default]", [string]$server="kestrel")

$scenarios = (-split $scenarios) -join ","

if(Test-Path test)
{
  rmdir -recurse -force test
}

md test
cd test
git clone https://github.com/aspnet/benchmarks.git

cd benchmarks
. .\build.ps1
cd src/Benchmarks
dotnet build -c Release -f netcoreapp1.0
Start-Process -NoNewWindow dotnet "run -c Release server.urls=http://*:8080 server=$server threadCount=8 NonInteractive=true scenarios=$scenarios"