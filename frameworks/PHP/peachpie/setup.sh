fw_depends dotnetcore

# dotnet-compile-php is a .NET Core 1.0.4 application
sudo apt-get install -y dotnet-dev-1.0.4

dotnet restore
dotnet run -p Server
