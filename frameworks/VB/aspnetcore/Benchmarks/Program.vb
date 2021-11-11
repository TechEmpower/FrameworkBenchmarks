' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

Imports Microsoft.AspNetCore.Hosting
Imports Microsoft.Extensions.Configuration

Class Program

    Shared Sub Main(args As String())

        BatchUpdateString.Initalize()

        BuildWebHost(args).Run()

    End Sub

    Shared Function BuildWebHost(args As String()) As IWebHost

        Dim config = New ConfigurationBuilder() _
            .AddJsonFile("appsettings.json") _
            .AddEnvironmentVariables(prefix:="ASPNETCORE_") _
            .AddCommandLine(args) _
            .Build()

        Dim appSettings = config.Get(Of AppSettings)()
        Console.WriteLine($"Database: {appSettings.Database}")

        Dim host = New WebHostBuilder() _
            .UseConfiguration(config) _
            .UseStartup(Of Startup)() _
            .UseKestrel() _
            .Build()

        Return host

    End Function
End Class
