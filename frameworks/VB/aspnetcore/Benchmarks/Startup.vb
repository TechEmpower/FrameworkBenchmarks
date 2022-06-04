' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

Imports System.Data.Common
Imports System.Text.Encodings.Web
Imports System.Text.Unicode
Imports Microsoft.AspNetCore.Builder
Imports Microsoft.AspNetCore.Hosting
Imports Microsoft.Extensions.Configuration
Imports Microsoft.Extensions.DependencyInjection

Imports MySqlConnector
Imports Npgsql

Public Class Startup

    Private Configuration As IConfiguration

    Public Sub New(configuration As IConfiguration)
        Me.Configuration = configuration
    End Sub

    Public Sub ConfigureServices(ByVal services As IServiceCollection)

        services.Configure(Of AppSettings)(Configuration)
        services.AddSingleton(Of ConcurrentRandom)()

        Dim appSettings = Configuration.[Get](Of AppSettings)()
        services.AddSingleton(appSettings)

        If appSettings.Database = DatabaseServer.PostgreSql Then
            services.AddSingleton(Of DbProviderFactory)(NpgsqlFactory.Instance)
            services.AddSingleton(Of RawDb)
        ElseIf appSettings.Database = DatabaseServer.MySql Then
            services.AddSingleton(Of DbProviderFactory)(MySqlConnectorFactory.Instance)
            services.AddSingleton(Of RawDb)
        End If

        Dim settings = New TextEncoderSettings(UnicodeRanges.BasicLatin, UnicodeRanges.Katakana, UnicodeRanges.Hiragana)
        settings.AllowCharacter("—"c)
        services.AddWebEncoders(
            Sub(options)
                options.TextEncoderSettings = settings
            End Sub)

    End Sub

    Public Sub Configure(ByVal app As IApplicationBuilder)

        app.UsePlainText()
        app.UseJson()
        app.UseUtf8Json()
        app.UseFortunesRaw()
        app.UseSingleQueryRaw()
        app.UseMultipleQueriesRaw()
        app.UseMultipleUpdatesRaw()

    End Sub
End Class