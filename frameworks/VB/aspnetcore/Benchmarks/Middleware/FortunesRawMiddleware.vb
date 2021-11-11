' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

Imports System.Runtime.CompilerServices
Imports System.Text.Encodings.Web
Imports Microsoft.AspNetCore.Builder
Imports Microsoft.AspNetCore.Http
Imports Microsoft.Extensions.DependencyInjection

Public Class FortunesRawMiddleware

    Private ReadOnly NextStage As RequestDelegate
    Private ReadOnly Encoder As HtmlEncoder

    Public Sub New(ByVal NextStage As RequestDelegate, ByVal htmlEncoder As HtmlEncoder)
        Me.NextStage = NextStage
        Encoder = htmlEncoder
    End Sub

    Public Async Function Invoke(ByVal httpContext As HttpContext) As Task

        If httpContext.Request.Path.StartsWithSegments("/fortunes", StringComparison.Ordinal) Then
            Dim db = httpContext.RequestServices.GetService(Of RawDb)()
            Dim rows = Await db.LoadFortunesRows()
            Await MiddlewareHelpers.RenderFortunesHtml(rows, httpContext, Encoder)
            Return
        End If

        Await NextStage(httpContext)

    End Function
End Class

Module FortunesRawMiddlewareExtensions
    <Extension()>
    Function UseFortunesRaw(ByVal builder As IApplicationBuilder) As IApplicationBuilder
        Return builder.UseMiddleware(Of FortunesRawMiddleware)()
    End Function
End Module
