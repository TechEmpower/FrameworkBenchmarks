' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

Imports System.Runtime.CompilerServices
Imports Microsoft.AspNetCore.Builder
Imports Microsoft.AspNetCore.Http
Imports Utf8Json

Public Structure JsonMessage
#Disable Warning IDE1006 ' Naming Styles
    Public Property message
#Enable Warning IDE1006 ' Naming Styles
End Structure

Public Class Utf8JsonMiddleware

    Private Const BufferSize As Integer = 27
    Private ReadOnly NextStage As RequestDelegate

    Public Sub New(ByVal NextStage As RequestDelegate)
        Me.NextStage = NextStage
    End Sub

    Public Function Invoke(ByVal httpContext As HttpContext) As Task

        If httpContext.Request.Path.StartsWithSegments("/utf8json", StringComparison.Ordinal) Then
            httpContext.Response.StatusCode = 200
            httpContext.Response.ContentType = "application/json"
            httpContext.Response.ContentLength = BufferSize
            Dim msg = New JsonMessage With {
                .message = "Hello, World!"
            }
            JsonSerializer.Serialize(httpContext.Response.Body, msg)
            Return Task.CompletedTask
        End If

        Return NextStage(httpContext)

    End Function
End Class

Module Utf8JsonMiddlewareExtensions
    <Extension()>
    Function UseUtf8Json(ByVal builder As IApplicationBuilder) As IApplicationBuilder
        Return builder.UseMiddleware(Of Utf8JsonMiddleware)()
    End Function
End Module

