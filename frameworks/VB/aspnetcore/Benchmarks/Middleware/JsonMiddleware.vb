' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

Imports System.IO
Imports System.Runtime.CompilerServices
Imports System.Text
Imports Microsoft.AspNetCore.Builder
Imports Microsoft.AspNetCore.Http
Imports Newtonsoft.Json

Public Class JsonMiddleware

    Private Shared ReadOnly Json As JsonSerializer = New JsonSerializer()
    Private Shared ReadOnly Encoding As UTF8Encoding = New UTF8Encoding(False)
    Private Const BufferSize As Integer = 27
    Private ReadOnly NextStage As RequestDelegate

    Public Sub New(ByVal NextStage As RequestDelegate)
        Me.NextStage = NextStage
    End Sub

    Public Function Invoke(ByVal httpContext As HttpContext) As Task

        If httpContext.Request.Path.StartsWithSegments("/json", StringComparison.Ordinal) Then
            httpContext.Response.StatusCode = 200
            httpContext.Response.ContentType = "application/json"
            httpContext.Response.ContentLength = BufferSize

            Using sw = New StreamWriter(httpContext.Response.Body, Encoding, bufferSize:=BufferSize)
                Json.Serialize(sw, New With {
                    .message = "Hello, World!"
                })
            End Using

            Return Task.CompletedTask
        End If

        Return NextStage(httpContext)

    End Function
End Class

Module JsonMiddlewareExtensions
    <Extension()>
    Function UseJson(ByVal builder As IApplicationBuilder) As IApplicationBuilder
        Return builder.UseMiddleware(Of JsonMiddleware)()
    End Function
End Module
