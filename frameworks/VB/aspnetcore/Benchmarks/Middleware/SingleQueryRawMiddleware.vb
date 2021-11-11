' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 


Imports System.Runtime.CompilerServices
Imports System.Text.Json
Imports Microsoft.AspNetCore.Builder
Imports Microsoft.AspNetCore.Http
Imports Microsoft.Extensions.DependencyInjection

Public Class SingleQueryRawMiddleware
    Private ReadOnly NextStage As RequestDelegate

    Public Sub New(ByVal NextStage As RequestDelegate)
        Me.NextStage = NextStage
    End Sub

    Public Async Function Invoke(ByVal httpContext As HttpContext) As Task

        If httpContext.Request.Path.StartsWithSegments("/db", StringComparison.Ordinal) Then
            Dim db = httpContext.RequestServices.GetService(Of RawDb)()
            Dim row = Await db.LoadSingleQueryRow()
            Dim result = JsonSerializer.Serialize(row)
            httpContext.Response.StatusCode = StatusCodes.Status200OK
            httpContext.Response.ContentType = "application/json"
            httpContext.Response.ContentLength = result.Length
            Await httpContext.Response.WriteAsync(result)
            Return
        End If

        Await NextStage(httpContext)

    End Function

End Class

Module SingleQueryRawMiddlewareExtensions
    <Extension()>
    Function UseSingleQueryRaw(ByVal builder As IApplicationBuilder) As IApplicationBuilder
        Return builder.UseMiddleware(Of SingleQueryRawMiddleware)()
    End Function
End Module

