' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

Imports System.Runtime.CompilerServices
Imports System.Text
Imports Microsoft.AspNetCore.Builder
Imports Microsoft.AspNetCore.Http

Public Class PlaintextMiddleware

    Private Shared ReadOnly HelloWorldPayload As Byte() = Encoding.UTF8.GetBytes("Hello, World!")
    Private ReadOnly NextStage As RequestDelegate

    Public Sub New(ByVal NextStage As RequestDelegate)
        Me.NextStage = NextStage
    End Sub

    Public Function Invoke(ByVal httpContext As HttpContext) As Task

        If httpContext.Request.Path.StartsWithSegments("/plaintext", StringComparison.Ordinal) Then
            Return WriteResponse(httpContext.Response)
        End If

        Return NextStage(httpContext)

    End Function

    Public Shared Function WriteResponse(ByVal response As HttpResponse) As Task

        Dim payloadLength = HelloWorldPayload.Length
        response.StatusCode = 200
        response.ContentType = "text/plain"
        response.ContentLength = payloadLength
        Return response.Body.WriteAsync(HelloWorldPayload, 0, payloadLength)

    End Function

End Class

Module PlaintextMiddlewareExtensions
    <Extension()>
    Function UsePlainText(ByVal builder As IApplicationBuilder) As IApplicationBuilder
        Return builder.UseMiddleware(Of PlaintextMiddleware)()
    End Function
End Module
