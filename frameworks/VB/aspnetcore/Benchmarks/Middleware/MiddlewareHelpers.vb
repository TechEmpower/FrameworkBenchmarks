' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

Imports System.Globalization
Imports System.Text
Imports System.Text.Encodings.Web
Imports Microsoft.AspNetCore.Http

Class MiddlewareHelpers
    Shared Function GetMultipleQueriesQueryCount(ByVal httpContext As HttpContext) As Integer

        Dim queries = 1
        Dim queriesRaw = httpContext.Request.Query("queries")

        If queriesRaw.Count = 1 Then
            Integer.TryParse(queriesRaw, queries)
        End If

        Return If(queries > 500, 500, If(queries > 0, queries, 1))

    End Function

    Shared Async Function RenderFortunesHtml(ByVal model As IEnumerable(Of Fortune), ByVal httpContext As HttpContext, ByVal htmlEncoder As HtmlEncoder) As Task

        httpContext.Response.StatusCode = StatusCodes.Status200OK
        httpContext.Response.ContentType = "text/html; charset=UTF-8"
        Dim sb = StringBuilderCache.Acquire()
        sb.Append("<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>")

        For Each item In model
            sb.Append("<tr><td>")
            sb.Append(item.Id.ToString(CultureInfo.InvariantCulture))
            sb.Append("</td><td>")
            sb.Append(htmlEncoder.Encode(item.Message))
            sb.Append("</td></tr>")
        Next

        sb.Append("</table></body></html>")
        Dim response = StringBuilderCache.GetStringAndRelease(sb)
        httpContext.Response.ContentLength = Encoding.UTF8.GetByteCount(response)
        Await httpContext.Response.WriteAsync(response)

    End Function
End Class