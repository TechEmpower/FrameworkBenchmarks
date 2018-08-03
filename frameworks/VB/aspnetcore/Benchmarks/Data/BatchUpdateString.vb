' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

Imports System.Runtime.CompilerServices

Friend Class BatchUpdateString

    Private Const MaxBatch As Integer = 500
    Private Shared ReadOnly Queries As String() = New String(499) {}

    Public Shared ReadOnly Property Strings As IList(Of BatchUpdateString) = Enumerable.Range(0, MaxBatch).[Select](Function(i) New BatchUpdateString With {
        .Id = $"Id_{i}",
        .Random = $"Random_{i}",
        .BatchSize = i
    }).ToArray()

    Private Property BatchSize As Integer
    Public Property Id As String
    Public Property Random As String

    Public ReadOnly Property UpdateQuery As String
        Get
            Return If(Queries(BatchSize), CreateQuery(BatchSize))
        End Get
    End Property

    <MethodImpl(MethodImplOptions.NoInlining)>
    Private Function CreateQuery(ByVal batchSize As Integer) As String

        Dim sb = StringBuilderCache.Acquire()

        For Each q In Enumerable.Range(0, batchSize + 1).[Select](Function(i) $"UPDATE world SET randomnumber = @Random_{i} WHERE id = @Id_{i};")
            sb.Append(q)
        Next

        Dim query = sb.ToString()
        Queries(batchSize) = query
        Return query

    End Function

    Public Shared Sub Initalize()

        Observe(Strings(0).UpdateQuery)
        Observe(Strings(4).UpdateQuery)
        Observe(Strings(9).UpdateQuery)
        Observe(Strings(14).UpdateQuery)
        Observe(Strings(19).UpdateQuery)

    End Sub

    <MethodImpl(MethodImplOptions.NoInlining)>
    Private Shared Sub Observe(ByVal query As String)
    End Sub

End Class

