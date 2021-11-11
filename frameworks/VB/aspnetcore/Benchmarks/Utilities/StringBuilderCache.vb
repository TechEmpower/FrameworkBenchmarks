' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

Imports System.Text

Friend Class StringBuilderCache

    Private Const DefaultCapacity As Integer = 1386
    Private Const MaxBuilderSize As Integer = DefaultCapacity * 3
    <ThreadStatic>
    Private Shared t_cachedInstance As StringBuilder

    Shared Function Acquire(ByVal Optional capacity As Integer = DefaultCapacity) As StringBuilder

        If capacity <= MaxBuilderSize Then
            Dim sb As StringBuilder = t_cachedInstance

            If capacity < DefaultCapacity Then
                capacity = DefaultCapacity
            End If

            If sb IsNot Nothing Then

                If capacity <= sb.Capacity Then
                    t_cachedInstance = Nothing
                    sb.Clear()
                    Return sb
                End If
            End If
        End If

        Return New StringBuilder(capacity)

    End Function

    Shared Sub Release(ByVal sb As StringBuilder)

        If sb.Capacity <= MaxBuilderSize Then
            t_cachedInstance = sb
        End If

    End Sub

    Shared Function GetStringAndRelease(ByVal sb As StringBuilder) As String

        Dim result As String = sb.ToString()
        Release(sb)
        Return result

    End Function
End Class

