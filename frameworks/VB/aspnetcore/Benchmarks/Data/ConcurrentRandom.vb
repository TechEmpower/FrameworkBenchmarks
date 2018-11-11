' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

Imports System.Runtime.CompilerServices
Imports System.Threading

Public Class ConcurrentRandom

    Private Shared nextSeed As Integer = 0
    <ThreadStatic>
    Private Shared ThreadRandom As Random

    Private Shared ReadOnly Property Instance As Random
        Get
            Return If(ThreadRandom, CreateRandom())
        End Get
    End Property

    <MethodImpl(MethodImplOptions.NoInlining)>
    Private Shared Function CreateRandom() As Random

        ThreadRandom = New Random(Interlocked.Increment(nextSeed))
        Return ThreadRandom

    End Function

    Public Function [Next](ByVal minValue As Integer, ByVal maxValue As Integer) As Integer

        Return Instance.Next(minValue, maxValue)

    End Function

End Class
