' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

Public Class Fortune
    Implements IComparable(Of Fortune)
    Implements IComparable

    Public Property Id As Integer
    Public Property Message As String

    Public Function CompareTo(ByVal obj As Object) As Integer Implements IComparable.CompareTo
        Return CompareTo(CType(obj, Fortune))
    End Function

    Public Function CompareTo(ByVal other As Fortune) As Integer Implements IComparable(Of Fortune).CompareTo
        Return String.CompareOrdinal(Message, other.Message)
    End Function

End Class
