' Copyright (c) .NET Foundation. All rights reserved. 
' Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

Imports System.Data
Imports System.Data.Common

Public Class RawDb

    Private Shared ReadOnly WorldSortComparison As Comparison(Of World) = Function(a, b) a.Id.CompareTo(b.Id)
    Private ReadOnly Random As ConcurrentRandom
    Private ReadOnly DbProviderFactory As DbProviderFactory
    Private ReadOnly ConnectionString As String

    Public Sub New(ByVal random As ConcurrentRandom, ByVal dbProviderFactory As DbProviderFactory, ByVal appSettings As AppSettings)

        Me.Random = random
        Me.DbProviderFactory = dbProviderFactory
        ConnectionString = appSettings.ConnectionString

    End Sub

    Public Async Function LoadSingleQueryRow() As Task(Of World)

        Using db = DbProviderFactory.CreateConnection()
            db.ConnectionString = ConnectionString
            Await db.OpenAsync()

            Using cmd = CreateReadCommand(db)
                Return Await ReadSingleRow(db, cmd)
            End Using
        End Using

    End Function

    Private Async Function ReadSingleRow(ByVal connection As DbConnection, ByVal cmd As DbCommand) As Task(Of World)

        Using rdr = Await cmd.ExecuteReaderAsync(CommandBehavior.SingleRow)
            Await rdr.ReadAsync()
            Return New World With {
                .Id = rdr.GetInt32(0),
                .RandomNumber = rdr.GetInt32(1)
            }
        End Using

    End Function

    Private Function CreateReadCommand(ByVal connection As DbConnection) As DbCommand

        Dim cmd = connection.CreateCommand()
        cmd.CommandText = "SELECT id, randomnumber FROM world WHERE id = @Id"
        Dim id = cmd.CreateParameter()
        id.ParameterName = "@Id"
        id.DbType = DbType.Int32
        id.Value = Random.[Next](1, 10001)
        cmd.Parameters.Add(id)
        Return cmd

    End Function

    Public Async Function LoadMultipleQueriesRows(ByVal count As Integer) As Task(Of World())

        Using db = DbProviderFactory.CreateConnection()
            db.ConnectionString = ConnectionString
            Await db.OpenAsync()
            Return Await LoadMultipleRows(count, db)
        End Using

    End Function

    Private Async Function LoadMultipleRows(ByVal count As Integer, ByVal db As DbConnection) As Task(Of World())

        Using cmd = CreateReadCommand(db)
            cmd.Parameters("@Id").Value = Random.[Next](1, 10001)
            Dim result = New World(count - 1) {}

            For i As Integer = 0 To result.Length - 1
                result(i) = Await ReadSingleRow(db, cmd)
                cmd.Parameters("@Id").Value = Random.[Next](1, 10001)
            Next

            Return result
        End Using

    End Function

    Public Async Function LoadMultipleUpdatesRows(ByVal count As Integer) As Task(Of World())

        Using db = DbProviderFactory.CreateConnection()
            db.ConnectionString = ConnectionString
            Await db.OpenAsync()
            Dim results = Await LoadMultipleRows(count, db)
            Array.Sort(results, WorldSortComparison)

            Using updateCmd = db.CreateCommand()

                For i As Integer = 0 To results.Length - 1
                    Dim strings = BatchUpdateString.Strings(i)
                    Dim id = updateCmd.CreateParameter()
                    id.ParameterName = strings.Id
                    id.DbType = DbType.Int32
                    updateCmd.Parameters.Add(id)
                    Dim random = updateCmd.CreateParameter()
                    random.ParameterName = strings.Random
                    random.DbType = DbType.Int32
                    updateCmd.Parameters.Add(random)
                    Dim randomNumber = Me.Random.[Next](1, 10001)
                    id.Value = results(i).Id
                    random.Value = randomNumber
                    results(i).RandomNumber = randomNumber
                Next

                updateCmd.CommandText = BatchUpdateString.Strings(results.Length - 1).UpdateQuery
                Await updateCmd.ExecuteNonQueryAsync()
                Return results
            End Using
        End Using

    End Function

    Public Async Function LoadFortunesRows() As Task(Of List(Of Fortune))

        Dim result = New List(Of Fortune)()

        Using db = DbProviderFactory.CreateConnection()

            Using cmd = db.CreateCommand()
                cmd.CommandText = "SELECT id, message FROM fortune"
                db.ConnectionString = ConnectionString
                Await db.OpenAsync()

                Using rdr = Await cmd.ExecuteReaderAsync(CommandBehavior.CloseConnection)

                    While Await rdr.ReadAsync()
                        result.Add(New Fortune With {
                            .Id = rdr.GetInt32(0),
                            .Message = rdr.GetString(1)
                        })
                    End While
                End Using
            End Using
        End Using

        result.Add(New Fortune With {
            .Message = "Additional fortune added at request time."
        })
        result.Sort()
        Return result

    End Function

End Class