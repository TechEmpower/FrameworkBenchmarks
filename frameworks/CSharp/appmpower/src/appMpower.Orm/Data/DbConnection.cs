using System.Collections.Concurrent;
using System.Data;
using System.Data.Odbc;

namespace appMpower.Orm.Data
{
   public class DbConnection : IDbConnection
   {
      private string _connectionString;
      private bool _keyed = false;
      private int _number;
      private OdbcConnection _odbcConnection;
      private ConcurrentStack<OdbcCommand> _odbcCommands = new();
      private Dictionary<string, OdbcCommand> _keyedOdbcCommands;

      public DbConnection()
      {
      }

      public DbConnection(string connectionString, bool keyed = false)
      {
         _keyed = keyed;
         _connectionString = connectionString; 
         GetConnection();
      }

      public IDbConnection Connection
      {
         get
         {
            return _odbcConnection;
         }
         set
         {
            _odbcConnection = (OdbcConnection)value;
         }
      }

      public string ConnectionString
      {
         get
         {
            return _odbcConnection.ConnectionString;
         }
         set
         {
            _connectionString = value; 
            GetConnection();
         }
      }

      private void GetConnection()
      {
         if (_keyed)
         {
            (_number, _odbcConnection, _keyedOdbcCommands) = 
               DbConnectionsKeyed.GetConnectionBase(_connectionString).GetAwaiter().GetResult();
         }
         else
         {
            (_number, _odbcConnection, _odbcCommands) = 
               DbConnections.GetConnectionBase(_connectionString).GetAwaiter().GetResult();
         }
      }

      public int ConnectionTimeout
      {
         get
         {
            return _odbcConnection.ConnectionTimeout;
         }
      }

      public string Database
      {
         get
         {
            return _odbcConnection.Database;
         }
      }

      public ConnectionState State
      {
         get
         {
            if (_odbcConnection is null) return ConnectionState.Closed;
            return _odbcConnection.State;
         }
      }

      public IDbTransaction BeginTransaction()
      {
         return _odbcConnection.BeginTransaction();
      }

      public IDbTransaction BeginTransaction(IsolationLevel il)
      {
         return _odbcConnection.BeginTransaction(il);
      }

      public void ChangeDatabase(string databaseName)
      {
         _odbcConnection.ChangeDatabase(databaseName);
      }

      public void Close()
      {
         _odbcConnection.Close();
      }

      public IDbCommand CreateCommand()
      {
         return _odbcConnection.CreateCommand();
      }

      public void Open()
      {
         if (_odbcConnection.State == ConnectionState.Closed)
         {
            _odbcConnection.Open();
         }
      }

      public async Task OpenAsync()
      {
         if (_odbcConnection.State == ConnectionState.Closed)
         {
            await _odbcConnection.OpenAsync();
         }
      }

      public void Dispose()
      {
         if (_keyed)
         {
            DbConnectionsKeyed.Release((Number: _number, OdbcConnection: _odbcConnection, KeyedOdbcCommands: _keyedOdbcCommands));
         }
         else
         {
            DbConnections.Release((Number: _number, OdbcConnection: _odbcConnection, OdbcCommands: _odbcCommands));
         }
      }

      internal OdbcCommand GetCommand(string commandText, CommandType commandType)
      {
         OdbcCommand odbcCommand;

         if (_odbcCommands.TryPop(out odbcCommand))
         {
            if (commandText != odbcCommand.CommandText)
            {
               odbcCommand.CommandText = commandText; 
               odbcCommand.Parameters.Clear();
            }

            return odbcCommand; 
         }
         else if (_keyed && _keyedOdbcCommands.TryGetValue(commandText, out odbcCommand)) return odbcCommand; 

         odbcCommand = _odbcConnection.CreateCommand();
         odbcCommand.CommandText = commandText;
         odbcCommand.CommandType = commandType;
         odbcCommand.Prepare();

         return odbcCommand;
      }

      internal void Release(OdbcCommand odbcCommand)
      {
         if (_keyed) _keyedOdbcCommands.TryAdd(odbcCommand.CommandText, odbcCommand);
         else _odbcCommands.Push(odbcCommand);
      }
   }
}