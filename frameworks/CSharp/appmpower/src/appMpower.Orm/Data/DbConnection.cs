using System.Collections.Concurrent;
using System.Data;
using System.Data.Odbc; 

namespace appMpower.Orm.Data
{
   public class DbConnection : IDbConnection
   {
      private string _connectionString;
      internal bool _keyed = false; 
      internal int _number; 
      internal OdbcConnection _odbcConnection;
      internal ConcurrentStack<OdbcCommand> _odbcCommands = new();
      internal Dictionary<string, OdbcCommand> _keyedOdbcCommands;

      public DbConnection()
      {
      }

      public DbConnection(string connectionString)
      {
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
         DbConnection dbConnection = DbConnections.GetConnection(_connectionString).GetAwaiter().GetResult();
         
         _odbcConnection = dbConnection._odbcConnection;
         _odbcCommands = dbConnection._odbcCommands;
         _number = dbConnection._number; 
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
         DbConnections.Release(this);
      }

      internal OdbcCommand GetCommand(string commandText, CommandType commandType, bool keyed = false)
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
         else if (_keyed && _keyedOdbcCommands.TryGetValue(commandText, out odbcCommand))
         {
            return odbcCommand; 
         }
         else
         {
            if (!_keyed && keyed) 
            {
               _keyedOdbcCommands = new();
               _keyed = keyed; 
            }
            
            odbcCommand = _odbcConnection.CreateCommand();
            odbcCommand.CommandText = commandText;
            odbcCommand.CommandType = commandType;
            odbcCommand.Prepare();

            return odbcCommand;
         }
      }
   }
}