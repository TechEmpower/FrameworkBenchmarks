using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data;
using System.Threading.Tasks;

namespace appMpower.Orm.Data
{
   public class DbConnection : IDbConnection
   {
      private string _connectionString;
      private System.Data.Common.DbConnection _dbConnection;
#if ODBC
      private bool _keyed = false;
      private int _number;
      private ConcurrentStack<System.Data.Common.DbCommand> _dbCommands = new();
      private Dictionary<string, System.Data.Common.DbCommand> _keyedDbCommands;
#endif

      public DbConnection()
      {
      }

      public DbConnection(string connectionString, bool keyed = false)
      {
         _connectionString = connectionString;

#if ODBC
         _keyed = keyed;
         GetConnection();
#else
         _dbConnection = DbFactory.GetConnection(_connectionString); 
#endif
      }

      public IDbConnection Connection
      {
         get
         {
            return _dbConnection;
         }
         set
         {
            _dbConnection = (System.Data.Common.DbConnection)value;
         }
      }

      public string ConnectionString
      {
         get
         {
            return _dbConnection.ConnectionString;
         }
         set
         {
            _connectionString = value; 
#if ODBC
            GetConnection();
#endif
         }
      }

#if ODBC
      private void GetConnection()
      {
         if (_keyed)
         {
            (_number, _dbConnection, _keyedDbCommands) = 
               DbConnectionsKeyed.GetConnectionBase();
         }
         else
         {
            (_number, _dbConnection, _dbCommands) =
               DbConnections.GetConnectionBase();
         }
      }
#endif

      public int ConnectionTimeout
      {
         get
         {
            return _dbConnection.ConnectionTimeout;
         }
      }

      public string Database
      {
         get
         {
            return _dbConnection.Database;
         }
      }

      public ConnectionState State
      {
         get
         {
            if (_dbConnection is null) return ConnectionState.Closed;
            return _dbConnection.State;
         }
      }

      public IDbTransaction BeginTransaction()
      {
         return _dbConnection.BeginTransaction();
      }

      public IDbTransaction BeginTransaction(IsolationLevel il)
      {
         return _dbConnection.BeginTransaction(il);
      }

      public void ChangeDatabase(string databaseName)
      {
         _dbConnection.ChangeDatabase(databaseName);
      }

      public void Close()
      {
         _dbConnection.Close();
      }

      public IDbCommand CreateCommand()
      {
         return _dbConnection.CreateCommand();
      }

      public void Open()
      {
         if (_dbConnection.State == ConnectionState.Closed)
         {
            _dbConnection.Open();
         }
      }

      public async Task OpenAsync()
      {
         if (_dbConnection.State == ConnectionState.Closed)
         {
            await _dbConnection.OpenAsync();
         }
      }

      public void Dispose()
      {
#if ODBC
         if (_keyed)
         {
            DbConnectionsKeyed.Release((Number: _number, DbConnection: _dbConnection, KeyedDbCommands: _keyedDbCommands));
         }
         else
         {
            DbConnections.Release((Number: _number, DbConnection: _dbConnection, DbCommands: _dbCommands));
         }
#else
         _dbConnection.Dispose();
#endif
      }

      internal System.Data.Common.DbCommand GetCommand(string commandText, CommandType commandType)
      {
         System.Data.Common.DbCommand dbCommand;

#if ODBC 
         if (_dbCommands.TryPop(out dbCommand))
         {
            if (commandText != dbCommand.CommandText)
            {
               dbCommand.CommandText = commandText; 
               dbCommand.Parameters.Clear();
            }

            return dbCommand; 
         }
         else if (_keyed && _keyedDbCommands.TryGetValue(commandText, out dbCommand)) return dbCommand; 
#endif

         dbCommand = _dbConnection.CreateCommand();
         dbCommand.CommandText = commandText;
         dbCommand.CommandType = commandType;

#if !(ADO && POSTGRESQL)
         dbCommand.Prepare();
#endif

         return dbCommand;
      }

      internal void Release(System.Data.Common.DbCommand dbCommand)
      {
#if ODBC
         if (_keyed) _keyedDbCommands.TryAdd(dbCommand.CommandText, dbCommand);
         else _dbCommands.Push(dbCommand);
#endif
      }
   }
}