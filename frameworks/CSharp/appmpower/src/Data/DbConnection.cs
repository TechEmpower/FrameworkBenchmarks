using System.Collections.Concurrent;
using System.Data;
using System.Threading.Tasks;

namespace appMpower.Data
{
   public class DbConnection : IDbConnection
   {
      private string _connectionString;
      internal InternalConnection _internalConnection;

      public DbConnection()
      {
         _connectionString = DbProviderFactory.ConnectionString;
      }

      public DbConnection(string connectionString)
      {
         _connectionString = connectionString;
      }

      internal ConcurrentDictionary<string, DbCommand> DbCommands
      {
         get
         {
            return _internalConnection.DbCommands;
         }
         set
         {
            _internalConnection.DbCommands = value;
         }
      }

      public short Number
      {
         get
         {
            return _internalConnection.Number;
         }
         set
         {
            _internalConnection.Number = value;
         }
      }

      public IDbConnection Connection
      {
         get
         {
            return _internalConnection.DbConnection;
         }
         set
         {
            _internalConnection.DbConnection = value;
         }
      }

      public string ConnectionString
      {
         get
         {
            return _internalConnection.DbConnection.ConnectionString;
         }
         set
         {
            _internalConnection.DbConnection.ConnectionString = value;
         }
      }

      public int ConnectionTimeout
      {
         get
         {
            return _internalConnection.DbConnection.ConnectionTimeout;
         }
      }

      public string Database
      {
         get
         {
            return _internalConnection.DbConnection.Database;
         }
      }

      public ConnectionState State
      {
         get
         {
            if (_internalConnection is null) return ConnectionState.Closed;
            return _internalConnection.DbConnection.State;
         }
      }

      public IDbTransaction BeginTransaction()
      {
         return _internalConnection.DbConnection.BeginTransaction();
      }

      public IDbTransaction BeginTransaction(IsolationLevel il)
      {
         return _internalConnection.DbConnection.BeginTransaction(il);
      }

      public void ChangeDatabase(string databaseName)
      {
         _internalConnection.DbConnection.ChangeDatabase(databaseName);
      }

      public void Close()
      {
         _internalConnection.DbConnection.Close();
      }

      public async Task CloseAsync()
      {
         await (_internalConnection.DbConnection as System.Data.Common.DbConnection).CloseAsync();
      }

      public IDbCommand CreateCommand()
      {
         return _internalConnection.DbConnection.CreateCommand();
      }

      public void Open()
      {
         if (_internalConnection.DbConnection.State == ConnectionState.Closed)
         {
            _internalConnection.DbConnection.Open();
         }
      }

      public void Dispose()
      {
#if ADO
         _internalConnection.DbConnection.Dispose();
         _internalConnection.Dispose();
#else
         DbConnections.Release(_internalConnection);
#endif
      }

      public async Task OpenAsync()
      {
#if ADO && SQLSERVER
         _internalConnection = new();
         _internalConnection.DbConnection = new System.Data.SqlClient.SqlConnection(_connectionString);
#elif ADO && POSTGRESQL
         _internalConnection = new(); 
         _internalConnection.DbConnection = new Npgsql.NpgsqlConnection(_connectionString);
#else
         if (_internalConnection is null)
         {
            _internalConnection = await DbConnections.GetConnection(_connectionString);
         }
#endif

         if (_internalConnection.DbConnection.State == ConnectionState.Closed)
         {
            await (_internalConnection.DbConnection as System.Data.Common.DbConnection).OpenAsync();
         }
      }

      internal DbCommand GetCommand(string commandText, CommandType commandType, DbCommand dbCommand)
      {
#if ADO
         dbCommand.Command = _internalConnection.DbConnection.CreateCommand();
         dbCommand.Command.CommandText = commandText;
         dbCommand.Command.CommandType = commandType;
         dbCommand.DbConnection = this;
#else
         DbCommand internalCommand;

         if (_internalConnection.DbCommands.TryRemove(commandText, out internalCommand))
         {
            dbCommand.Command = internalCommand.Command;
            dbCommand.DbConnection = internalCommand.DbConnection;
         }
         else
         {
            dbCommand.Command = _internalConnection.DbConnection.CreateCommand();
            dbCommand.Command.CommandText = commandText;
            dbCommand.Command.CommandType = commandType;
            dbCommand.DbConnection = this;

            //For non odbc drivers like Npgsql which do not support Prepare
            dbCommand.Command.Prepare();

            //Console.WriteLine("prepare pool connection: " + this._internalConnection.Number + " for command " + _internalConnection.DbCommands.Count);
         }
#endif

         return dbCommand;
      }

      public void ReleaseCommand(DbCommand dbCommand)
      {
#if !ADO
         _internalConnection.DbCommands.TryAdd(dbCommand.CommandText, dbCommand);
#endif
      }
   }
}