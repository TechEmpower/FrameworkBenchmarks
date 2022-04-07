using System.Collections.Concurrent;
using System.Data;
using System.Threading.Tasks;

namespace appMpower.Db
{
   public class PooledConnection : IDbConnection
   {
      private string _connectionString;
      internal InternalConnection _internalConnection; 

      public PooledConnection(string connectionString)
      {
         _connectionString = connectionString;
      }

      internal ConcurrentDictionary<string, PooledCommand> PooledCommands
      {
         get
         {
            return _internalConnection.PooledCommands;
         }
         set
         {
            _internalConnection.PooledCommands = value;
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

      public IDbConnection DbConnection
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
         PooledConnections.Release(_internalConnection);
      }

      public async Task OpenAsync()
      {
#if ADO
         _internalConnection = new(); 
         _internalConnection.DbConnection = new Npgsql.NpgsqlConnection(_connectionString);
#else
         if (_internalConnection is null)
         {
            _internalConnection = await PooledConnections.GetConnection(_connectionString);
         }
#endif

         if (_internalConnection.DbConnection.State == ConnectionState.Closed)
         {
            await (_internalConnection.DbConnection as System.Data.Common.DbConnection).OpenAsync();
         }
      }

      internal PooledCommand GetCommand(string commandText, CommandType commandType, PooledCommand pooledCommand)
      {
#if ADO
         pooledCommand.DbCommand = this.DbConnection.CreateCommand();
         pooledCommand.DbCommand.CommandText = commandText;
         pooledCommand.DbCommand.CommandType = commandType;
         pooledCommand.PooledConnection = this;
#else
         PooledCommand internalCommand;

         if (_internalConnection.PooledCommands.TryRemove(commandText, out internalCommand))
         {
            pooledCommand.DbCommand = internalCommand.DbCommand;
            pooledCommand.PooledConnection = internalCommand.PooledConnection;
         }
         else
         {
            pooledCommand.DbCommand = this.DbConnection.CreateCommand();
            pooledCommand.DbCommand.CommandText = commandText;
            pooledCommand.DbCommand.CommandType = commandType;
            pooledCommand.PooledConnection = this;

            //For non odbc drivers like Npgsql which do not support Prepare
            pooledCommand.DbCommand.Prepare();

            //Console.WriteLine("prepare pool connection: " + this._internalConnection.Number + " for command " + _internalConnection.PooledCommands.Count);
         }
#endif

         return pooledCommand;
      }

      public void ReleaseCommand(PooledCommand pooledCommand)
      {
#if !ADO
         _internalConnection.PooledCommands.TryAdd(pooledCommand.CommandText, pooledCommand);
#endif
      }
   }
}