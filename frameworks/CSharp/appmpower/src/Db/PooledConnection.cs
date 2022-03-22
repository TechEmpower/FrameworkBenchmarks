using System.Collections.Concurrent;
using System.Data;
using System.Threading.Tasks;

namespace appMpower.Db
{
   public class PooledConnection : IDbConnection
   {
      private short _number = 0;
      private string _connectionString;
      private IDbConnection _dbConnection;
      private ConcurrentDictionary<string, PooledCommand> _pooledCommands;

      public PooledConnection(string connectionString)
      {
         _connectionString = connectionString;
      }

      internal ConcurrentDictionary<string, PooledCommand> PooledCommands
      {
         get
         {
            return _pooledCommands;
         }
         set
         {
            _pooledCommands = value;
         }
      }

      public short Number
      {
         get
         {
            return _number;
         }
         set
         {
            _number = value;
         }
      }

      public IDbConnection DbConnection
      {
         get
         {
            return _dbConnection;
         }
         set
         {
            _dbConnection = value;
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
            _dbConnection.ConnectionString = value;
         }
      }

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

      public async Task CloseAsync()
      {
         await (_dbConnection as System.Data.Common.DbConnection).CloseAsync();
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

      public void Dispose()
      {
         PooledConnections.Dispose(this);
      }

      public async Task OpenAsync()
      {
#if ADO
         _dbConnection = new Npgsql.NpgsqlConnection(_connectionString);
#else
         if (_dbConnection is null)
         {
            using var internalConnection = await PooledConnections.GetConnection(_connectionString);

            _dbConnection = internalConnection.DbConnection;
            _number = internalConnection.Number;
            _pooledCommands = internalConnection.PooledCommands;
         }
#endif

         if (_dbConnection.State == ConnectionState.Closed)
         {
            await (_dbConnection as System.Data.Common.DbConnection).OpenAsync();
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

         if (_pooledCommands.TryRemove(commandText, out internalCommand))
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

            //Console.WriteLine("prepare pool connection: " + this._number + " for command " + _pooledCommands.Count);
         }
#endif

         return pooledCommand;
      }

      public void ReleaseCommand(PooledCommand pooledCommand)
      {
#if !ADO
         _pooledCommands.TryAdd(pooledCommand.CommandText, pooledCommand);
#endif
      }
   }
}