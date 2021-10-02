using System.Collections.Concurrent;
using System.Data;
using System.Data.Common;
using System.Data.Odbc;
using System.Threading.Tasks;

namespace appMpower.Db
{
   public class PooledConnection : IDbConnection
   {
      private bool _released = false;
      private short _number = 0;
      private IDbConnection _dbConnection;
      private ConcurrentDictionary<string, PooledCommand> _pooledCommands;

      internal PooledConnection()
      {
      }

      internal PooledConnection(IDbConnection dbConnection)
      {
         _dbConnection = dbConnection;
         _pooledCommands = new ConcurrentDictionary<string, PooledCommand>();
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
            return _dbConnection.State;
         }
      }

      public bool Released
      {
         get
         {
            return _released;
         }
         internal set
         {
            _released = value;
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
         _released = true;
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

      public void Release()
      {
         if (!_released && _dbConnection.State == ConnectionState.Open)
         {
            PooledConnections.Release(this);
         }
      }

      public void Dispose()
      {
         if (!_released && _dbConnection.State == ConnectionState.Open)
         {
            PooledConnections.Dispose(this);
         }
      }

      public async Task OpenAsync()
      {
         if (_dbConnection.State == ConnectionState.Closed)
         {
            await (_dbConnection as DbConnection).OpenAsync();
         }
      }

      internal PooledCommand GetCommand(string commandText, PooledCommand pooledCommand)
      {
         PooledCommand internalCommand;

         if (_pooledCommands.TryRemove(commandText, out internalCommand))
         {
            pooledCommand.DbCommand = internalCommand.DbCommand;
            pooledCommand.PooledConnection = internalCommand.PooledConnection;
         }
         else
         {
            if (DataProvider.IsOdbcConnection)
            {
               pooledCommand.DbCommand = new OdbcCommand(commandText, this.DbConnection as OdbcConnection);
               pooledCommand.DbCommand.Prepare();
            }
            else
            {
               //For future use with non odbc drivers which can be AOT compiled without reflection
               //pooledCommand.DbCommand = new NpgsqlCommand(commandText, this.DbConnection as NpgsqlConnection);
            }

            pooledCommand.PooledConnection = this;

            //Console.WriteLine("prepare pool connection: " + this._number + " for command " + _pooledCommands.Count);
         }

         return pooledCommand;
      }

      public void ReleaseCommand(PooledCommand pooledCommand)
      {
         _pooledCommands.TryAdd(pooledCommand.CommandText, pooledCommand);
      }
   }
}