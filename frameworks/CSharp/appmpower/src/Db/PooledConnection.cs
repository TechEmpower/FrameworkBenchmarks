using System;
using System.Collections.Concurrent;
using System.Data;
using System.Data.Odbc;
using System.Threading.Tasks;

namespace appMpower.Db
{
   public class PooledConnection : IDbConnection
   {
      private bool _isInUse = true;
      private byte _number = 0;
      private OdbcConnection _odbcConnection;
      private ConcurrentDictionary<string, PooledCommand> _pooledCommands;

      internal PooledConnection()
      {
      }

      internal PooledConnection(OdbcConnection odbcConnection)
      {
         _odbcConnection = odbcConnection;
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

      public bool IsInUse
      {
         get
         {
            return _isInUse;
         }
         set
         {
            _isInUse = value;
         }
      }

      public byte Number
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

      public OdbcConnection OdbcConnection
      {
         get
         {
            return _odbcConnection;
         }
         set
         {
            _odbcConnection = value;
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
            _odbcConnection.ConnectionString = value;
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
         PooledConnections.ReleaseConnection(this);
         _isInUse = false;
      }

      public IDbCommand CreateCommand()
      {
         return _odbcConnection.CreateCommand();
      }

      public OdbcCommand CreateOdbcCommand()
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

      public void Dispose()
      {
         if (_isInUse && _odbcConnection.State == ConnectionState.Open)
         {
            PooledConnections.ReleaseConnection(this);
            _isInUse = false;
         }
      }

      public async Task OpenAsync()
      {
         if (_odbcConnection.State == ConnectionState.Closed)
         {
            try
            {
               await _odbcConnection.OpenAsync();
            }
            catch (Exception exception)
            {
               Console.WriteLine(exception.Message);
            }
         }
      }

      internal PooledCommand GetCommand(string commandText, PooledCommand pooledCommand)
      {
         PooledCommand internalCommand;

         if (_pooledCommands.TryRemove(commandText, out internalCommand))
         {
            pooledCommand.OdbcCommand = internalCommand.OdbcCommand;
            pooledCommand.PooledConnection = internalCommand.PooledConnection;
         }
         else
         {
            pooledCommand.OdbcCommand = new OdbcCommand(commandText, this.OdbcConnection);
            pooledCommand.PooledConnection = this;
            _pooledCommands.TryAdd(commandText, pooledCommand);

            //Console.WriteLine("prepare pool connection: " + this._number + " for command " + _pooledCommands.Count);
            pooledCommand.OdbcCommand.Prepare();
         }

         return pooledCommand;
      }

      public void ReleaseCommand(PooledCommand pooledCommand)
      {
         _pooledCommands.TryAdd(pooledCommand.CommandText, pooledCommand);
      }
   }
}