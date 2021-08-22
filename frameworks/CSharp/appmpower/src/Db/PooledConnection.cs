using System;
using System.Collections.Concurrent;
using System.Data;
using System.Data.Odbc;
using System.Threading.Tasks;

namespace appMpower.Db
{
   public class PooledConnection : IDbConnection
   {
      private bool _released = false;
      private short _number = 0;
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
         _released = true;
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

      public void Release()
      {
         if (!_released && _odbcConnection.State == ConnectionState.Open)
         {
            PooledConnections.Release(this);
         }
      }

      public void Dispose()
      {
         if (!_released && _odbcConnection.State == ConnectionState.Open)
         {
            PooledConnections.Dispose(this);
         }
      }

      public async Task OpenAsync()
      {
         if (_odbcConnection.State == ConnectionState.Closed)
         {
            await _odbcConnection.OpenAsync();
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
            pooledCommand.OdbcCommand.Prepare();
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