using System; 
using System.Collections.Concurrent;
using System.Data;
using System.Data.Odbc; 
using System.Threading.Tasks;
using appMpower.Orm; 

namespace appMpower.Orm.Data
{
   public class DbConnection : IDbConnection
   {
      private string _connectionString;
      internal int _number; 
      internal OdbcConnection _odbcConnection;
      public ConcurrentStack<OdbcCommand> OdbcCommands = new();

      public DbConnection()
      {
         _connectionString = DbProviderFactory.ConnectionString;
      }

      public DbConnection(string connectionString)
      {
         _connectionString = connectionString;
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

      public async Task CloseAsync()
      {
         await (_odbcConnection as System.Data.Common.DbConnection).CloseAsync();
      }

      public IDbCommand CreateCommand()
      {
         return _odbcConnection.CreateCommand();
      }

      /*
      public void Open()
      {
         if (_odbcConnection.State == ConnectionState.Closed)
         {
            _odbcConnection.Open();
         }
      }
      */

      public void Dispose()
      {
         DbConnections.Release(this);
      }

      public void Open()
      {
         if (_odbcConnection is null)
         {
            DbConnections.GetConnection(_connectionString, this);
         }

         if (_odbcConnection.State == ConnectionState.Closed)
         {
            //Console.WriteLine("OpenAsync " + _odbcConnection.Number.ToString());
            //await (_odbcConnection as System.Data.Common.DbConnection).OpenAsync();
            _odbcConnection.Open();
         }
      }

      /*
      public async Task OpenAsync()
      {
         if (_odbcConnection is null)
         {
            //_odbcConnection = 
            await DbConnections.GetConnection(_connectionString, this);
         }

         if (_odbcConnection.State == ConnectionState.Closed)
         {
            //Console.WriteLine("OpenAsync " + _odbcConnection.Number.ToString());
            await (_odbcConnection as System.Data.Common.DbConnection).OpenAsync();
         }
      }
      */

      internal OdbcCommand GetCommand(string commandText, CommandType commandType)
      {
         OdbcCommand odbcCommand;

         if (this.OdbcCommands.TryPop(out odbcCommand))
         {
            if (commandText != odbcCommand.CommandText)
            {
               odbcCommand.CommandText = commandText; 
               odbcCommand.CommandType = commandType;

               odbcCommand.Prepare();
            }
         }
         else
         {
            odbcCommand = _odbcConnection.CreateCommand();
            odbcCommand.CommandText = commandText;
            odbcCommand.CommandType = commandType;
            //dbCommand.DbConnection = this;

            odbcCommand.Prepare();

            //Console.WriteLine("prepare pool connection: " + this._odbcConnection.Number + " for command " + _odbcConnection.DbCommands.Count);
         }

         return odbcCommand;
      }
   }
}