using System.Data;
using System.Data.Common;
using System.Data.Odbc;
using System.Threading.Tasks;

namespace appMpower.Db
{
   public class PooledCommand : IDbCommand
   {
      private OdbcCommand _odbcCommand;
      private PooledConnection _pooledConnection;

      public PooledCommand(PooledConnection pooledConnection)
      {
         _odbcCommand = (OdbcCommand)pooledConnection.CreateCommand();
         _pooledConnection = pooledConnection;
      }

      public PooledCommand(string commandText, PooledConnection pooledConnection)
      {
         pooledConnection.GetCommand(commandText, this);
      }

      internal PooledCommand(OdbcCommand odbcCommand, PooledConnection pooledConnection)
      {
         _odbcCommand = odbcCommand;
         _pooledConnection = pooledConnection;
      }

      internal OdbcCommand OdbcCommand
      {
         get
         {
            return _odbcCommand;
         }
         set
         {
            _odbcCommand = value;
         }
      }

      internal PooledConnection PooledConnection
      {
         get
         {
            return _pooledConnection;
         }
         set
         {
            _pooledConnection = value;
         }
      }

      public string CommandText
      {
         get
         {
            return _odbcCommand.CommandText;
         }
         set
         {
            _odbcCommand.CommandText = value;
         }
      }

      public int CommandTimeout
      {
         get
         {
            return _odbcCommand.CommandTimeout;
         }
         set
         {
            _odbcCommand.CommandTimeout = value;
         }
      }
      public CommandType CommandType
      {
         get
         {
            return _odbcCommand.CommandType;
         }
         set
         {
            _odbcCommand.CommandType = value;
         }
      }

#nullable enable
      public IDbConnection? Connection
      {
         get
         {
            return _odbcCommand.Connection;
         }
         set
         {
            _odbcCommand.Connection = (OdbcConnection?)value;
         }
      }
#nullable disable


      public IDataParameterCollection Parameters
      {
         get
         {
            return _odbcCommand.Parameters;
         }
      }

#nullable enable
      public IDbTransaction? Transaction
      {
         get
         {
            return _odbcCommand.Transaction;
         }
         set
         {
            _odbcCommand.Transaction = (OdbcTransaction?)value;
         }
      }
#nullable disable

      public UpdateRowSource UpdatedRowSource
      {
         get
         {
            return _odbcCommand.UpdatedRowSource;
         }
         set
         {
            _odbcCommand.UpdatedRowSource = value;
         }
      }
      public void Cancel()
      {
         _odbcCommand.Cancel();
      }

      public IDbDataParameter CreateParameter()
      {
         return _odbcCommand.CreateParameter();
      }

      public IDbDataParameter CreateParameter(string name, DbType dbType, object value)
      {
         OdbcParameter odbcParameter = null;

         if (this.Parameters.Contains(name))
         {
            odbcParameter = (OdbcParameter)this.Parameters[name];
            odbcParameter.Value = value;
         }
         else
         {
            odbcParameter = _odbcCommand.CreateParameter();

            odbcParameter.ParameterName = name;
            odbcParameter.DbType = dbType;
            odbcParameter.Value = value;
            this.Parameters.Add(odbcParameter);
         }

         return odbcParameter;
      }

      public int ExecuteNonQuery()
      {
         return _odbcCommand.ExecuteNonQuery();
      }

      public IDataReader ExecuteReader()
      {
         return _odbcCommand.ExecuteReader();
      }

      public async Task<int> ExecuteNonQueryAsync()
      {
         return await _odbcCommand.ExecuteNonQueryAsync();
      }

      public async Task<DbDataReader> ExecuteReaderAsync(CommandBehavior behavior)
      {
         return await _odbcCommand.ExecuteReaderAsync(behavior);
      }

      public IDataReader ExecuteReader(CommandBehavior behavior)
      {
         return _odbcCommand.ExecuteReader(behavior);
      }

#nullable enable
      public object? ExecuteScalar()
      {
         return _odbcCommand.ExecuteScalar();
      }
#nullable disable

      public void Prepare()
      {
         _odbcCommand.Prepare();
      }

      public void Dispose()
      {
         _pooledConnection.ReleaseCommand(this);
      }
   }
}