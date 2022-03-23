using System.Data;
using System.Threading.Tasks;

namespace appMpower.Db
{
   public class PooledCommand : IDbCommand
   {
      private IDbCommand _dbCommand;
      private PooledConnection _pooledConnection;

      public PooledCommand(PooledConnection pooledConnection)
      {
         _dbCommand = pooledConnection.CreateCommand();
         _pooledConnection = pooledConnection;
      }

      public PooledCommand(string commandText, PooledConnection pooledConnection)
      {
         pooledConnection.GetCommand(commandText, CommandType.Text, this);
      }

      public PooledCommand(string commandText, CommandType commandType, PooledConnection pooledConnection)
      {
         pooledConnection.GetCommand(commandText, commandType, this);
      }

      internal PooledCommand(IDbCommand dbCommand, PooledConnection pooledConnection)
      {
         _dbCommand = dbCommand;
         _pooledConnection = pooledConnection;
      }

      internal IDbCommand DbCommand
      {
         get
         {
            return _dbCommand;
         }
         set
         {
            _dbCommand = value;
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
            return _dbCommand.CommandText;
         }
         set
         {
            _dbCommand.CommandText = value;
         }
      }

      public int CommandTimeout
      {
         get
         {
            return _dbCommand.CommandTimeout;
         }
         set
         {
            _dbCommand.CommandTimeout = value;
         }
      }
      public CommandType CommandType
      {
         get
         {
            return _dbCommand.CommandType;
         }
         set
         {
            _dbCommand.CommandType = value;
         }
      }

#nullable enable
      public IDbConnection? Connection
      {
         get
         {
            return _dbCommand.Connection;
         }
         set
         {
            _dbCommand.Connection = (IDbConnection?)value;
         }
      }
#nullable disable


      public IDataParameterCollection Parameters
      {
         get
         {
            return _dbCommand.Parameters;
         }
      }

#nullable enable
      public IDbTransaction? Transaction
      {
         get
         {
            return _dbCommand.Transaction;
         }
         set
         {
            _dbCommand.Transaction = (IDbTransaction?)value;
         }
      }
#nullable disable

      public UpdateRowSource UpdatedRowSource
      {
         get
         {
            return _dbCommand.UpdatedRowSource;
         }
         set
         {
            _dbCommand.UpdatedRowSource = value;
         }
      }
      public void Cancel()
      {
         _dbCommand.Cancel();
      }

      public IDbDataParameter CreateParameter()
      {
         return _dbCommand.CreateParameter();
      }

      public IDbDataParameter CreateParameter(string name, object value)
      {
         return CreateParameter(name, DbType.String, value);
      }

      public IDbDataParameter CreateParameter(string name, DbType dbType, object value)
      {
         IDbDataParameter dbDataParameter = null;

         if (this.Parameters.Contains(name))
         {
            dbDataParameter = this.Parameters[name] as IDbDataParameter;
            dbDataParameter.Value = value;
         }
         else
         {
            dbDataParameter = _dbCommand.CreateParameter();

            dbDataParameter.ParameterName = name;
            dbDataParameter.DbType = dbType;
            dbDataParameter.Value = value;
            this.Parameters.Add(dbDataParameter);
         }

         return dbDataParameter;
      }

      public int ExecuteNonQuery()
      {
         return _dbCommand.ExecuteNonQuery();
      }

      public IDataReader ExecuteReader()
      {
         return _dbCommand.ExecuteReader();
      }

      public async Task<int> ExecuteNonQueryAsync()
      {
         return await (_dbCommand as System.Data.Common.DbCommand).ExecuteNonQueryAsync();
      }

      public async Task<System.Data.Common.DbDataReader> ExecuteReaderAsync(CommandBehavior behavior)
      {
         return await (_dbCommand as System.Data.Common.DbCommand).ExecuteReaderAsync(behavior);
      }

      public IDataReader ExecuteReader(CommandBehavior behavior)
      {
         return _dbCommand.ExecuteReader(behavior);
      }

#nullable enable
      public object? ExecuteScalar()
      {
         return _dbCommand.ExecuteScalar();
      }
#nullable disable

#nullable enable
      public async Task<object?> ExecuteScalarAsync()
      {
         return await ((System.Data.Common.DbCommand)_dbCommand).ExecuteScalarAsync();
      }
#nullable disable

      public void Prepare()
      {
         _dbCommand.Prepare();
      }

      public void Dispose()
      {
         _pooledConnection.ReleaseCommand(this);
      }
   }
}