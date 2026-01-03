using System.Data;
using System.Data.Common;
using System.Threading.Tasks;

namespace appMpower.Orm.Data
{
   public class DbCommand : IDbCommand
   {
      private System.Data.Common.DbCommand _dbCommand;
      private DbConnection _dbConnection;

      public DbCommand(DbConnection dbConnection)
      {
         _dbCommand = (System.Data.Common.DbCommand)dbConnection.CreateCommand();
         _dbConnection = dbConnection;
      }

      public DbCommand(string commandText, DbConnection dbConnection)
      {
         _dbCommand = dbConnection.GetCommand(commandText, CommandType.Text);
         _dbConnection = dbConnection;
      }

      public DbCommand(string commandText, CommandType commandType, DbConnection dbConnection)
      {
         _dbCommand = dbConnection.GetCommand(commandText, commandType);
         _dbConnection = dbConnection; 
      }

      internal IDbCommand Command
      {
         get
         {
            return _dbCommand;
         }
         set
         {
            _dbCommand = (System.Data.Common.DbCommand)value;
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
            _dbCommand.Connection = (System.Data.Common.DbConnection?)value;
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
            _dbCommand.Transaction = (System.Data.Common.DbTransaction?)value;
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

      public DbParameter CreateParameter(string name, object value)
      {
         return CreateParameter(name, DbType.String, value);
      }

      public DbParameter CreateParameter(string name, DbType dbType, object value)
      {
         DbParameter dbParameter;

         if (_dbCommand.Parameters.Contains(name))
         {
            dbParameter = _dbCommand.Parameters[name];
            dbParameter.Value = value;
         }
         else
         {
            dbParameter = _dbCommand.CreateParameter();

            dbParameter.ParameterName = name;
            dbParameter.DbType = dbType;
            dbParameter.Value = value;
            _dbCommand.Parameters.Add(dbParameter);
         }

         return dbParameter;
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
         return await _dbCommand.ExecuteNonQueryAsync();
      }

      public IDataReader ExecuteReader(CommandBehavior behavior)
      {
         return _dbCommand.ExecuteReader(behavior);
      }

      public async Task<System.Data.Common.DbDataReader> ExecuteReaderAsync(CommandBehavior behavior)
      {
         return await _dbCommand.ExecuteReaderAsync(behavior);
      }

#nullable enable
      public object? ExecuteScalar()
      {
         return _dbCommand.ExecuteScalar();
      }
#nullable disable

      public void Prepare()
      {
         _dbCommand.Prepare();
      }

      public void Dispose()
      {
         _dbConnection.Release(_dbCommand);
      }
   }
}