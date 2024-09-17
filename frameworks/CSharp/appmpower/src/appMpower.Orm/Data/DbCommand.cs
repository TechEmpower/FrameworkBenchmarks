using System.Data;
using System.Data.Odbc; 

namespace appMpower.Orm.Data
{
   public class DbCommand : IDbCommand
   {
      private OdbcCommand _odbcCommand;
      private DbConnection _dbConnection;

      public DbCommand(DbConnection dbConnection)
      {
         _odbcCommand = (OdbcCommand)dbConnection.CreateCommand();
         _dbConnection = dbConnection;
      }

      public DbCommand(string commandText, DbConnection dbConnection)
      {
         _odbcCommand = dbConnection.GetCommand(commandText, CommandType.Text);
         _dbConnection = dbConnection;
      }

      public DbCommand(string commandText, DbConnection dbConnection, bool keyed)
      {
         _odbcCommand = dbConnection.GetCommand(commandText, CommandType.Text, keyed);
         _dbConnection = dbConnection;
      }

      public DbCommand(string commandText, CommandType commandType, DbConnection dbConnection)
      {
         _odbcCommand = dbConnection.GetCommand(commandText, commandType);
         _dbConnection = dbConnection; 
      }

      internal IDbCommand Command
      {
         get
         {
            return _odbcCommand;
         }
         set
         {
            _odbcCommand = (OdbcCommand)value;
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

      public IDbDataParameter CreateParameter(string name, object value)
      {
         return CreateParameter(name, DbType.String, value);
      }

      public IDbDataParameter CreateParameter(string name, DbType dbType, object value)
      {
         IDbDataParameter dbDataParameter;

         if (_odbcCommand.Parameters.Contains(name))
         {
            dbDataParameter = _odbcCommand.Parameters[name];
            dbDataParameter.Value = value;
         }
         else
         {
            dbDataParameter = _odbcCommand.CreateParameter();

            dbDataParameter.ParameterName = name;
            dbDataParameter.DbType = dbType;
            dbDataParameter.Value = value;
            _odbcCommand.Parameters.Add(dbDataParameter);
         }

         return dbDataParameter;
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

      public IDataReader ExecuteReader(CommandBehavior behavior)
      {
         return _odbcCommand.ExecuteReader(behavior);
      }

      public async Task<System.Data.Common.DbDataReader> ExecuteReaderAsync(CommandBehavior behavior)
      {
         return await _odbcCommand.ExecuteReaderAsync(behavior);
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
         _dbConnection.Release(_odbcCommand);
      }
   }
}