using System.Collections.Concurrent;
using System.Threading.Tasks;

namespace appMpower.Orm.Data
{
   public static class DbConnections
   {
      //private static bool _connectionsCreated = false;
      private static short _createdConnections = 0;
      //private static short _maxConnections = 500;

      private static ConcurrentStack<DbConnection> _stack = new();
      //private static ConcurrentQueue<TaskCompletionSource<DbConnection>> _waitingQueue = new();

      public static DbConnection GetConnection(string connectionString)
      {
         DbConnection popDbConnection; 

         if (!_stack.TryPop(out popDbConnection))
         {
            popDbConnection = new DbConnection();
            popDbConnection._odbcConnection = new System.Data.Odbc.OdbcConnection(connectionString);

            _createdConnections++;

            popDbConnection._number = _createdConnections;
            Console.WriteLine("connection created: " + _createdConnections.ToString());
         }

         return popDbConnection; 
      }


      public static void GetConnection(string connectionString, DbConnection dbConnection)
      {
         DbConnection popDbConnection = null;

         if (_stack.TryPop(out popDbConnection))
         {
            dbConnection._odbcConnection = popDbConnection._odbcConnection; 
            dbConnection.OdbcCommands = popDbConnection.OdbcCommands;
            dbConnection._number = popDbConnection._number; 
         }
         else
         {
            dbConnection._odbcConnection = new System.Data.Odbc.OdbcConnection(connectionString);

            _createdConnections++;

            //if (_createdConnections == _maxConnections) _connectionsCreated = true;

            dbConnection._number = _createdConnections;
            Console.WriteLine("connection created: " + _createdConnections.ToString());
            //dbConnection.DbCommands = new ConcurrentDictionary<string, DbCommand>();
            //Console.WriteLine("opened connection number: " + dbConnection.Number);
         }
      }

      /*
      public static async Task GetConnection(string connectionString, DbConnection dbConnection)
      {
         if (_connectionsCreated)
         {
            DbConnection popDbConnection = null;

            if (!_stack.TryPop(out popDbConnection))
            {
               popDbConnection = await GetDbConnectionAsync();
            }

            dbConnection._odbcConnection = popDbConnection._odbcConnection; 
            dbConnection.OdbcCommands = popDbConnection.OdbcCommands;
            dbConnection._number = popDbConnection._number; 
         }
         else
         {
            dbConnection._odbcConnection = new System.Data.Odbc.OdbcConnection(connectionString);

            _createdConnections++;

            if (_createdConnections == _maxConnections) _connectionsCreated = true;

            dbConnection._number = _createdConnections;
            //dbConnection.DbCommands = new ConcurrentDictionary<string, DbCommand>();
            //Console.WriteLine("opened connection number: " + dbConnection.Number);
         }
      }
      */

      /*
      public static Task<DbConnection> GetDbConnectionAsync()
      {
         var taskCompletionSource = new TaskCompletionSource<DbConnection>(TaskCreationOptions.RunContinuationsAsynchronously);

         _waitingQueue.Enqueue(taskCompletionSource);
         return taskCompletionSource.Task;
      }
      */

      public static void Release(DbConnection dbConnection)
      {
         /*
         TaskCompletionSource<DbConnection> taskCompletionSource;

         if (_waitingQueue.TryDequeue(out taskCompletionSource))
         {
            taskCompletionSource.SetResult(dbConnection);
         }
         else
         {
         */
            _stack.Push(dbConnection);
         //}
      }
   }
}