using System.Collections.Concurrent;

namespace appMpower.Orm.Data
{
   public static class DbConnections
   {
      private static bool _maxConnectionsCreated = false;
      private static short _createdConnections = 0;
      private static short _maxConnections = 500;

      private static ConcurrentStack<DbConnection> _connectionsStack = new();
      private static ConcurrentQueue<TaskCompletionSource<DbConnection>> _waitingQueue = new();

      public static async Task<DbConnection> GetConnection(string connectionString)
      {
         DbConnection dbConnection;

         if (!_connectionsStack.TryPop(out dbConnection))
         {
            if (_maxConnectionsCreated)
            {
               dbConnection = await GetDbConnectionAsync();
            }
            else
            {
               _createdConnections++;
               
               dbConnection = new DbConnection();
               dbConnection._odbcConnection = new System.Data.Odbc.OdbcConnection(connectionString);
               dbConnection._number = _createdConnections;

               if (_createdConnections == _maxConnections) _maxConnectionsCreated = true;

               //Console.WriteLine("opened connection number: " + dbConnection._number);
            }
         }

         return dbConnection;
      }

      public static Task<DbConnection> GetDbConnectionAsync()
      {
         var taskCompletionSource = new TaskCompletionSource<DbConnection>(TaskCreationOptions.RunContinuationsAsynchronously);

         _waitingQueue.Enqueue(taskCompletionSource);
         return taskCompletionSource.Task;
      }

      public static void Release(DbConnection dbConnection)
      {
         TaskCompletionSource<DbConnection> taskCompletionSource;

         if (_waitingQueue.TryDequeue(out taskCompletionSource))
         {
            taskCompletionSource.SetResult(dbConnection);
         }
         else
         {
            _connectionsStack.Push(dbConnection);
         }
      }
   }
}