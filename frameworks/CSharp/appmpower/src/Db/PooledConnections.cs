using System.Collections.Concurrent;
using System.Threading.Tasks;

namespace appMpower.Db
{
   public static class PooledConnections
   {
      private static bool _connectionsCreated = false;
      private static short _createdConnections = 0;
      private static short _maxConnections = 240;

      private static ConcurrentStack<InternalConnection> _stack = new();
      private static ConcurrentQueue<TaskCompletionSource<InternalConnection>> _waitingQueue = new();

      public static async Task<InternalConnection> GetConnection(string connectionString)
      {
         InternalConnection internalConnection = null;

         if (_connectionsCreated)
         {
            if (!_stack.TryPop(out internalConnection))
            {
               internalConnection = await GetPooledConnectionAsync();
            }

            return internalConnection;
         }
         else
         {
            internalConnection = new InternalConnection();

#if ADO
            internalConnection.DbConnection = new Npgsql.NpgsqlConnection(connectionString);
#else
            internalConnection.DbConnection = new System.Data.Odbc.OdbcConnection(connectionString);
#endif               

            _createdConnections++;

            if (_createdConnections == _maxConnections) _connectionsCreated = true;

            internalConnection.Number = _createdConnections;
            internalConnection.PooledCommands = new ConcurrentDictionary<string, PooledCommand>();
            //Console.WriteLine("opened connection number: " + pooledConnection.Number);

            return internalConnection;
         }
      }

      public static Task<InternalConnection> GetPooledConnectionAsync()
      {
         var taskCompletionSource = new TaskCompletionSource<InternalConnection>(TaskCreationOptions.RunContinuationsAsynchronously);

         _waitingQueue.Enqueue(taskCompletionSource);
         return taskCompletionSource.Task;
      }

      public static void Dispose(PooledConnection pooledConnection)
      {
         InternalConnection internalConnection = new InternalConnection();

         internalConnection.DbConnection = pooledConnection.DbConnection;
         internalConnection.Number = pooledConnection.Number;
         internalConnection.PooledCommands = pooledConnection.PooledCommands;

         Release(internalConnection);
      }

      public static void Release(InternalConnection internalConnection)
      {
         TaskCompletionSource<InternalConnection> taskCompletionSource;

         if (_waitingQueue.TryDequeue(out taskCompletionSource))
         {
            taskCompletionSource.SetResult(internalConnection);
         }
         else
         {
            _stack.Push(internalConnection);
         }
      }
   }
}
