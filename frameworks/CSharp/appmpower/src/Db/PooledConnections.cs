using System.Collections.Concurrent;
using System.Data.Odbc;
using System.Threading.Tasks;

namespace appMpower.Db
{
   public static class PooledConnections
   {
      private static bool _connectionsCreated = false;
      private static short _createdConnections = 0;

#if MYSQL
      private static short _maxConnections = 500; 
#else
      private static short _maxConnections = 500;
#endif

      private static ConcurrentStack<PooledConnection> _stack = new ConcurrentStack<PooledConnection>();
      private static ConcurrentQueue<TaskCompletionSource<PooledConnection>> _waitingQueue = new ConcurrentQueue<TaskCompletionSource<PooledConnection>>();

      public static async Task<PooledConnection> GetConnection(string connectionString)
      {
         PooledConnection pooledConnection = null;

         if (_connectionsCreated)
         {
            if (_stack.TryPop(out pooledConnection))
            {
               pooledConnection.Released = false;
            }
            else
            {
               pooledConnection = await GetPooledConnectionAsync();
            }

            return pooledConnection;
         }
         else
         {
            pooledConnection = new PooledConnection();
            pooledConnection.OdbcConnection = new OdbcConnection(connectionString);
            _createdConnections++;

            if (_createdConnections == _maxConnections) _connectionsCreated = true;

            pooledConnection.Number = _createdConnections;
            pooledConnection.PooledCommands = new ConcurrentDictionary<string, PooledCommand>();
            //Console.WriteLine("opened connection number: " + pooledConnection.Number);

            return pooledConnection;
         }
      }

      public static Task<PooledConnection> GetPooledConnectionAsync()
      {
         var taskCompletionSource = new TaskCompletionSource<PooledConnection>(TaskCreationOptions.RunContinuationsAsynchronously);

         _waitingQueue.Enqueue(taskCompletionSource);
         return taskCompletionSource.Task;
      }

      public static void Dispose(PooledConnection pooledConnection)
      {
         PooledConnection newPooledConnection = new PooledConnection();

         newPooledConnection.OdbcConnection = pooledConnection.OdbcConnection;
         newPooledConnection.Number = pooledConnection.Number;
         newPooledConnection.PooledCommands = pooledConnection.PooledCommands;

         Release(newPooledConnection);
      }

      public static void Release(PooledConnection pooledConnection)
      {
         TaskCompletionSource<PooledConnection> taskCompletionSource;

         if (_waitingQueue.TryDequeue(out taskCompletionSource))
         {
            taskCompletionSource.SetResult(pooledConnection);
         }
         else
         {
            pooledConnection.Released = true;
            _stack.Push(pooledConnection);
         }
      }
   }
}
