using System;
using System.Collections.Concurrent;
using System.Data.Odbc;
using System.Threading.Tasks;

namespace appMpower.Db
{
   public static class PooledConnections
   {
      private static bool _connectionsCreated = false;
      private static byte _createdConnections = 0;
      private static byte _maxConnections = 255; //Math.Min((byte)Environment.ProcessorCount, (byte)21);
      private static ConcurrentStack<PooledConnection> _stack = new ConcurrentStack<PooledConnection>();
      private static ConcurrentQueue<TaskCompletionSource<PooledConnection>> _waitingQueue = new ConcurrentQueue<TaskCompletionSource<PooledConnection>>();

      public static async Task<PooledConnection> GetConnection(string connectionString)
      {
         PooledConnection pooledConnection = null;

         if (_connectionsCreated)
         {
            if (!_stack.TryPop(out pooledConnection))
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

      public static void ReleaseConnection(PooledConnection pooledConnection)
      {
         TaskCompletionSource<PooledConnection> taskCompletionSource;
         PooledConnection stackedConnection = new PooledConnection();

         stackedConnection.OdbcConnection = pooledConnection.OdbcConnection;
         stackedConnection.Number = pooledConnection.Number;
         stackedConnection.PooledCommands = pooledConnection.PooledCommands;

         if (_waitingQueue.TryDequeue(out taskCompletionSource))
         {
            taskCompletionSource.SetResult(stackedConnection);
         }
         else
         {
            _stack.Push(stackedConnection);
         }
      }
   }
}
