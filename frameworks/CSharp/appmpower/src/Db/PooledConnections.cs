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
      private static byte _maxConnections = Math.Min((byte)Environment.ProcessorCount, (byte)21);
      private static ConcurrentStack<PooledConnection> _stack = new ConcurrentStack<PooledConnection>();
      private static TimeSpan _timeSpan = new TimeSpan(10); // 10 = 0.001 millisecond

      public static async Task<PooledConnection> GetConnection(string connectionString)
      {
         PooledConnection pooledConnection = null;

         if (_connectionsCreated)
         {
            while (!_stack.TryPop(out pooledConnection))
            {
               await Task.Delay(_timeSpan);
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

      public static void ReleaseConnection(PooledConnection pooledConnection)
      {
         PooledConnection stackedConnection = new PooledConnection();

         stackedConnection.OdbcConnection = pooledConnection.OdbcConnection;
         stackedConnection.Number = pooledConnection.Number;
         stackedConnection.PooledCommands = pooledConnection.PooledCommands;

         _stack.Push(stackedConnection);
      }
   }
}