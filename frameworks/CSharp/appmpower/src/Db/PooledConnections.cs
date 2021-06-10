using System;
using System.Collections.Concurrent;
using System.Data.Odbc;
using System.Threading.Tasks;

namespace appMpower.Db
{
   public static class PooledConnections
   {
      private static int _maxLoops = 999;
      private static byte _createdConnections = 0;
      private static byte _maxConnections = Math.Min((byte)Environment.ProcessorCount, (byte)21);
      private static ConcurrentStack<PooledConnection> _stack = new ConcurrentStack<PooledConnection>();

      public static async Task<PooledConnection> GetConnection(string connectionString)
      {
         int i = 0;
         PooledConnection pooledConnection;

         if (_createdConnections < _maxConnections)
         {
            pooledConnection = new PooledConnection();
            pooledConnection.OdbcConnection = new OdbcConnection(connectionString);
            _createdConnections++;
            pooledConnection.Number = _createdConnections;
            pooledConnection.PooledCommands = new ConcurrentDictionary<string, PooledCommand>();
            //Console.WriteLine("opened connection number: " + pooledConnection.Number);

            return pooledConnection;
         }
         else
         {
            while (!_stack.TryPop(out pooledConnection) && i < _maxLoops)
            {
               if (i < 5) await Task.Delay(1);
               else if (i < 10) await Task.Delay(2);
               else if (i < 25) await Task.Delay(3);
               else if (i < 50) await Task.Delay(4);
               else if (i < 100) await Task.Delay(5);
               else if (i < 500) await Task.Delay(10);
               else await Task.Delay(20);

               i++;
               //Console.WriteLine("waiting: " + i);
            }

            if (i < _maxLoops)
            {
               //Console.WriteLine("opened connection number: " + pooledConnection.Number);
               return pooledConnection;
            }
            else
            {
               throw new Exception("No connections are available");
            }
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