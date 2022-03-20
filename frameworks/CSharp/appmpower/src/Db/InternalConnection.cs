using System.Collections.Concurrent;
using System.Data;

namespace appMpower.Db
{
   public class InternalConnection : System.IDisposable
   {
      public short Number { get; set; }
      public IDbConnection DbConnection { get; set; }
      public ConcurrentDictionary<string, PooledCommand> PooledCommands { get; set; }

      public void Dispose()
      {
      }
   }
}