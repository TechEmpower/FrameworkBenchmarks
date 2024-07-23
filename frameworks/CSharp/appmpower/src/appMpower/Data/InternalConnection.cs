using System.Collections.Concurrent;
using System.Data;

namespace appMpower.Data
{
   public class InternalConnection : System.IDisposable
   {
      public short Number { get; set; }
      public IDbConnection DbConnection { get; set; }
      public ConcurrentDictionary<string, DbCommand> DbCommands { get; set; }

      public void Dispose()
      {
      }
   }
}