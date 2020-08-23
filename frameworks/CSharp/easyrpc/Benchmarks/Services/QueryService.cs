using System.Threading.Tasks;
using Benchmarks.Data;
using EasyRpc.Abstractions.Path;

namespace Benchmarks.Services
{
    public class QueryService
    {
        private IRawDb _rawDb;

        public QueryService(IRawDb rawDb)
        {
            _rawDb = rawDb;
        }

        [GetMethod("/db")]
        public Task<World> Single()
        {
            return _rawDb.LoadSingleQueryRow();
        }
    }
}
