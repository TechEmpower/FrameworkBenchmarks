using System.Threading.Tasks;
using Benchmarks.Data;
using EasyRpc.Abstractions.Path;
using EasyRpc.Abstractions.Services;

namespace Benchmarks.Services
{
    [SharedService]
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
