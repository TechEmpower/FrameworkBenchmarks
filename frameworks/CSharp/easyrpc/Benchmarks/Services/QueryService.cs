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

        [GetMethod("/queries/{count}")]
        public Task<World[]> Multiple(int count = 1)
        {
            if(count < 1 )
            {
                count = 1;
            }

            if(count > 500)
            {
                count = 500;
            }

            return _rawDb.LoadMultipleQueriesRows(count);
        }

        [GetMethod("/updates/{count}")]
        public Task<World[]> Updates(int count = 1)
        {
            if(count < 1 )
            {
                count = 1;
            }

            if(count > 500)
            {
                count = 500;
            }
            
            return _rawDb.LoadMultipleUpdatesRows(count);
        }

        [GetMethod("/cached-worlds/{count}")]
        public Task<World[]> CachedWorlds(int count = 1)
        {
            if(count < 1 )
            {
                count = 1;
            }

            if(count > 500)
            {
                count = 500;
            }
            
            return _rawDb.LoadCachedQueries(count);
        }
    }
}
