using EasyRpc.Abstractions.Path;
using EasyRpc.AspNetCore.Views;
using Benchmarks.Data;
using System.Threading.Tasks;
using System.Collections.Generic;

namespace Benchmarks
{
    public class FortuneService
    {
        private IRawDb _rawDb;

        public FortuneService(IRawDb rawDb) => _rawDb = rawDb;

        [GetMethod("/Fortunes/Fortunes")]
        [ReturnView]
        public Task<List<Fortune>> Fortunes()
        {
            return _rawDb.LoadFortunesRows();
        }
    }
}