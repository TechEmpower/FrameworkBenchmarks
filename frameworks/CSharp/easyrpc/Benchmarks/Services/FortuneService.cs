using EasyRpc.Abstractions.Path;
using EasyRpc.AspNetCore.Views;
using Benchmarks.Data;
using System.Threading.Tasks;
using System.Collections.Generic;
using EasyRpc.Abstractions.Services;

namespace Benchmarks
{
    [SharedService]
    public class FortuneService
    {
        private IRawDb _rawDb;

        public FortuneService(IRawDb rawDb) => _rawDb = rawDb;

        [GetMethod("/Fortunes/Fortunes")]
        [ReturnView(ContentType = "text/html; charset=utf-8")]
        public Task<List<Fortune>> Fortunes()
        {
            return _rawDb.LoadFortunesRows();
        }
    }
}