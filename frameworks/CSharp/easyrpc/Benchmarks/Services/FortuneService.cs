using EasyRpc.Abstractions.Path;
using EasyRpc.AspNetCore.Views;
using Benchmarks.Data;
using System.Threading.Tasks;

namespace Benchmarks
{
    public class FortuneService
    {
        [GetMethod("/Fortunes/Fortunes")]
        [ReturnView]
        public async Task<Fortune[]> Fortunes()
        {
            return new [] {new Fortune(1,"Hello world!")};
        }
    }
}