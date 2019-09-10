using System;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public interface IEventWork : IDisposable
    {
        Task Execute();
    }
}
