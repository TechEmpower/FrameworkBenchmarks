using System.Runtime.InteropServices;

namespace AkazawaYun.Benchmark.Aot;

public static partial class AotImport
{
    [LibraryImport("AkazawaYun.Trial.so", SetLastError = true)]
    // [LibraryImport("AkazawaYun.Trial.dll", SetLastError = true)]
    public static partial IntPtr ServerLaunch(int plat);

}
