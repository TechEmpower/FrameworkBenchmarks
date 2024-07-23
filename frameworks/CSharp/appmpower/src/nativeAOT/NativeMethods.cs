using System.Runtime.InteropServices;
using System.Text;

namespace nativeAOT;

public static class NativeMethods
{
    [UnmanagedCallersOnly(EntryPoint = "HelloWorld")]
    public static unsafe char* HellowWorld()
    {
        string hellloWorld = "Hello, World!"; 

        fixed(char* s = hellloWorld)
        {
            return s; 
        }
    }
}
