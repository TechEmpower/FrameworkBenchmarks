using System.Runtime.InteropServices;

public unsafe partial class NativeMethods
{
    [DllImport("nativeAOT.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
    public static extern char* HelloWorld(); 
}