using System;
using System.Runtime.InteropServices;

public unsafe partial class NativeMethods
{
#if DEBUG
    [DllImport("nativeAOT.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("nativeAOT.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern char* HelloWorld(); 

#if DEBUG
    [DllImport("nativeAOT.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("nativeAOT.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern char* JsonMessage(); 

#if DEBUG
    [DllImport("nativeAOT.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("nativeAOT.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern byte* JsonMessage2(); 



#if DEBUG
    [DllImport("nativeAOT.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("nativeAOT.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern int JsonMessage31(int currentThreadId); 

#if DEBUG
    [DllImport("nativeAOT.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("nativeAOT.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern byte* JsonMessage32(int currentThreadId); 
}