using System;
using System.Runtime.InteropServices;

public unsafe partial class NativeMethods
{
#if DEBUG
    [DllImport("appMpowerAot.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("appMpowerAot.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern void Dbms(int dbms); 

#if DEBUG
    [DllImport("appMpowerAot.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("appMpowerAot.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern void DbProvider(int dbProvider); 

#if DEBUG
    [DllImport("appMpowerAot.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("appMpowerAot.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern char* HelloWorld(); 

#if DEBUG
    [DllImport("appMpowerAot.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("appMpowerAot.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif
    //public static extern byte* JsonMessage(out int length);    
    public static extern IntPtr JsonMessage(out int length, out IntPtr unmanagedPointer); 

#if DEBUG
    [DllImport("appMpowerAot.dylib", CallingConvention = CallingConvention.Cdecl)]
#else
    [DllImport("appMpowerAot.so", CallingConvention = CallingConvention.Cdecl)]
#endif
    public static extern void FreeUnmanagedPointer(IntPtr byteArray);

#if DEBUG
    [DllImport("appMpowerAot.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("appMpowerAot.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    //public static extern byte* Db(out int length); 
    public static extern IntPtr Db(out int length); 
}