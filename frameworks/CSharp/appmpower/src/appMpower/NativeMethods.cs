using System;
using System.Runtime.InteropServices;

public unsafe partial class NativeMethods
{
#if DEBUG
    [DllImport("appMpower.Orm.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("appMpower.Orm.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern void Dbms(int dbms); 

#if DEBUG
    [DllImport("appMpower.Orm.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("appMpower.Orm.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern void DbProvider(int dbProvider); 

#if DEBUG
    [DllImport("appMpower.Orm.dylib", CallingConvention = CallingConvention.Cdecl)]
#else
    [DllImport("appMpower.Orm.so", CallingConvention = CallingConvention.Cdecl)]
#endif
    public static extern void FreeHandlePointer(IntPtr handlePointer);

#if DEBUG
    [DllImport("appMpower.Orm.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("appMpower.Orm.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    //public static extern byte* Db(out int length); 
    public static extern IntPtr Db(out int length, out IntPtr handlePointer); 

#if DEBUG
    [DllImport("appMpower.Orm.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("appMpower.Orm.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern IntPtr Fortunes(out int length, out IntPtr handlePointer); 

#if DEBUG
    [DllImport("appMpower.Orm.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("appMpower.Orm.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern IntPtr Query(int queries, out int length, out IntPtr handlePointer); 

#if DEBUG
    [DllImport("appMpower.Orm.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("appMpower.Orm.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    public static extern IntPtr Updates(int queries, out int length, out IntPtr handlePointer); 

#if DEBUG
    [DllImport("appMpower.Orm.dylib", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#else
    [DllImport("appMpower.Orm.so", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
#endif   
    //public static extern byte* Db(out int length); 
    public static extern IntPtr DbById(int id, out int length, out IntPtr handlePointer); 
}
