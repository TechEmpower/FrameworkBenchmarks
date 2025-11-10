using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public static partial class NativeMethods
{
#if DEBUG
    private const string LibName = "appMpower.Orm.dylib";
#else
    private const string LibName = "appMpower.Orm.so";
#endif

    [LibraryImport(LibName, EntryPoint = "Dbms", StringMarshalling = StringMarshalling.Utf16)]
    [UnmanagedCallConv(CallConvs = new[] { typeof(CallConvCdecl) })]
    public static partial void Dbms(int dbms);

    [LibraryImport(LibName, EntryPoint = "DbProvider", StringMarshalling = StringMarshalling.Utf16)]
    [UnmanagedCallConv(CallConvs = new[] { typeof(CallConvCdecl) })]
    public static partial void DbProvider(int dbProvider);

    [LibraryImport(LibName, EntryPoint = "FreeHandlePointer")]
    [UnmanagedCallConv(CallConvs = new[] { typeof(CallConvCdecl) })]
    public static partial void FreeHandlePointer(IntPtr handlePointer);

    [LibraryImport(LibName, EntryPoint = "Db", StringMarshalling = StringMarshalling.Utf16)]
    [UnmanagedCallConv(CallConvs = new[] { typeof(CallConvCdecl) })]
    public static partial IntPtr Db(out int length, out IntPtr handlePointer);

    [LibraryImport(LibName, EntryPoint = "Fortunes", StringMarshalling = StringMarshalling.Utf16)]
    [UnmanagedCallConv(CallConvs = new[] { typeof(CallConvCdecl) })]

    public static partial IntPtr Fortunes(out int length, out IntPtr handlePointer);

    [LibraryImport(LibName, EntryPoint = "Query", StringMarshalling = StringMarshalling.Utf16)]
    [UnmanagedCallConv(CallConvs = new[] { typeof(CallConvCdecl) })]
    public static partial IntPtr Query(int queries, out int length, out IntPtr handlePointer);

    [LibraryImport(LibName, EntryPoint = "Updates", StringMarshalling = StringMarshalling.Utf16)]
    [UnmanagedCallConv(CallConvs = new[] { typeof(CallConvCdecl) })]
    public static partial IntPtr Updates(int queries, out int length, out IntPtr handlePointer);

    [LibraryImport(LibName, StringMarshalling = StringMarshalling.Utf16)]
    [UnmanagedCallConv(CallConvs = new[] { typeof(CallConvCdecl) })]
    public static partial IntPtr DbById(int id, out int length, out IntPtr handlePointer);

}