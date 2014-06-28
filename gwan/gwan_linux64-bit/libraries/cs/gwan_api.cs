// ============================================================================
// C# module for the G-WAN Web Application Server (http://trustleap.ch/)
// ----------------------------------------------------------------------------
// gwan_api.cs: exported G-WAN API calls for .NET C# servlets
// ============================================================================
using System;
using System.Runtime.CompilerServices;

public class Gwan
{
	[MethodImplAttribute(MethodImplOptions.InternalCall)]
	extern public static long getReply(string env);

	[MethodImplAttribute(MethodImplOptions.InternalCall)]
	extern public static void xbufCat(long reply, string mono_reply);

   [MethodImplAttribute(MethodImplOptions.InternalCall)]
   extern public static long cycles64();

   [MethodImplAttribute(MethodImplOptions.InternalCall)]
   extern public static long getNs   ();

   [MethodImplAttribute(MethodImplOptions.InternalCall)]
   extern public static long getUs   ();

   [MethodImplAttribute(MethodImplOptions.InternalCall)]
   extern public static long getMs   ();

   [MethodImplAttribute(MethodImplOptions.InternalCall)]
   extern public static void logErr  (long env, String msg);

   [MethodImplAttribute(MethodImplOptions.InternalCall)]
   extern public static void report  (long reply, int html_format);
}
// ============================================================================
// End of Source Code
// ============================================================================
