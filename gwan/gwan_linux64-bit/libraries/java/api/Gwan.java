// ============================================================================
// Java API calls for the G-WAN Web App. Server (http://trustleap.ch/)
// ----------------------------------------------------------------------------
package api;

public class Gwan 
{
   public static native long getReply(long env);
   public static native void xbufCat (long ctx, String str);
   public static native long cycles64();
   public static native long getNs   ();
   public static native long getUs   ();
   public static native long getMs   ();
   public static native void logErr  (long env, String msg);
   public static native void report  (long reply, int html_format);
}
// ============================================================================
// End of Source Code
// ============================================================================

