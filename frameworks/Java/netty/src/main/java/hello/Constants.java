package hello;

import io.netty.util.AsciiString;
import io.netty.util.CharsetUtil;

public class Constants {

   public static final byte[] STATIC_PLAINTEXT = "Hello, World!".getBytes(CharsetUtil.UTF_8);
   public static final int STATIC_PLAINTEXT_LEN = STATIC_PLAINTEXT.length;

   public static final CharSequence PLAINTEXT_CLHEADER_VALUE = AsciiString.cached(String.valueOf(STATIC_PLAINTEXT_LEN));
   public static final int JSON_LEN = jsonLen();
   public static final CharSequence JSON_CLHEADER_VALUE = AsciiString.cached(String.valueOf(JSON_LEN));
   public static final CharSequence SERVER_NAME = AsciiString.cached("Netty");

   private static int jsonLen() {
      return JsonUtils.serializeMsg(newMsg()).length;
   }

   public static Message newMsg() {
      return new Message("Hello, World!");
   }

}
