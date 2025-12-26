package hello;

import io.netty.util.AsciiString;
import io.netty.util.CharsetUtil;

public final class Constants {

    public static final byte[] STATIC_PLAINTEXT = "Hello, World!".getBytes(CharsetUtil.UTF_8);
    public static final int STATIC_PLAINTEXT_LEN = STATIC_PLAINTEXT.length;
    public static final CharSequence PLAINTEXT_CLHEADER_VALUE =
        AsciiString.cached(String.valueOf(STATIC_PLAINTEXT_LEN));

    public static final CharSequence SERVER_NAME = AsciiString.cached("Netty");

    public static final Message STATIC_MESSAGE = new Message("Hello, World!");

    private Constants() {
    }
}
