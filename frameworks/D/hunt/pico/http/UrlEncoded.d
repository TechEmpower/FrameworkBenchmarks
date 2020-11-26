module http.UrlEncoded;



import hunt.collection.List;
import hunt.collection.MultiMap;
import hunt.collection.StringBuffer;
import hunt.Exceptions;
import hunt.logging;
import hunt.text.Charset;
import hunt.text.Common;
import hunt.text.StringBuilder;
import hunt.util.TypeUtils;

import std.conv;
import std.array;


/**
 * Handles coding of MIME "x-www-form-urlencoded".
 * <p>
 * This class handles the encoding and decoding for either the query string of a
 * URL or the _content of a POST HTTP request.
 * </p>
 * <b>Notes</b>
 * <p>
 * The UTF-8 charset is assumed, unless otherwise defined by either passing a
 * parameter or setting the "org.hunt.utils.UrlEncoding.charset" System
 * property.
 * </p>
 * <p>
 * The hashtable either contains string single values, vectors of string or
 * arrays of Strings.
 * </p>
 * <p>
 * This class is only partially synchronised. In particular, simple get
 * operations are not protected from concurrent updates.
 * </p>
 *
 * @see java.net.URLEncoder
 */
class UrlEncoded  : MultiMap!string {

    enum string ENCODING = StandardCharsets.UTF_8;


    this() {
    }

    this(string query) {
        decodeTo(query, this, ENCODING);
    }

    void decode(string query) {
        decodeTo(query, this, ENCODING);
    }

    void decode(string query, string charset) {
        decodeTo(query, this, charset);
    }

    /**
     * Encode MultiMap with % encoding for UTF8 sequences.
     *
     * @return the MultiMap as a string with % encoding
     */
    string encode() {
        return encode(ENCODING, false);
    }

    /**
     * Encode MultiMap with % encoding for arbitrary string sequences.
     *
     * @param charset the charset to use for encoding
     * @return the MultiMap as a string encoded with % encodings
     */
    string encode(string charset) {
        return encode(charset, false);
    }

    /**
     * Encode MultiMap with % encoding.
     *
     * @param charset            the charset to encode with
     * @param equalsForNullValue if True, then an '=' is always used, even
     *                           for parameters without a value. e.g. <code>"blah?a=&amp;b=&amp;c="</code>.
     * @return the MultiMap as a string encoded with % encodings
     */
    string encode(string charset, bool equalsForNullValue) {
        return encode(this, charset, equalsForNullValue);
    }

    /**
     * Encode MultiMap with % encoding.
     *
     * @param map                the map to encode
     * @param charset            the charset to use for encoding (uses default encoding if null)
     * @param equalsForNullValue if True, then an '=' is always used, even
     *                           for parameters without a value. e.g. <code>"blah?a=&amp;b=&amp;c="</code>.
     * @return the MultiMap as a string encoded with % encodings.
     */
    static string encode(MultiMap!string map, string charset, bool equalsForNullValue) {
        if (charset is null)
            charset = ENCODING;

        StringBuilder result = new StringBuilder(128);
        bool delim = false;
        foreach(string key, List!string list; map)
        {
            int s = list.size();

            if (delim) {
                result.append('&');
            }

            if (s == 0) {
                result.append(encodeString(key, charset));
                if (equalsForNullValue)
                    result.append('=');
            } else {
                for (int i = 0; i < s; i++) {
                    if (i > 0)
                        result.append('&');
                    string val = list.get(i);
                    result.append(encodeString(key, charset));

                    if (val != null) {
                        if (val.length > 0) {
                            result.append('=');
                            result.append(encodeString(val, charset));
                        } else if (equalsForNullValue)
                            result.append('=');
                    } else if (equalsForNullValue)
                        result.append('=');
                }
            }
            delim = true;
        }
        return result.toString();
    }

    /**
     * Decoded parameters to Map.
     *
     * @param content the string containing the encoded parameters
     * @param map     the MultiMap to put parsed query parameters into
     * @param charset the charset to use for decoding
     */
    static void decodeTo(string content, MultiMap!string map, string charset = ENCODING) {
        if (charset.empty)
            charset = ENCODING;

        synchronized (map) {
            string key = null;
            string value = null;
            int mark = -1;
            bool encoded = false;
            for (int i = 0; i < content.length; i++) {
                char c = content[i];
                switch (c) {
                    case '&':
                        int l = i - mark - 1;
                        value = l == 0 ? "" :
                                (encoded ? decodeString(content, mark + 1, l) : content.substring(mark + 1, i));
                        mark = i;
                        encoded = false;
                        if (key != null) {
                            map.add(key, value);
                        } else if (value != null && value.length > 0) {
                            map.add(value, "");
                        }
                        key = null;
                        value = null;
                        break;
                    case '=':
                        if (key != null)
                            break;
                        key = encoded ? decodeString(content, mark + 1, i - mark - 1) : content.substring(mark + 1, i);
                        mark = i;
                        encoded = false;
                        break;
                    case '+':
                        encoded = true;
                        break;
                    case '%':
                        encoded = true;
                        break;
                    default: break;
                }
            }

            int contentLen = cast(int)content.length;

            if (key != null) {
                int l =  contentLen - mark - 1;
                value = l == 0 ? "" : (encoded ? decodeString(content, mark + 1, l) : content.substring(mark + 1));
                version(HUNT_DEBUG) tracef("key=%s, value=%s", key, value);
                map.add(key, value);
            } else if (mark < contentLen) {
                version(HUNT_DEBUG) tracef("empty value: content=%s, key=%s", content, key);
                key = encoded
                        ? decodeString(content, mark + 1, contentLen - mark - 1, charset)
                        : content.substring(mark + 1);
                if (!key.empty) {
                    map.add(key, "");
                }
            } else {
                warningf("No key found.");
            }
        }
    }

    /**
     * Decode string with % encoding.
     * This method makes the assumption that the majority of calls
     * will need no decoding.
     *
     * @param encoded the encoded string to decode
     * @return the decoded string
     */
    static string decodeString(string encoded) {
        return decodeString(encoded, 0, cast(int)encoded.length);
    }

    /**
     * Decode string with % encoding.
     * This method makes the assumption that the majority of calls
     * will need no decoding.
     *
     * @param encoded the encoded string to decode
     * @param offset  the offset in the encoded string to decode from
     * @param length  the length of characters in the encoded string to decode
     * @param charset the charset to use for decoding
     * @return the decoded string
     */
    static string decodeString(string encoded, int offset, int length, string charset = ENCODING) {
        StringBuffer buffer = null;
        warningf("decodeString ...............");
        for (int i = 0; i < length; i++) {
            char c = encoded.charAt(offset + i);
            if (c < 0 || c > 0xff) {
                if (buffer is null) {
                    buffer = new StringBuffer(length);
                    buffer.append(encoded, offset, offset + i + 1);
                } else
                    buffer.append(c);
            } else if (c == '+') {
                if (buffer is null) {
                    buffer = new StringBuffer(length);
                    buffer.append(encoded, offset, offset + i);
                }

                buffer.append(' ');
            } else if (c == '%') {
                if (buffer is null) {
                    buffer = new StringBuffer(length);
                    buffer.append(encoded, offset, offset + i);
                }

                byte[] ba = new byte[length];
                int n = 0;
                while (c >= 0 && c <= 0xff) {
                    if (c == '%') {
                        if (i + 2 < length) {
                            int o = offset + i + 1;
                            i += 3;
                           // ba[n] = cast(byte) TypeUtils.parseInt(encoded, o, 2, 16);
                            n++;
                        } else {
                            ba[n++] = cast(byte) '?';
                            i = length;
                        }
                    } else if (c == '+') {
                        ba[n++] = cast(byte) ' ';
                        i++;
                    } else {
                        ba[n++] = cast(byte) c;
                        i++;
                    }

                    if (i >= length)
                        break;
                    c = encoded.charAt(offset + i);
                }

                i--;
                buffer.append(cast(string)(ba[0 .. n]));

            } else if (buffer !is null)
                buffer.append(c);
        }

        if (buffer is null) {
            if (offset == 0 && encoded.length == length)
                return encoded;
            return encoded.substring(offset, offset + length);
        }

        return buffer.toString();
    }


    /**
     * Perform URL encoding.
     *
     * @param string the string to encode
     * @return encoded string.
     */
    static string encodeString(string string) {
        return encodeString(string, ENCODING);
    }

    /**
     * Perform URL encoding.
     *
     * @param string  the string to encode
     * @param charset the charset to use for encoding
     * @return encoded string.
     */
    static string encodeString(string str, string charset) {
        if (charset is null)
            charset = ENCODING;
        byte[] bytes = cast(byte[])str;
        // bytes = string.getBytes(charset);
        warningf("encodeString ...............");
        int len = cast(int)bytes.length;
        byte[] encoded = new byte[bytes.length * 3];
        int n = 0;
        bool noEncode = true;

        for (int i = 0; i < len; i++) {
            byte b = bytes[i];

            if (b == ' ') {
                noEncode = false;
                encoded[n++] = cast(byte) '+';
            } else if (b >= 'a' && b <= 'z' ||
                    b >= 'A' && b <= 'Z' ||
                    b >= '0' && b <= '9') {
                encoded[n++] = b;
            } else {
                noEncode = false;
                encoded[n++] = cast(byte) '%';
                byte nibble = cast(byte) ((b & 0xf0) >> 4);
                if (nibble >= 10)
                    encoded[n++] = cast(byte) ('A' + nibble - 10);
                else
                    encoded[n++] = cast(byte) ('0' + nibble);
                nibble = cast(byte) (b & 0xf);
                if (nibble >= 10)
                    encoded[n++] = cast(byte) ('A' + nibble - 10);
                else
                    encoded[n++] = cast(byte) ('0' + nibble);
            }
        }

        if (noEncode)
            return str;

        return cast(string)(encoded[0 .. n]);
    }
}
