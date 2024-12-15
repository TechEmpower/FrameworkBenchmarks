/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.function.Function;
import org.redkale.convert.Convert;
import org.redkale.net.http.*;
import org.redkale.util.*;

/**
 *
 * @author zhangjx
 */
public class FortuneRender implements org.redkale.net.http.HttpRender {

    private static final String contentType = "text/html; charset=utf-8";

    private static final byte[] text1 =
            "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
                    .getBytes(StandardCharsets.UTF_8);

    private static final byte[] text2 = "<tr><td>".getBytes(StandardCharsets.UTF_8);

    private static final byte[] text3 = "</td><td>".getBytes(StandardCharsets.UTF_8);

    private static final byte[] text4 = "</td></tr>".getBytes(StandardCharsets.UTF_8);

    private static final byte[] text5 = "</table></body></html>".getBytes(StandardCharsets.UTF_8);

    private static final String arrayName = "fortuneByteArray";

    private static final Function<String, ByteArray> arrayFunc = s -> new ByteArray(1280);

    private byte[][] idBytesCache;

    private byte[][] escapeCache;

    @Override
    public void init(HttpContext context, AnyValue config) {
        escapeCache = new byte[100][];
        escapeCache['<'] = "&lt;".getBytes();
        escapeCache['>'] = "&gt;".getBytes();
        escapeCache['"'] = "&quot;".getBytes();
        escapeCache['\''] = "&#39;".getBytes();
        escapeCache['&'] = "&amp;".getBytes();
        idBytesCache = new byte[100][];
        for (int i = 0; i < idBytesCache.length; i++) {
            idBytesCache[i] = String.valueOf(i).getBytes(StandardCharsets.UTF_8);
        }
    }

    @Override
    public void renderTo(HttpRequest request, HttpResponse response, Convert convert, HttpScope scope) {
        ByteArray array = request.getSubobjectIfAbsent(arrayName, arrayFunc).clear();
        array.put(text1);
        for (Fortune item : (List<Fortune>) scope.getReferObj()) {
            ByteArray msg = request.getSubobjectIfAbsent(item.getMessage(), this::escapeMessage);
            array.put(text2).put(escapeId(item.getId())).put(text3).put(msg).put(text4);
        }
        array.put(text5);
        response.finish(contentType, array);
    }

    private byte[] escapeId(int id) {
        if (id >= 0 && id < idBytesCache.length) {
            return idBytesCache[id];
        } else {
            return String.valueOf(id).getBytes(StandardCharsets.UTF_8);
        }
    }

    private ByteArray escapeMessage(String message) {
        ByteArray array = new ByteArray(128);
        CharSequence cs = message;
        for (int i = 0; i < cs.length(); i++) {
            char c = cs.charAt(i);
            byte[] bs = c < escapeCache.length ? escapeCache[c] : null;
            if (bs != null) {
                array.put(bs);
            } else if (c < 0x80) {
                array.put((byte) c);
            } else if (c < 0x800) {
                array.put((byte) (0xc0 | (c >> 6)), (byte) (0x80 | (c & 0x3f)));
            } else {
                array.put((byte) (0xe0 | (c >> 12)), (byte) (0x80 | ((c >> 6) & 0x3f)), (byte) (0x80 | (c & 0x3f)));
            }
        }
        return array;
    }
}
