/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.nio.charset.StandardCharsets;
import java.util.List;
import org.redkale.convert.Convert;
import org.redkale.net.http.*;
import org.redkale.util.*;

/**
 *
 * @author zhangjx
 */
public class FortuneRender implements org.redkale.net.http.HttpRender {

    private static final String contentType = "text/html; charset=utf-8";

    private static final byte[] text1 = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>".getBytes(StandardCharsets.UTF_8);

    private static final byte[] text2 = "<tr><td>".getBytes(StandardCharsets.UTF_8);

    private static final byte[] text3 = "</td><td>".getBytes(StandardCharsets.UTF_8);

    private static final byte[] text4 = "</td></tr>".getBytes(StandardCharsets.UTF_8);

    private static final byte[] text5 = "</table></body></html>".getBytes(StandardCharsets.UTF_8);

    private final ThreadLocal<ByteArray> localByteArray = ThreadLocal.withInitial(() -> new ByteArray(1200));

    @Override
    public void init(HttpContext context, AnyValue config) {
    }

    @Override
    public void renderTo(HttpRequest request, HttpResponse response, Convert convert, HttpScope scope) {
        ByteArray array = localByteArray.get().clear();
        array.put(text1);
        for (Fortune item : (List<Fortune>) scope.getReferObj()) {
            array.put(text2).put(String.valueOf(item.getId()).getBytes(StandardCharsets.UTF_8))
                .put(text3).put(escape(item.getMessage()).toString().getBytes(StandardCharsets.UTF_8)).put(text4);
        }
        array.put(text5);
        response.finish(contentType, array);
    }

    private static CharSequence escape(CharSequence value) {
        if (value == null || value.length() == 0) return "";
        CharSequence cs = value;
        StringBuilder sb = new StringBuilder(value.length() + 16);
        for (int i = 0; i < cs.length(); i++) {
            char ch = cs.charAt(i);
            switch (ch) {
                case '<':
                    sb.append("&lt;");
                    break;
                case '>':
                    sb.append("&gt;");
                    break;
                case '"':
                    sb.append("&quot;");
                    break;
                case '\'':
                    sb.append("&#39;");
                    break;
                case '&':
                    sb.append("&amp;");
                    break;
                default:
                    sb.append(ch);
                    break;
            }
        }
        return sb;
    }
}
