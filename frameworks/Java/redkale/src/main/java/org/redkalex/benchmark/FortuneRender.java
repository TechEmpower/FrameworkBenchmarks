/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.List;
import org.redkale.convert.Convert;
import org.redkale.net.http.*;
import org.redkale.util.AnyValue;

/**
 *
 * @author zhangjx
 */
public class FortuneRender implements org.redkale.net.http.HttpRender {

    @Override
    public void init(HttpContext context, AnyValue config) {
    }

    @Override
    public void renderTo(HttpRequest request, HttpResponse response, Convert convert, HttpScope scope) {
        StringBuilder sb = new StringBuilder(1200);
        sb.append("<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>");
        for (Fortune item : (List<Fortune>) scope.find("fortunes")) {
            sb.append("<tr><td>").append(item.getId()).append("</td><td>").append(escape(item.getMessage())).append("</td></tr>");
        }
        sb.append("</table></body></html>");
        response.setContentType("text/html; charset=utf-8").finish(sb.toString());
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
