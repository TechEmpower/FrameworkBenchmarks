/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import javax.annotation.Resource;
import org.redkale.net.http.*;
import org.redkale.source.DataSource;

/**
 *
 * @author zhangjx
 */
@WebServlet(value = {"/json", "/plaintext", "/db", "/queries", "/updates", "/fortunes"}, repair = false)
public class Servlet extends HttpServlet {

    private static final ByteBuffer helloBuffer = ByteBuffer.wrap("Hello, world!".getBytes()).asReadOnlyBuffer();

    @Resource
    private DataSource source;

    @HttpMapping(url = "/json")
    public void json(HttpRequest request, HttpResponse response) throws IOException {
        response.finishJson(new Message("Hello, World!"));
    }

    @HttpMapping(url = "/plaintext")
    public void plaintext(HttpRequest request, HttpResponse response) throws IOException {
        response.setContentType("text/plain").finish(helloBuffer.duplicate());
    }

    @HttpMapping(url = "/db")
    public void db(HttpRequest request, HttpResponse response) throws IOException {
        response.finishJson(source.find(World.class, randomId()));
    }

    @HttpMapping(url = "/queries")
    public void queries(HttpRequest request, HttpResponse response) throws IOException {
        int count = request.getIntParameter("queries", 1);
        count = Math.min(500, Math.max(1, count));
        World[] rs = new World[count];
        for (int i = 0; i < count; i++) {
            rs[i] = source.find(World.class, randomId());
        }
        response.finishJson((Object) rs);
    }

    @HttpMapping(url = "/updates")
    public void updates(HttpRequest request, HttpResponse response) throws IOException {
        int count = request.getIntParameter("queries", 1);
        count = Math.min(500, Math.max(1, count));
        World[] rs = new World[count];
        for (int i = 0; i < count; i++) {
            rs[i] = source.find(World.class, randomId());
            rs[i].setRandomNumber(randomId());
        }
        source.update(rs);
        response.finishJson((Object) rs);
    }

    @HttpMapping(url = "/fortunes")
    public void fortunes(HttpRequest request, HttpResponse response) throws IOException {
        List<Fortune> fortunes = source.queryList(Fortune.class);
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        response.setContentType("text/html; charset=UTF-8").finish(FortunesTemplate.template(fortunes).render().toString());
    }

    private static int randomId() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }
}
