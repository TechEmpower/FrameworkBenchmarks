/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.io.*;
import java.nio.ByteBuffer;
import java.util.*;
import javax.annotation.Resource;
import org.redkale.convert.json.JsonConvert;
import org.redkale.net.http.*;

/**
 *
 * @author zhangjx
 */
@WebServlet(value = {"/json", "/plaintext", "/db", "/queries", "/updates", "/fortunes"}, repair = false)
public class Servlet extends HttpServlet {

    private static final ByteBuffer helloBuffer = ByteBuffer.wrap("Hello, world!".getBytes()).asReadOnlyBuffer();

    @Resource
    private JsonConvert convert;

    @Resource
    private Service service;

    @HttpMapping(url = "/json")
    public void json(HttpRequest request, HttpResponse response) throws IOException {
        ByteBuffer[] buffers = convert.convertTo(response.getBufferSupplier(), new Message("Hello, World!"));
        response.setContentType("application/json").finish(buffers);
    }

    @HttpMapping(url = "/plaintext")
    public void plaintext(HttpRequest request, HttpResponse response) throws IOException {
        response.setContentType("text/plain").finish(helloBuffer.duplicate());
    }

    @HttpMapping(url = "/db")
    public void db(HttpRequest request, HttpResponse response) throws IOException {
        ByteBuffer[] buffers = convert.convertTo(response.getBufferSupplier(), service.findWorld());
        response.setContentType("application/json").finish(buffers);
    }

    @HttpMapping(url = "/queries")
    public void queries(HttpRequest request, HttpResponse response) throws IOException {
        int count = getQueries(request);
        service.queryWorld(count).whenComplete((obj, t) -> {
            ByteBuffer[] buffers = convert.convertTo(response.getBufferSupplier(), obj);
            response.setContentType("application/json").finish(buffers);
        });
    }

    @HttpMapping(url = "/updates")
    public void updates(HttpRequest request, HttpResponse response) throws IOException {
        int count = getQueries(request);
        service.updateWorld(count).whenComplete((obj, t) -> {
            ByteBuffer[] buffers = convert.convertTo(response.getBufferSupplier(), obj);
            response.setContentType("application/json").finish(buffers);
        });
    }

    @HttpMapping(url = "/fortunes")
    public void fortunes(HttpRequest request, HttpResponse response) throws IOException {
        List<Fortune> fortunes = service.queryFortune();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        response.setContentType("text/html; charset=UTF-8").finish(FortunesTemplate.template(fortunes).render().toString());
    }

    private static int getQueries(HttpRequest request) {
        try {
            return request.getIntParameter("queries", 1);
        } catch (Exception e) {
            return 1;
        }
    }

    public static void main(String[] args) throws Throwable {
        org.redkale.boot.Application.main(args);
//        JavaGeneratorRunnable jgr = new JavaGeneratorRunnable();
//        jgr.setTemplateDirectory(new File("D:\\Java-Projects\\FrameworkBenchmarks\\frameworks\\Java\\redkale\\src\\main\\templates"));
//        jgr.setOutputDirectory(new File("D:\\Java-Projects\\RedkaleBenchmarkProject\\src"));
//        jgr.run();
    }

}
