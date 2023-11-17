package org.smartboot.servlet;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

/**
 * @author 三刀（zhengjunweimail@163.com）
 * @version V1.0 , 2020/12/22
 */
public class HelloWorldServlet extends HttpServlet {
    private final static byte[] BODY = "Hello, World!".getBytes();

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        resp.setContentLength(BODY.length);
        resp.setContentType("text/plain; charset=UTF-8");
        resp.setBufferSize(0);
        resp.getOutputStream().write(BODY);
    }
}
