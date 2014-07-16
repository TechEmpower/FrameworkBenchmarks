package hello;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Map;

import javax.activation.MimeType;
import javax.servlet.GenericServlet;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.jetty.http.MimeTypes;

public class PlaintextServlet extends GenericServlet
{
    byte[] helloWorld = "Hello, World!".getBytes(StandardCharsets.ISO_8859_1);
    @Override
    public void service(ServletRequest req, ServletResponse res) throws ServletException, IOException
    {
        HttpServletResponse response= (HttpServletResponse)res;
        response.setContentType(MimeTypes.Type.TEXT_PLAIN.asString());
        response.getOutputStream().write(helloWorld);
    }
}
