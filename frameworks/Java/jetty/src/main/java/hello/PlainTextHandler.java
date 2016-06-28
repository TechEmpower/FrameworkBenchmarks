package hello;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.jetty.http.HttpField;
import org.eclipse.jetty.http.HttpHeader;
import org.eclipse.jetty.http.MimeTypes;
import org.eclipse.jetty.http.PreEncodedHttpField;
import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.handler.AbstractHandler;
import org.eclipse.jetty.util.BufferUtil;

public class PlainTextHandler extends AbstractHandler
{
    ByteBuffer helloWorld = BufferUtil.toBuffer("Hello, World!");
    HttpField contentType = new PreEncodedHttpField(HttpHeader.CONTENT_TYPE,MimeTypes.Type.TEXT_PLAIN.asString());

    @Override
    public void handle(String target, Request baseRequest, HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
    {
        baseRequest.setHandled(true);
        baseRequest.getResponse().getHttpFields().add(contentType); 
        baseRequest.getResponse().getHttpOutput().sendContent(helloWorld.slice());
    }
}
