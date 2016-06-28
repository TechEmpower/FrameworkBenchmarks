package hello;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.jetty.http.HttpField;
import org.eclipse.jetty.http.HttpHeader;
import org.eclipse.jetty.http.PreEncodedHttpField;
import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.handler.AbstractHandler;
import org.eclipse.jetty.util.ajax.JSON;



public class JsonHandler extends AbstractHandler
{
    private JSON json = new JSON();
    HttpField contentType = new PreEncodedHttpField(HttpHeader.CONTENT_TYPE,"application/json");

    @Override
    public void handle(String target, Request baseRequest, HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
    {
        baseRequest.setHandled(true);
        baseRequest.getResponse().getHttpFields().add(contentType);  
        Map<String,String> map = Collections.singletonMap("message","Hello, World!");        
        json.append(response.getWriter(),map);
    }

}
