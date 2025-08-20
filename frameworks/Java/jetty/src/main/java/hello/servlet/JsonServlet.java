package hello.servlet;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;

import jakarta.servlet.GenericServlet;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletResponse;

import org.eclipse.jetty.util.ajax.JSON;



public class JsonServlet extends GenericServlet
{
    private final JSON json = new JSON();

    @Override
    public void service(ServletRequest req, ServletResponse res) throws IOException
    {
        HttpServletResponse response= (HttpServletResponse)res;
        response.setContentType("application/json");
        Map<String,String> map = Collections.singletonMap("message","Hello, World!");

        json.append(response.getWriter(),map);
    }
}
