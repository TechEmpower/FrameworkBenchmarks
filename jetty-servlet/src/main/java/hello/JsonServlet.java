package hello;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;

import javax.servlet.GenericServlet;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.jetty.util.ajax.JSON;



public class JsonServlet extends GenericServlet
{
    private JSON json = new JSON();
    
    @Override
    public void service(ServletRequest req, ServletResponse res) throws ServletException, IOException
    {
        HttpServletResponse response= (HttpServletResponse)res;
        response.setContentType("application/json");
        Map<String,String> map = Collections.singletonMap("message","Hello, World!");
        
        json.append(response.getWriter(),map);
    }

}
