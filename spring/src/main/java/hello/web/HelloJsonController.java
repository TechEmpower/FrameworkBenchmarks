package hello.web;

import java.io.*;
import java.util.*;

import javax.servlet.http.*;

import org.springframework.http.*;
import org.springframework.http.converter.*;
import org.springframework.http.converter.json.*;
import org.springframework.http.server.*;
import org.springframework.stereotype.*;
import org.springframework.web.bind.annotation.*;
 
/**
 * Handles requests for the application home page.
 */
@Controller
public class HelloJsonController {
 
  @RequestMapping(value = "/json")
  public Object json(HttpServletResponse response) 
  {
    Map<String, String> json = new HashMap<String, String>();
    json.put("message", "Hello, world");

    try {
      new MappingJackson2HttpMessageConverter().write(
          json, MediaType.APPLICATION_JSON, new ServletServerHttpResponse(response));
    } catch (HttpMessageNotWritableException e) {
        e.printStackTrace();
    } catch (IOException e) {
        e.printStackTrace();
    }
    return null;
  }
}
