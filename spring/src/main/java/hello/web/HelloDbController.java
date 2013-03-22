package hello.web;

import hello.domain.*;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;

import javax.servlet.http.*;

import org.hibernate.Session;
import org.hibernate.Transaction;

import org.springframework.http.*;
import org.springframework.http.converter.*;
import org.springframework.http.converter.json.*;
import org.springframework.http.server.*;
import org.springframework.stereotype.*;
import org.springframework.web.bind.annotation.*;

@Controller
public class HelloDbController 
{
  private static final int    DB_ROWS                = 10000;

  @RequestMapping(value = "/db")
  public Object index(HttpServletRequest request, HttpServletResponse response, Integer queries)
  {
    if (queries == null)
    {
      queries = 1;
    }
    
    final World[] worlds = new World[queries];
    final Random random = ThreadLocalRandom.current();
    final Session session = HibernateUtil.getSessionFactory().openSession();

    for(int i = 0; i < queries; i++)
    {
      worlds[i] = (World)session.byId(World.class).load(random.nextInt(DB_ROWS) + 1);
    }

    session.close();
    
    try 
    {
      new MappingJackson2HttpMessageConverter().write(
          worlds, MediaType.APPLICATION_JSON, new ServletServerHttpResponse(response));
    } 
    catch (IOException e) 
    {
      // do nothing
    }
    
    return null;
  }
}
