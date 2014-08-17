package hello.pages;

import hello.entities.*;

import java.util.*;
import java.util.concurrent.*;
import java.io.IOException;

import org.apache.tapestry5.*;
import org.apache.tapestry5.annotations.*;
import org.apache.tapestry5.ioc.annotations.*;
import org.apache.tapestry5.json.*;
import org.apache.tapestry5.util.*;
import org.apache.tapestry5.services.*;
import org.hibernate.*;
import com.fasterxml.jackson.databind.*;

/**
 * Database Mapping Test
 */
public class HelloDB
{
  @Inject
  private org.hibernate.Session session;

  @Inject
  private Request request;

  private static final int DB_ROWS = 10000;

  private static final ObjectMapper mapper = new ObjectMapper();

  StreamResponse onActivate() {

    // Read queries from URL, but don't bother validating
    int queries = 1;
    String qString = this.request.getParameter("queries");
    if (qString != null) {
      queries = Integer.parseInt(qString);
    }
    if (queries <= 0) {
      queries = 1;
    }
    final World[] worlds = new World[queries];

    // For generating a random row ID
    final Random rand = ThreadLocalRandom.current();

    for (int i = 0; i < queries; i++) {
      // Read object from database
      worlds[i] = (World)session.get(World.class, new Integer(rand.nextInt(DB_ROWS) + 1));
    }

    // Send reponse
    String response = "";
    try
    {
      if (queries == 1)
      {
        response = HelloDB.mapper.writeValueAsString(worlds[0]); 
      }
      else
      {
        response = HelloDB.mapper.writeValueAsString(worlds);
      }
    }
    catch (IOException ex)
    {
      // do nothing
    }
    return new TextStreamResponse("application/json", response);
  }
}
