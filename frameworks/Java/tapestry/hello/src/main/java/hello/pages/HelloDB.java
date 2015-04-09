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

    // For generating a random row ID
    final Random rand = ThreadLocalRandom.current();

    final World world = (World)session.get(World.class, new Integer(rand.nextInt(DB_ROWS) + 1));

    // Send reponse
    String response = "";
    try
    {
      response = HelloDB.mapper.writeValueAsString(world);
    }
    catch (IOException ex)
    {
      // do nothing
    }
    return new TextStreamResponse("application/json", response);
  }
}
