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
public class Query
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
      try {
        queries = Integer.parseInt(qString);
      }
      catch (Exception e) {
        queries = 1;
      }
    }
    if (queries <= 0) {
      queries = 1;
    }
    else if (queries > 500) {
      queries = 500;
    }

    // Since we're using hibernate ORM which has a 1st level cache
    // that can't be turned off, we instead pick distinct random numbers
    // to avoid failing query verification
    final World[] worlds = ThreadLocalRandom
            .current()
            .ints(1, DB_ROWS + 1)
            .distinct()
            .limit(queries)
            .mapToObj(id -> session.get(World.class, id))
            .toArray(World[]::new);

    // Send reponse
    String response = "";
    try
    {
      response = mapper.writeValueAsString(worlds);
    }
    catch (IOException ex)
    {
      // do nothing
    }
    return new TextStreamResponse("application/json", response);
  }
}
