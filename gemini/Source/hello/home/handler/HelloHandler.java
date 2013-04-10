package hello.home.handler;

import hello.*;
import hello.home.entity.*;

import java.util.*;
import java.util.concurrent.*;

import com.techempower.cache.*;
import com.techempower.gemini.*;
import com.techempower.gemini.path.*;
import com.techempower.gemini.path.annotation.*;

/**
 * Responds to the framework benchmarking requests for "hello, world" and
 * simple database queries.
 */
public class HelloHandler
    extends  BasicPathHandler<GhContext>
{

  private static final int DB_ROWS = 10000;
  
  private final EntityStore store;
  
  /**
   * Constructor.
   */
  public HelloHandler(GeminiApplication app)
  {
    super(app, "hllo");
    this.store = app.getStore();
  }
  
  /**
   * Return "hello world" as a JSON-encoded message.
   */
  @PathDefault
  public boolean helloworld()
  {
    return message("Hello, World!");
  }

  /**
   * Return a list of World objects as JSON, selected randomly from the World
   * table.  For consistency, we have assumed the table has 10,000 rows.
   */
  @PathSegment
  public boolean db()
  {
    final Random random = ThreadLocalRandom.current();
    final int queries = context().getInt("queries", 1, 1, 500);
    final World[] worlds = new World[queries];

    for (int i = 0; i < queries; i++)
    {
      worlds[i] = store.get(World.class, random.nextInt(DB_ROWS) + 1);
    }
    
    return json(worlds);
  }
  
  /**
   * Fetch the full list of Fortunes from the database, sort them by the
   * fortune message text, and then render the results to simple HTML using a 
   * server-side template.
   */
  @PathSegment
  public boolean fortunes()
  {
    final List<Fortune> fortunes = store.list(Fortune.class);
    Collections.sort(fortunes);
    //return mustache("fortunes", Collections.singletonMap("fortunes", fortunes));   
    return mustache("fortunes", fortunes);
  }

}
