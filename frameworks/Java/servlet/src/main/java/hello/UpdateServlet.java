package hello;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.util.concurrent.*;

import javax.annotation.*;
import javax.servlet.*;
import javax.servlet.http.*;
import javax.sql.*;

/**
 * Database connectivity (with a Servlet-container managed pool) test.
 */
@SuppressWarnings("serial")
public class UpdateServlet extends HttpServlet
{
  // Database details.
  private static final String DB_QUERY      = "SELECT * FROM World WHERE id = ?";
  private static final String UPDATE_QUERY  = "UPDATE World SET randomNumber = ? WHERE id = ?";
  private static final int    DB_ROWS       = 10000;

  // Database connection pool.
  @Resource(name="jdbc/hello_world")
  private DataSource mysqlDataSource;
    
  @Override
  protected void doGet(HttpServletRequest req, HttpServletResponse res)
      throws ServletException, IOException
  {
    // Set content type to JSON
    res.setHeader(Common.HEADER_CONTENT_TYPE, Common.CONTENT_TYPE_JSON);

    // Reference the data source.
    final DataSource source = mysqlDataSource;
    
    // Get the count of queries to run.
    int count = 1;
    try
    {
      count = Integer.parseInt(req.getParameter("queries"));
      
      // Bounds check.
      if (count > 500)
      {
        count = 500;
      }
      if (count < 1)
      {
        count = 1;
      }
    }
    catch (NumberFormatException nfexc)
    {
      // Do nothing.
    }

    // Fetch some rows from the database.
    final World[] worlds = new World[count];
    final Random random = ThreadLocalRandom.current();
    
    try (Connection conn = source.getConnection())
    {
      try (PreparedStatement statement = conn.prepareStatement(DB_QUERY, 
          ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY))
      {
        // Run the query the number of times requested.
        for (int i = 0; i < count; i++)
        {
          final int id = random.nextInt(DB_ROWS) + 1;
          statement.setInt(1, id);
          
          try (ResultSet results = statement.executeQuery())
          {
            if (results.next())
            {
              worlds[i] = new World(id, results.getInt("randomNumber"));

              // Update row
              try (PreparedStatement statement2 = conn.prepareStatement(UPDATE_QUERY))
              {
		worlds[i].setRandomNumber(random.nextInt(DB_ROWS) + 1);
                statement2.setInt(1, worlds[i].getRandomNumber());
                statement2.setInt(2, id);

                statement2.execute();
              }
            }
          }
        }
      }
    }
    catch (SQLException sqlex)
    {
      System.err.println("SQL Exception: " + sqlex);
    }
    
    // Write JSON encoded message to the response.
    try
    {
      Common.MAPPER.writeValue(res.getOutputStream(), worlds);
    }
    catch (IOException ioe) 
    {
      // do nothing
    }
  }
}
