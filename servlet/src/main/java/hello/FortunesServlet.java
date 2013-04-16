package hello;

import java.io.*;
import java.sql.*;
import java.util.*;

import javax.annotation.*;
import javax.servlet.*;
import javax.servlet.http.*;
import javax.sql.*;

/**
 * Fortunes test, returns a list of fortune cookie messages fetched from
 * a database table and then composed by server-side templates.
 */
@SuppressWarnings("serial")
public class FortunesServlet extends HttpServlet
{
  
  // Database details.
  private static final String DB_QUERY = "SELECT * FROM Fortune";

  // Database connection pool.
  @Resource(name="jdbc/hello_world")
  private DataSource mysqlDataSource;
  
  @Override
  protected void doGet(HttpServletRequest req, HttpServletResponse res)
      throws ServletException, IOException
  {
    // Set content type to JSON
    res.setHeader(Common.HEADER_CONTENT_TYPE, Common.CONTENT_TYPE_HTML);

    // Reference the data source.
    final DataSource source = mysqlDataSource;

    List<Fortune> fortunes = new ArrayList<>();
    
    try (Connection conn = source.getConnection())
    {
      try (PreparedStatement statement = conn.prepareStatement(DB_QUERY, 
          ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY))
      {
        try (ResultSet results = statement.executeQuery())
        {
          while (results.next())
          {
            fortunes.add(new Fortune(results.getInt("id"), results.getString("message")));
          }
        }
      }
    }
    catch (SQLException sqlex)
    {
      System.err.println("SQL Exception: " + sqlex);
    }
    
    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
    Collections.sort(fortunes);
    
    // Set the list of Fortunes as an attribute of the request, making it
    // available to the JSP.
    req.setAttribute("fortunes", fortunes);
    
    // Dispatch to the JSP.
    RequestDispatcher disp = req.getRequestDispatcher("/WEB-INF/jsp/fortunes.jsp");
    disp.forward(req, res);
  }
}
