package fi.markoa.tfb.servlet3;

import com.google.common.util.concurrent.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.AsyncContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * Web Framework Benchmarks
 * Test type 3: Multiple database queries
 *
 * @author marko asplund
 */
@WebServlet(urlPatterns={"/queries"}, asyncSupported=true)
public class DatabaseQueriesServlet extends DatabaseBaseServlet {
  private static final Logger LOGGER = LoggerFactory.getLogger(DatabaseQueriesServlet.class);

  @Override
  protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
    resp.setContentType(MEDIATYPE_APPLICATION_JSON);
    final AsyncContext asyncContext = req.startAsync();
    ListenableFuture<?> future = dao.read(generateRandomNumbers(getQueries(req.getParameter("queries")),
      WORLD_LEAST_VALUE, WORLD_BOUND_VALUE+1));
    addResponseCallback(asyncContext, future, executorService);
  }

}
