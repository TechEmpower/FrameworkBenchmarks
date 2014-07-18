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
import java.util.concurrent.ThreadLocalRandom;

/**
 * Web Framework Benchmarks
 * Test type 2: Single database query
 *
 * @author marko asplund
 */
@WebServlet(urlPatterns={"/db"}, asyncSupported=true)
public class DatabaseQueryServlet extends DatabaseBaseServlet {
  private static final Logger LOGGER = LoggerFactory.getLogger(DatabaseQueryServlet.class);

  @Override
  protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
    resp.setContentType(MEDIATYPE_APPLICATION_JSON);
    AsyncContext asyncContext = req.startAsync();
    int randId = ThreadLocalRandom.current().nextInt(WORLD_LEAST_VALUE, WORLD_BOUND_VALUE+1);
    ListenableFuture<?> future = dao.read(randId);
    addResponseCallback(asyncContext, future, executorService);
  }

}
