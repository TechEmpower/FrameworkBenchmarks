package fi.markoa.tfb.servlet3;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.util.concurrent.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.AsyncContext;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Web Framework Benchmarks
 * Test type 2: Single database query
 * Test type 3: Multiple database queries
 *
 * @author marko asplund
 */
@WebServlet(urlPatterns={"/db", "/queries"}, asyncSupported=true)
public class DatabaseQueryServlet extends HttpServlet {
  private static final Logger LOGGER = LoggerFactory.getLogger(DatabaseQueryServlet.class);
  private static final ObjectMapper mapper = new ObjectMapper();
  private static final String MEDIATYPE_APPLICATION_JSON = "application/json";

  private static final ListeningExecutorService executorService =
    MoreExecutors.listeningDecorator(Executors.newCachedThreadPool());
  private MessageDAOCassImpl dao;

  @Override
  public void init(ServletConfig config) throws ServletException {
    dao = new MessageDAOCassImpl();
    dao.init();
  }

  @Override
  protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
    resp.setContentType(MEDIATYPE_APPLICATION_JSON);
    final AsyncContext asyncContext = req.startAsync();
    ListenableFuture<?> future;
    if("/queries".equals(req.getServletPath())) {
      List<Integer> ids = new ArrayList<>();
      int queries = getQueries(req.getParameter("queries"));
      for(int cnt = 0; cnt < queries; cnt++)
        ids.add(ThreadLocalRandom.current().nextInt(1, 10001));
      future = dao.read(ids);
    } else {
      int randId = ThreadLocalRandom.current().nextInt(1, 10001);
      future = dao.read(randId);
    }

    Futures.addCallback(future, new FutureCallback<Object>() {
      @Override
      public void onSuccess(Object world) {
        try {
          mapper.writeValue(asyncContext.getResponse().getOutputStream(), world);
        } catch (IOException ex) {
          LOGGER.error("failed to get output stream", ex);
          throw new RuntimeException("failed to get output stream", ex);
        }
        asyncContext.complete();
      }

      @Override
      public void onFailure(Throwable th) {
        // TODO
        LOGGER.error("failed to get data, "+th);
        asyncContext.complete();
        throw new RuntimeException(th);
      }
    }, executorService);

  }

  private int getQueries(String queries) {
    int q;
    if(queries == null) {
      return 1;
    }
    try {
      q = Integer.parseInt(queries);
    } catch (NumberFormatException ex) {
      return 1;
    }
    if(q > 500)
      return 500;
    if(q < 1)
      return 1;

    return q;
  }

  @Override
  public void destroy() {
    dao.destroy();
  }
}
