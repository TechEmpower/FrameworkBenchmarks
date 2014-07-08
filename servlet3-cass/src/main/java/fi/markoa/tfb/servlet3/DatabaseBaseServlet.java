package fi.markoa.tfb.servlet3;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.util.concurrent.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.AsyncContext;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Base class for Web Framework Benchmarks database test type implementations.
 *
 * @author marko asplund
 */
public abstract class DatabaseBaseServlet extends HttpServlet {
  private static final Logger LOGGER = LoggerFactory.getLogger(DatabaseBaseServlet.class);
  protected static final ObjectMapper mapper = new ObjectMapper();
  protected static final String MEDIATYPE_APPLICATION_JSON = "application/json";
  protected static final int WORLD_LEAST_VALUE = 1;
  protected static final int WORLD_BOUND_VALUE = 10000;

  protected static final ListeningExecutorService executorService =
    MoreExecutors.listeningDecorator(Executors.newCachedThreadPool());
  protected MessageDAOCassImpl dao;

  @Override
  public void init(ServletConfig config) throws ServletException {
    dao = new MessageDAOCassImpl();
    dao.init();
  }

  /**
   * callback for sending the response back to the client
   *
   * @param asyncContext Servlet asynchronous context
   * @param future ListenableFuture holding the backend response
   * @param executor ExecutorService instance for executing the ListenableFuture
   */
  protected void addResponseCallback(final AsyncContext asyncContext, ListenableFuture<?> future, Executor executor) {
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
    }, executor);
  }

  protected int getQueries(String queries) {
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

  protected List<Integer> generateRandomNumbers(int count, int least, int bound) {
    List<Integer> ids = new ArrayList<>();
    for(int cnt = 0; cnt < count; cnt++)
      ids.add(ThreadLocalRandom.current().nextInt(least, bound));
    return ids;
  }

  @Override
  public void destroy() {
    dao.destroy();
  }
}
