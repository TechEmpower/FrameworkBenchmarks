package fi.markoa.tfb.servlet3;

import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.AsyncContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

/**
 * Web Framework Benchmarks
 * Test type 5: Database updates
 *
 * @author marko asplund
 */

@WebServlet(urlPatterns={"/updates"}, asyncSupported=true)
public class DatabaseUpdatesServlet extends DatabaseBaseServlet {
  private static final Logger LOGGER = LoggerFactory.getLogger(DatabaseUpdatesServlet.class);

  @Override
  protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
    resp.setContentType(MEDIATYPE_APPLICATION_JSON);
    final int queries = getQueries(req.getParameter("queries"));
    final AsyncContext asyncContext = req.startAsync();
    ListenableFuture<List<World>> readFuture = dao.read(generateRandomNumbers(queries,
      WORLD_LEAST_VALUE, WORLD_BOUND_VALUE+1));
    final ListenableFuture<List<Integer>> newRandomsFuture = generateRandomNumbersFuture(queries,
      WORLD_LEAST_VALUE, WORLD_BOUND_VALUE+1);

    Futures.addCallback(readFuture, new FutureCallback<List<World>>() {
      @Override
      public void onSuccess(List<World> worlds) {
        List<Integer> newRandoms;
        try {
          newRandoms = newRandomsFuture.get();
        } catch (InterruptedException | ExecutionException ex) {
          LOGGER.error("failed to generate random numbers", ex);
          errorDispatch(asyncContext, HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "failed to generate random numbers"+ex.getMessage());
          return;
        }
        List<World> newWorlds = new ArrayList<>();
        for(int i = 0; i < worlds.size(); i++)
          newWorlds.add(new World(worlds.get(i).getId(), newRandoms.get(i)));
        dao.update(newWorlds);

        try {
          mapper.writeValue(asyncContext.getResponse().getOutputStream(), newWorlds);
        } catch (IOException ex) {
          LOGGER.error("failed to get output stream", ex);
        }
        asyncContext.complete();

        LOGGER.debug("update done");
      }

      @Override
      public void onFailure(Throwable th) {
        LOGGER.error("update failed", th);
        errorDispatch(asyncContext, HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "update failed: "+th.getMessage());
      }
    }, executorService);

  }

  protected ListenableFuture<List<Integer>> generateRandomNumbersFuture(final int count, final int least, final int bound) {
    return executorService.submit(new Callable<List<Integer>>() {
      @Override
      public List<Integer> call() throws Exception {
        return generateRandomNumbers(count, least, bound);
      }
    });
  }

}
