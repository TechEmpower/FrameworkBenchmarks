package fi.markoa.tfb.servlet3;

import com.datastax.driver.core.*;
import com.google.common.base.Function;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.ListeningExecutorService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;

/**
 * Cassandra data access implementation class for the "World" domain model.
 *
 * @author marko asplund
 */
public class MessageDAOCassImpl implements MessageDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(MessageDAOCassImpl.class);
  private static final String CONFIG_FILE_NAME= "/application.properties";
  private Cluster cluster;
  private Session session;
  private Map<String, PreparedStatement> statements;

  @Override
  public void init(ListeningExecutorService executorService) {
    LOGGER.debug("init()");

    Properties conf;
    try (InputStream is = this.getClass().getClassLoader().getResourceAsStream(CONFIG_FILE_NAME)) {
      if(is == null)
        throw new IOException("file not found: "+CONFIG_FILE_NAME);
      conf = new Properties();
      conf.load(is);
    } catch (IOException ex) {
      LOGGER.error("failed to open config file", ex);
      throw new RuntimeException(ex);
    }

    cluster = Cluster.builder()
      .addContactPoint(conf.getProperty("cassandra.host"))
//      .withCredentials(conf.getProperty("cassandra.user"), conf.getProperty("cassandra.pwd"))
      .build();
    session = cluster.connect(conf.getProperty("cassandra.keyspace"));

    Map<String, PreparedStatement> stmts = new HashMap<>();
    stmts.put("get_by_id", session.prepare("SELECT randomnumber FROM world WHERE id=?"));
    stmts.put("update_by_id", session.prepare("UPDATE world SET randomnumber=? WHERE id=?"));
    statements = Collections.unmodifiableMap(stmts);
  }

  @Override
  public ListenableFuture<World> read(final int id) {
    Function<ResultSet, World> transformation = new Function<ResultSet, World>() {
      @Override
      public World apply(ResultSet results) {
        Row r = results.one();
        return new World(id, r.getInt("randomnumber"));
      }
    };
    return Futures.transform(session.executeAsync(statements.get("get_by_id").bind(id)), transformation);
  }

  public ListenableFuture<List<World>> read(List<Integer> ids) {
    List<ListenableFuture<World>> futures = new ArrayList<>();
    for(Integer id : ids)
      futures.add(read(id));
    return Futures.allAsList(futures);
  }

  public ListenableFuture<Void> update(List<World> worlds) {
    Function<ResultSet, Void> transformation = new Function<ResultSet, Void>() {
      @Override
      public Void apply(ResultSet rows) {
        return null;
      }
    };
    BatchStatement bs = new BatchStatement(BatchStatement.Type.UNLOGGED);
    for(World w : worlds)
      bs.add(statements.get("update_by_id").bind(w.getId(), w.getRandomNumber()));
    return Futures.transform(session.executeAsync(bs), transformation);
  }

  @Override
  public void destroy() {
    LOGGER.debug("destroy()");
    session.close();
    cluster.close();
  }

}
