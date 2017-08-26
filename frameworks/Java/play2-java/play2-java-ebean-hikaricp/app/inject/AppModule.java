package inject;

import akka.dispatch.ExecutionContexts;
import com.google.inject.AbstractModule;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import com.google.inject.name.Named;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import play.Configuration;
import play.core.NamedThreadFactory;
import scala.concurrent.ExecutionContext;

public class AppModule extends AbstractModule {

  protected void configure() {
  }

  @Provides @Singleton @Named("dbTpe")
  public ThreadPoolExecutor provideThreadPoolExecutor(Configuration configuration) {
    int partitionCount = configuration.getInt("db.default.partitionCount");
    int maxConnections = partitionCount * configuration.getInt("db.default.maxConnectionsPerPartition");
    int minConnections = partitionCount * configuration.getInt("db.default.minConnectionsPerPartition");

    return new ThreadPoolExecutor(minConnections, maxConnections,
            0L, TimeUnit.MILLISECONDS,
            new LinkedBlockingQueue<>(),
            new NamedThreadFactory("dbEc"));
  }

  @Provides @Singleton @Named("dbEc")
  public ExecutionContext provideExecutionContext(@Named("dbTpe") ThreadPoolExecutor tpe) {
    return ExecutionContexts.fromExecutorService(tpe);
  }

}
