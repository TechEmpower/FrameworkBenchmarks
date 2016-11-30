import com.twitter.finagle.Mysql
import com.twitter.finagle.client.DefaultPool.Param
import com.twitter.finagle.mysql.Client
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.util.Duration.fromSeconds
import com.twitter.util.NullMonitor
import io.fintrospect.configuration.Host

object Database {
  def apply(dbHost: Host): Client = {
    Mysql.client
      .withCredentials("benchmarkdbuser", "benchmarkdbpass")
      .withDatabase("hello_world")
      .configured(Param(low = 0, high = 10, idleTime = fromSeconds(5 * 60), bufferSize = 0, maxWaiters = Int.MaxValue))
      .withStatsReceiver(NullStatsReceiver)
      .withMonitor(NullMonitor)
      .withTracer(NullTracer)
      .withMaxConcurrentPrepareStatements(256)
      .newRichClient(dbHost.value + ":3306")
  }
}
