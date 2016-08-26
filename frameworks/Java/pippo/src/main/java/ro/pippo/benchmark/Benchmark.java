package ro.pippo.benchmark;

import ro.pippo.benchmark.app.BenchmarkApplication;
import ro.pippo.core.AbstractWebServer;
import ro.pippo.core.Pippo;

public class Benchmark {

  public static String SERVER_NAME;

  private Pippo pippo;

  public Benchmark() {
    BenchmarkApplication app = new BenchmarkApplication();
    pippo = new Pippo(app);
  }

  public Benchmark serverName(String serverName) {
    SERVER_NAME = serverName;
    return this;
  }

  public Benchmark server(AbstractWebServer server) {
    pippo.setServer(server);
    return this;
  }

  public Benchmark start() {
    pippo.start();
    return this;
  }

  public Benchmark stop() {
    pippo.stop();
    return this;
  }
}
