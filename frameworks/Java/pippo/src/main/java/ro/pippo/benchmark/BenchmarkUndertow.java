package ro.pippo.benchmark;

import ro.pippo.undertow.UndertowServer;

public class BenchmarkUndertow {

  public static void main(String[] args) {
    new Benchmark()
        .serverName("Undertow")
        .server(new UndertowServer())
        .start();
  }
}