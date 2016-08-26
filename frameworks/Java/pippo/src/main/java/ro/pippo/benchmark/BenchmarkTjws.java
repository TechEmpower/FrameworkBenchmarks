package ro.pippo.benchmark;

import ro.pippo.tjws.TjwsServer;

public class BenchmarkTjws {

  public static void main(String[] args) {
    new Benchmark()
        .serverName("TJWS")
        .server(new TjwsServer())
        .start();
  }
}