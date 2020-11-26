package hello;

import com.linecorp.armeria.server.Server;
import com.linecorp.armeria.server.ServerBuilder;

import hello.services.HelloService;
import hello.services.PostgresDbService;
import hello.services.PostgresFortunesService;

public final class App {
  public static void main(String[] args) {
    ServerBuilder sb = Server.builder();

    sb.http(8080)
      .annotatedService(new HelloService())
      .annotatedService(new PostgresDbService())
      .annotatedService(new PostgresFortunesService());

    Server server = sb.build();
    server.start().join();
  }
}
