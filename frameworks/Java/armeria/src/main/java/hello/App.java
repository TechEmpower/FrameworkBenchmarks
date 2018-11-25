package hello;

import hello.services.HelloService;
import hello.services.PostgresDbService;
import hello.services.PostgresFortunesService;

import com.linecorp.armeria.common.SessionProtocol;
import com.linecorp.armeria.server.Server;
import com.linecorp.armeria.server.ServerBuilder;

public final class App {
  public static void main(String[] args) {
    ServerBuilder sb = new ServerBuilder();

    sb.port(8080, SessionProtocol.HTTP)
      .annotatedService("/", new HelloService())
      .annotatedService("/", new PostgresDbService())
      .annotatedService("/", new PostgresFortunesService());

    Server server = sb.build();
    server.start().join();
  }
}
