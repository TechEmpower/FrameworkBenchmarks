package hello;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import com.linecorp.armeria.common.HttpHeaderNames;
import com.linecorp.armeria.server.Server;
import com.linecorp.armeria.server.ServerBuilder;

import hello.services.HelloService;
import hello.services.PostgresDbService;
import hello.services.PostgresFortunesService;

public final class App {
  public static void main(String[] args) {
    ServerBuilder sb = new ServerBuilder();

    sb.http(8080)
      .annotatedService("/", new HelloService())
      .annotatedService("/", new PostgresDbService())
      .annotatedService("/", new PostgresFortunesService())
      .decorator((delegate, ctx, req) -> {
        ctx.addAdditionalResponseHeader(HttpHeaderNames.SERVER, "armeria");
        ctx.addAdditionalResponseHeader(HttpHeaderNames.DATE,
                                        DateTimeFormatter.RFC_1123_DATE_TIME.format(
                                                ZonedDateTime.now(ZoneOffset.UTC)));
        return delegate.serve(ctx, req);
      });

    Server server = sb.build();
    server.start().join();
  }
}
