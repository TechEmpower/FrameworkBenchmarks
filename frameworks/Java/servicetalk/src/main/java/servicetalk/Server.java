package servicetalk;

import java.util.concurrent.ThreadLocalRandom;
import java.util.HashMap;
import java.util.Map;
import java.time.format.DateTimeFormatter;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;

import io.servicetalk.concurrent.api.AsyncContext;
import io.servicetalk.concurrent.api.Completable;
import io.servicetalk.http.api.*;
import io.servicetalk.http.netty.HttpProtocolConfigs;
import io.servicetalk.http.netty.HttpServers;
import io.servicetalk.data.jackson.JacksonSerializationProvider;
import io.servicetalk.transport.api.ConnectionAcceptor;
import io.servicetalk.transport.api.ConnectionContext;
import io.servicetalk.transport.api.IoExecutor;
import io.servicetalk.transport.netty.internal.*;

import static io.servicetalk.concurrent.api.Single.succeeded;



public final class Server
{

    public static void main(String[] args) throws Exception
    {

        /*
         * Disable  AsyncContext
         */
        AsyncContext.disable();

        /*
         *   Factory to implement io pooling
         */
        IoExecutor ioExecutor = NettyIoExecutors.createIoExecutor(
          Runtime.getRuntime().availableProcessors(),
          new IoThreadFactory("io-pool")
        );

        /*
         * Factory to disable headers validation
         */

        DefaultHttpHeadersFactory headersFactory = new DefaultHttpHeadersFactory(false,false);


        HttpSerializationProvider serializer = HttpSerializationProviders.jsonSerializer(new JacksonSerializationProvider());
//            Create a custom server builder with performance enhancements
        HttpServers
          .forPort(8080)
          .executionStrategy(HttpExecutionStrategies.noOffloadsStrategy())
          .ioExecutor(ioExecutor)
          .disableDrainingRequestPayloadBody()
          .protocols(HttpProtocolConfigs.h1().headersFactory(headersFactory).build())
          .appendConnectionAcceptorFilter(delegate -> new ConnectionAcceptor()
          {
              @Override
              public Completable accept(ConnectionContext context)
              {
                  ((NettyConnectionContext)context).updateFlushStrategy((current, isOrig) -> FlushStrategies.flushOnEnd());
                  return delegate.accept(context);
              }
          })
          .listenAndAwait((ctx, request, responseFactory) ->
            {
              ((NettyConnectionContext)ctx).updateFlushStrategy(((current, isCurrentOriginal) -> FlushStrategies.flushOnEach()));
              if(request.path().equals("/json"))
              {
                Map<String, String> obj = new HashMap<String, String>();
                obj.put("message", "Hello, World!");
                return succeeded(responseFactory.ok()
                  .payloadBody(obj, serializer.serializerFor(Map.class))
                  .addHeader("Date", getCurrentTime())
                  .addHeader("Server", "ServiceTalk"));
              }

              if(request.path().equals("/plaintext"))
              {
                return succeeded(responseFactory.ok()
                  .payloadBody("Hello, World!", HttpSerializationProviders.textSerializer())
                  .addHeader("Date", getCurrentTime())
                  .addHeader("Server", "ServiceTalk"));
              };
              return null;
            })
            .awaitShutdown();
    }

    public static String getCurrentTime()
    {
        return DateTimeFormatter.RFC_1123_DATE_TIME.format(
          ZonedDateTime.now(ZoneOffset.UTC));
    }

    private static int getRandomNumber()
    {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }
}

