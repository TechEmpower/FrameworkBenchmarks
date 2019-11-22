package com.techempower.servicetalk;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import io.servicetalk.http.netty.HttpServers;
import io.servicetalk.http.api.HttpSerializationProviders;
import io.servicetalk.http.api.HttpSerializationProvider;
import io.servicetalk.data.jackson.JacksonSerializationProvider;
import static io.servicetalk.concurrent.api.Single.succeeded;



public final class Server {

    public static void main(String[] args) throws Exception {
        try{
            HttpSerializationProvider serializer = HttpSerializationProviders.jsonSerializer(new JacksonSerializationProvider());
            Map<String, String> obj = new HashMap<String, String>();
            obj.put("message", "Hello, World!");
            HttpServers.forPort(8080)
                    .listenAndAwait((ctx, request, responseFactory) -> {
                        if(request.path().equals("/json")) {
                            return succeeded(responseFactory.ok()
                                    .payloadBody(obj, serializer.serializerFor(Map.class))
                                    .addHeader("Date", GetCurrentTime())
                                    .addHeader("Server", "TFB"));
                        }

                        if(request.path().equals("/plaintext")) {
                            return succeeded(responseFactory.ok()
                                    .payloadBody("Hello, World!", HttpSerializationProviders.textSerializer())
                                    .addHeader("Date", GetCurrentTime())
                                    .addHeader("Server", "TFB"));
                        };

                        return null;
                    })
                    .awaitShutdown();
        } catch (Exception e) {
//            logging is not allowed
        }
    }

    public static String GetCurrentTime() {
        SimpleDateFormat formatter = new SimpleDateFormat("EE, dd MMM yyyy kk:mm:ss z  ");
        Date date = new Date();
        return formatter.format(date);
    }
}

