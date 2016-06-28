package bayou;

import bayou.http.*;
import bayou.mime.ContentType;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.Collections;

/**
 *  bayou.io HTTP Server for TechEmpower/FrameworkBenchmarks
 */
public class BayouServer
{
    public static void main(String[] args) throws Exception
    {
        System.setProperty("bayou.http.server.pipeline", "true" ); // favor pipelined requests
        System.setProperty("bayou.http.server.fiber",    "false"); // fiber not needed in this app

        byte[] bytesHelloWorld = "Hello, World!".getBytes();

        HttpServer server = new HttpServer(request->
        {
            switch(request.uri())
            {
                case "/json" :
                    Object obj = Collections.singletonMap("message", "Hello, World!");
                    return new SimpleHttpResponse(HttpStatus.c200_OK, ContentType.json, toJson(obj));

                case "/plaintext" :
                    return new SimpleHttpResponse(HttpStatus.c200_OK, ContentType.text_plain, bytesHelloWorld);

                default :
                    return HttpResponse.text(404, "Not Found -- ", request.uri() );
            }
        });

        server.conf().setProxyDefaults();  // disable some non-essential features
        //server.conf().trafficDump(System.out::print);

        server.start();
    }


    // json - jackson -------------------------------------------------------------------------------------
    // pretty slow; don't care much.

    static final ObjectMapper objectMapper = new ObjectMapper();
    static byte[] toJson(Object obj)
    {
        try{
            return objectMapper.writeValueAsBytes(obj);
        }catch (JsonProcessingException e){
            throw new RuntimeException(e);  // HTTP 500 Internal Error
        }
    }

}
