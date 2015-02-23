package sabina.benchmark;

import static org.apache.http.client.fluent.Request.Get;
import static org.junit.Assert.*;
import static sabina.benchmark.Application.main;
import static sabina.Sabina.stop;
import static sun.misc.IOUtils.readFully;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;
import org.apache.http.HttpResponse;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public final class ApplicationTest {
    private static final String ENDPOINT = "http://localhost:8080";
    private static final Gson GSON = new Gson ();

    @BeforeClass public static void setup () {
        main (null);
    }

    @AfterClass public static void close () {
        stop ();
    }

    @Test public void json () throws IOException {
        HttpResponse response = get (ENDPOINT + "/json");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        assertEquals ("Hello, World!", GSON.fromJson (content, Map.class).get ("message"));
    }

    @Test public void plaintext () throws IOException {
        HttpResponse response = get (ENDPOINT + "/plaintext");
        String content = getContent (response);

        checkResponse (response, content, "text/plain");
        assertEquals ("Hello, World!", content);
    }

    @Test public void no_query_parameter () throws IOException {
        HttpResponse response = get (ENDPOINT + "/db");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        Map<?, ?> resultsMap = GSON.fromJson (content, Map.class);
        assertTrue (resultsMap.containsKey ("id") && resultsMap.containsKey ("randomNumber"));
    }

    @Test public void empty_query_parameter () throws IOException {
        HttpResponse response = get (ENDPOINT + "/db?queries");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        checkResultItems (content, 1);
    }

    @Test public void text_query_parameter () throws IOException {
        HttpResponse response = get (ENDPOINT + "/db?queries=text");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        checkResultItems (content, 1);
    }

    @Test public void zero_queries () throws IOException {
        HttpResponse response = get (ENDPOINT + "/db?queries=0");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        checkResultItems (content, 1);
    }

    @Test public void one_thousand_queries () throws IOException {
        HttpResponse response = get (ENDPOINT + "/db?queries=1000");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        checkResultItems (content, 500);
    }

    @Test public void one_query () throws IOException {
        HttpResponse response = get (ENDPOINT + "/db?queries=1");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        checkResultItems (content, 1);
    }

    @Test public void ten_query () throws IOException {
        HttpResponse response = get (ENDPOINT + "/db?queries=10");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        checkResultItems (content, 10);
    }

    @Test public void five_hundred_queries () throws IOException {
        HttpResponse response = get (ENDPOINT + "/db?queries=500");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        checkResultItems (content, 500);
    }

    private HttpResponse get (String uri) throws IOException {
        return Get (uri).execute ().returnResponse ();
    }

    private String getContent (HttpResponse aResponse) throws IOException {
        return new String (readFully (aResponse.getEntity ().getContent (), -1, true));
    }

    private void checkResponse (HttpResponse aRes, String aContent, String contentType) {
        assertTrue (aRes.getFirstHeader ("Server") != null);
        assertTrue (aRes.getFirstHeader ("Date") != null);
        assertEquals (aContent.length (), aRes.getEntity ().getContentLength ());
        assertEquals (contentType, aRes.getEntity ().getContentType ().getValue ());
    }

    private void checkResultItems (String result, int size) {
        List<?> resultsList = GSON.fromJson (result, List.class);
        assertEquals (size, resultsList.size ());

        for (int ii = 0; ii < size; ii++) {
            Map<?, ?> r = (Map)resultsList.get (ii);
            assertTrue (r.containsKey ("id") && r.containsKey ("randomNumber"));
        }
    }
}
