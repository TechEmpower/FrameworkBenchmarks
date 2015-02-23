package sabina.benchmark;

import static org.apache.http.client.fluent.Request.Get;
import static org.testng.AssertJUnit.*;
import static sabina.benchmark.Application.main;
import static sabina.Sabina.stop;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

import com.google.gson.Gson;
import org.apache.http.HttpResponse;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

@org.testng.annotations.Test
public final class ApplicationTest {
    private static final String ENDPOINT = "http://localhost:5050";
    private static final Gson GSON = new Gson ();

    @org.testng.annotations.BeforeClass @BeforeClass
    public static void setup () {
        main (null);
    }

    @org.testng.annotations.AfterClass @AfterClass
    public static void close () {
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
        checkDbRequest ("/query?queries", 1);
    }

    @Test public void text_query_parameter () throws IOException {
        checkDbRequest ("/query?queries=text", 1);
    }

    @Test public void zero_queries () throws IOException {
        checkDbRequest ("/query?queries=0", 1);
    }

    @Test public void one_thousand_queries () throws IOException {
        checkDbRequest ("/query?queries=1000", 500);
    }

    @Test public void one_query () throws IOException {
        checkDbRequest ("/query?queries=1", 1);
    }

    @Test public void ten_queries () throws IOException {
        checkDbRequest ("/query?queries=10", 10);
    }

    @Test public void five_hundred_queries () throws IOException {
        checkDbRequest ("/query?queries=500", 500);
    }

    @Test public void fortunes () throws IOException {
        HttpResponse response = get (ENDPOINT + "/fortune");
        String content = getContent (response);
        String contentType = response.getEntity ().getContentType ().getValue ();

        assertTrue (response.getFirstHeader ("Server") != null);
        assertTrue (response.getFirstHeader ("Date") != null);
        assertTrue (content.contains ("&lt;script&gt;alert(&quot;This should not be displayed"));
        assertTrue (content.contains ("フレームワークのベンチマーク"));
        assertEquals ("text/html; charset=utf-8", contentType);
    }

    @Test public void no_updates_parameter () throws IOException {
        HttpResponse response = get (ENDPOINT + "/update");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        Map<?, ?> resultsMap = GSON.fromJson (content, Map.class);
        assertTrue (resultsMap.containsKey ("id") && resultsMap.containsKey ("randomNumber"));
    }

    @Test public void empty_updates_parameter () throws IOException {
        checkDbRequest ("/update?queries", 1);
    }

    @Test public void text_updates_parameter () throws IOException {
        checkDbRequest ("/update?queries=text", 1);
    }

    @Test public void zero_updates () throws IOException {
        checkDbRequest ("/update?queries=0", 1);
    }

    @Test public void one_thousand_updates () throws IOException {
        checkDbRequest ("/update?queries=1000", 500);
    }

    @Test public void one_update () throws IOException {
        checkDbRequest ("/update?queries=1", 1);
    }

    @Test public void ten_updates () throws IOException {
        checkDbRequest ("/update?queries=10", 10);
    }

    @Test public void five_hundred_updates () throws IOException {
        checkDbRequest ("/update?queries=500", 500);
    }

    private void checkDbRequest (String path, int itemsCount) throws IOException {
        HttpResponse response = get (ENDPOINT + path);
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        checkResultItems (content, itemsCount);
    }

    private HttpResponse get (String uri) throws IOException {
        return Get (uri).execute ().returnResponse ();
    }

    private String getContent (HttpResponse response) throws IOException {
        InputStream in = response.getEntity ().getContent ();
        return new Scanner (in).useDelimiter ("\\A").next ();
    }

    private void checkResponse (HttpResponse res, String content, String contentType) {
        assertTrue (res.getFirstHeader ("Server") != null);
        assertTrue (res.getFirstHeader ("Date") != null);
        assertEquals (content.length (), res.getEntity ().getContentLength ());
        assertEquals (contentType, res.getEntity ().getContentType ().getValue ());
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
