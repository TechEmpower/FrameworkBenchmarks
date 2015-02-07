package sabina;

import static org.apache.http.client.fluent.Request.Get;
import static org.junit.Assert.*;
import static sabina.Application.main;
import static sabina.Sabina.stop;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;
import org.apache.http.Header;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class ApplicationTest {
    private static final String ENDPOINT = "http://localhost:8080";
    private static final Gson GSON = new Gson ();

    @BeforeClass public static void setup () {
        main (null);
    }

    @AfterClass public static void close () {
        stop ();
    }

    @Test public void json () throws IOException {
        String result = Get (ENDPOINT + "/json").execute ().returnContent ().asString ();
        assertEquals ("Hello, World!", GSON.fromJson (result, Map.class).get ("message"));
    }

    @Test public void plaintext () throws IOException {
        String result = Get (ENDPOINT + "/plaintext").execute ().returnContent ().asString ();
        assertEquals ("Hello, World!", result);
    }

    @Test public void date_header () throws IOException {
        Header[] result = Get (ENDPOINT + "/json").execute ().returnResponse ().getHeaders ("Date");
        assertEquals (1, result.length);
        assertFalse (result[0].getValue ().isEmpty ());
    }

    @Test public void no_query_parameter () throws IOException {
        String result = Get (ENDPOINT + "/db").execute ().returnContent ().asString ();
        Map<?, ?> resultsMap = GSON.fromJson (result, Map.class);
        assertTrue (resultsMap.containsKey ("id") && resultsMap.containsKey ("randomNumber"));
    }

    @Test public void empty_query_parameter () throws IOException {
        String result = Get (ENDPOINT + "/db?queries").execute ().returnContent ().asString ();
        checkResultItems (result, 1);
    }

    @Test public void text_query_parameter () throws IOException {
        String result = Get (ENDPOINT + "/db?queries=text").execute ().returnContent ().asString ();
        checkResultItems (result, 1);
    }

    @Test public void zero_queries () throws IOException {
        String result = Get (ENDPOINT + "/db?queries=0").execute ().returnContent ().asString ();
        checkResultItems (result, 1);
    }

    @Test public void one_thousand_queries () throws IOException {
        String result = Get (ENDPOINT + "/db?queries=1000").execute ().returnContent ().asString ();
        checkResultItems (result, 500);
    }

    @Test public void one_query () throws IOException {
        String result = Get (ENDPOINT + "/db?queries=1").execute ().returnContent ().asString ();
        checkResultItems (result, 1);
    }

    @Test public void ten_query () throws IOException {
        String result = Get (ENDPOINT + "/db?queries=10").execute ().returnContent ().asString ();
        checkResultItems (result, 10);
    }

    @Test public void five_hundred_queries () throws IOException {
        String result = Get (ENDPOINT + "/db?queries=500").execute ().returnContent ().asString ();
        checkResultItems (result, 500);
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
