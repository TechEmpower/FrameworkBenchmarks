/*
 * Copyright © 2015 Juan José Aguililla. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */

package sabina.benchmark;

import static org.apache.http.client.fluent.Request.Get;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

import com.google.gson.Gson;
import org.apache.http.HttpResponse;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

/**
 * <p>TODO
 * Write article about stress test with TestNG (scenarios, combine different tests in scenarios,
 * adding random pauses...)
 */
@Test public final class ApplicationTest {
    private static final String ENDPOINT = "http://localhost:5050";
    private static final Gson GSON = new Gson ();

    private static Application application;

    @BeforeClass public void setup () throws IOException {
        application = new Application ();
    }

    @AfterClass public void close () {
        application.stop ();
    }

    public void json () throws IOException {
        HttpResponse response = get (ENDPOINT + "/json");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        assert "Hello, World!".equals (GSON.fromJson (content, Map.class).get ("message"));
    }

    public void plaintext () throws IOException {
        HttpResponse response = get (ENDPOINT + "/plaintext");
        String content = getContent (response);

        checkResponse (response, content, "text/plain");
        assert "Hello, World!".equals (content);
    }

    public void no_query_parameter () throws IOException {
        HttpResponse response = get (ENDPOINT + "/db");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        Map<?, ?> resultsMap = GSON.fromJson (content, Map.class);
        assert resultsMap.containsKey ("id") && resultsMap.containsKey ("randomNumber");
    }

    public void empty_query_parameter () throws IOException {
        checkDbRequest ("/query?queries", 1);
    }

    public void text_query_parameter () throws IOException {
        checkDbRequest ("/query?queries=text", 1);
    }

    public void zero_queries () throws IOException {
        checkDbRequest ("/query?queries=0", 1);
    }

    public void one_thousand_queries () throws IOException {
        checkDbRequest ("/query?queries=1000", 500);
    }

    public void one_query () throws IOException {
        checkDbRequest ("/query?queries=1", 1);
    }

    public void ten_queries () throws IOException {
        checkDbRequest ("/query?queries=10", 10);
    }

    public void one_hundred_queries () throws IOException {
        checkDbRequest ("/query?queries=100", 100);
    }

    public void five_hundred_queries () throws IOException {
        checkDbRequest ("/query?queries=500", 500);
    }

    public void five_hundred_and_one_queries () throws IOException {
        checkDbRequest ("/query?queries=501", 500);
    }

    public void fortunes () throws IOException {
        HttpResponse response = get (ENDPOINT + "/fortune");
        String content = getContent (response);
        String contentType = response.getEntity ().getContentType ().getValue ();

        assert response.getFirstHeader ("Server") != null;
        assert response.getFirstHeader ("Date") != null;
        assert content.contains ("&lt;script&gt;alert(&quot;This should not be displayed");
        assert content.contains ("フレームワークのベンチマーク");
        assert "text/html; charset=utf-8".equals (contentType.toLowerCase ());
    }

    public void no_updates_parameter () throws IOException {
        HttpResponse response = get (ENDPOINT + "/update");
        String content = getContent (response);

        checkResponse (response, content, "application/json");
        Map<?, ?> resultsMap = GSON.fromJson (content, Map.class);
        assert resultsMap.containsKey ("id") && resultsMap.containsKey ("randomNumber");
    }

    public void empty_updates_parameter () throws IOException {
        checkDbRequest ("/update?queries", 1);
    }

    public void text_updates_parameter () throws IOException {
        checkDbRequest ("/update?queries=text", 1);
    }

    public void zero_updates () throws IOException {
        checkDbRequest ("/update?queries=0", 1);
    }

    public void one_thousand_updates () throws IOException {
        checkDbRequest ("/update?queries=1000", 500);
    }

    public void one_update () throws IOException {
        checkDbRequest ("/update?queries=1", 1);
    }

    public void ten_updates () throws IOException {
        checkDbRequest ("/update?queries=10", 10);
    }

    public void one_hundred_updates () throws IOException {
        checkDbRequest ("/update?queries=100", 100);
    }

    public void five_hundred_updates () throws IOException {
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
        assert res.getFirstHeader ("Server") != null;
        assert res.getFirstHeader ("Date") != null;
        assert content.length () == res.getEntity ().getContentLength ();
        assert res.getEntity ().getContentType ().getValue ().contains (contentType);
    }

    private void checkResultItems (String result, int size) {
        List<?> resultsList = GSON.fromJson (result, List.class);
        assert size == resultsList.size ();

        for (int ii = 0; ii < size; ii++) {
            Map<?, ?> r = (Map)resultsList.get (ii);
            assert r.containsKey ("id") && r.containsKey ("randomNumber");
        }
    }
}
