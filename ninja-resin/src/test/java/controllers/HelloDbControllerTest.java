package controllers;

import dao.SetupDao;
import model.World;
import ninja.NinjaDocTester;
import org.doctester.testbrowser.Request;
import org.doctester.testbrowser.Response;
import org.hamcrest.CoreMatchers;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.Before;

public class HelloDbControllerTest extends NinjaDocTester {
            
    String URL_DB = "/db";
    String URL_QUERIES = "/queries";
    String URL_UPDATE = "/update";
    
    @Before
    public void setupClass() {
        getInjector().getInstance(SetupDao.class).generateWorldsForTest();
    }
    
    @Test
    public void testSingleGet() {
        
        Response response = makeRequest(
                Request
                        .GET()
                        .url(testServerUrl().path(URL_DB))
                        .contentTypeApplicationJson());
        
        // Just make sure that we get back a World Json.
        assertThat(response.payloadAs(World.class), notNullValue());      
                
    }
    
    @Test
    public void multipleQueries() {
        
        assertThatMutipleGetWorksFor(1);
        assertThatMutipleGetWorksFor(5);
        assertThatMutipleGetWorksFor(10);
        assertThatMutipleGetWorksFor(15);
        assertThatMutipleGetWorksFor(20);
                
    }
    
    private void assertThatMutipleGetWorksFor(int numberOfQueries) {
        Response response = makeRequest(
            Request
                .GET()
                .url(
                    testServerUrl()
                    .path(URL_QUERIES)
                    .addQueryParameter("queries", numberOfQueries + ""))
                .contentTypeApplicationJson());
        
        // Just make sure that we get back an array
        assertThat(response.payloadAs(World[].class).length, is(numberOfQueries)); 
    }
    
    @Test
    public void testUpdates() {
        
        assertThatUpdateWorks(1);
        assertThatUpdateWorks(5);
        assertThatUpdateWorks(10);
        assertThatUpdateWorks(15);
        assertThatUpdateWorks(20);
                
    }
    
    private void assertThatUpdateWorks(int numberOfQueries) {
        
        Response response = makeRequest(
            Request.GET()
                .url(
                    testServerUrl()
                    .path(URL_UPDATE)
                    .addQueryParameter("queries", numberOfQueries + ""))
                .contentTypeApplicationJson());
        
        assertThat(response.payloadAs(World[].class).length, is(numberOfQueries)); 
        
    }

    
    
}
