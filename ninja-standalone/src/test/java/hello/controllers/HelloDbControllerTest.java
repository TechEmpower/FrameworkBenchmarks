/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package hello.controllers;

import hello.model.World;
import ninja.NinjaDocTester;
import org.doctester.testbrowser.Request;
import org.doctester.testbrowser.Response;
import org.hamcrest.CoreMatchers;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;

/**
 *
 * @author ra
 */
public class HelloDbControllerTest extends NinjaDocTester {
            
    String SINGLE_GET = "/db";
    String QUERIES = "/queries";
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
                        .url(testServerUrl().path(SINGLE_GET))
                        .contentTypeApplicationJson());
        
        // Just make sure that we get back a World Json.
        assertThat(response.payloadAs(World.class), CoreMatchers.notNullValue());      
                
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
                    .path(QUERIES)
                    .addQueryParameter("queries", numberOfQueries + ""))
                .contentTypeApplicationJson());
        
        // Just make sure that we get back an array
        assertThat(response.payloadAs(World[].class).length, CoreMatchers.is(numberOfQueries)); 
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
        
        assertThat(response.payloadAs(World[].class).length, CoreMatchers.is(numberOfQueries)); 
        
    }

    
    
}
