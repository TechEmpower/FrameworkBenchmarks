/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package hello.controllers;

import hello.model.Fortune;
import ninja.NinjaDocTester;
import org.doctester.testbrowser.Request;
import org.doctester.testbrowser.Response;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author ra
 */
public class HelloFortuneControllerTest extends NinjaDocTester {

    String URL = "/fortunes";
    
    @Test
    public void testSomeMethod() {
        
        getInjector().getInstance(SetupDao.class).generateFortunesForTest();

        Response response 
                = makeRequest(Request.GET().url(testServerUrl().path(URL)));
        
        System.out.println(" " + response.payload);
        
        // make sure escaping works
        assertTrue(response.payload.contains("&lt;script&gt;I want to be escaped&lt;/script&gt;"));

        // make sure utf-8 works
        assertTrue(response.payload.contains("レームワークのベンチマーク<"));
        
        // make sure new Fortune has been added to response
        assertTrue(response.payload.contains("Additional fortune added at request time."));

        
    }
    
}
