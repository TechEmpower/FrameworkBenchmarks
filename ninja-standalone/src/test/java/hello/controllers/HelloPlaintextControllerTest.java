 /*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package hello.controllers;

import ninja.NinjaDocTester;
import org.doctester.testbrowser.Request;
import org.doctester.testbrowser.Response;
import org.hamcrest.CoreMatchers;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author ra
 */
public class HelloPlaintextControllerTest extends NinjaDocTester {
    
    String URL = "/plaintext";
    
    @Test
    public void helloPlaintextControllerTest() {
        
        Response response = makeRequest(
            Request.GET().url(testServerUrl().path(URL)));
        
        assertThat(response.payload, CoreMatchers.is("Hello, world!"));
        assertThat(
            response.headers.get("Content-Type"), 
            CoreMatchers.is("text/plain; charset=UTF-8"));
        
   
    }
    
}
