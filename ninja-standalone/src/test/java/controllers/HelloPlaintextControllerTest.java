package controllers;

import ninja.NinjaDocTester;
import org.doctester.testbrowser.Request;
import org.doctester.testbrowser.Response;
import org.hamcrest.CoreMatchers;
import static org.hamcrest.CoreMatchers.is;
import org.junit.Test;
import static org.junit.Assert.*;

public class HelloPlaintextControllerTest extends NinjaDocTester {
    
    String URL_PLAINTEXT = "/plaintext";
    
    @Test
    public void helloPlaintextControllerTest() {
        
        Response response = makeRequest(
            Request.GET().url(testServerUrl().path(URL_PLAINTEXT)));
        
        assertThat(response.payload, CoreMatchers.is("Hello, world!"));
        assertThat(
            response.headers.get("Content-Type"), 
            is("text/plain; charset=UTF-8"));
        
   
    }
    
}
