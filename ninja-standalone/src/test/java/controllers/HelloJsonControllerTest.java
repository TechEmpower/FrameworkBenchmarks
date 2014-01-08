package controllers;

import ninja.NinjaDocTester;
import org.doctester.testbrowser.Request;
import org.doctester.testbrowser.Response;
import org.hamcrest.CoreMatchers;
import static org.hamcrest.CoreMatchers.is;
import org.junit.Test;
import static org.junit.Assert.*;


public class HelloJsonControllerTest extends NinjaDocTester {
    
    String URL_JSON = "/json";

    @Test
    public void testHelloJsonController() {
        
        Response response = makeRequest(
            Request
                .GET()
                .url(testServerUrl().path(URL_JSON))
                .contentTypeApplicationJson());
        
        assertThat(
            response.payloadAs(Message.class).message, 
            is("Hello, world"));
        
    }
    
    /**
     * Duplicated from HelloJsonController.
     * 
     * Stuff in HelloJsonController is final, but to deserialize the message we
     * need an empty constructor...
     */
    public final static class Message {

	public String message;
        
	public Message() {}

	public Message(String message) {
	    this.message = message;
	}
    }
    
}
