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
public class HelloJsonControllerTest extends NinjaDocTester {
    
    String URL = "/json";

    @Test
    public void testHelloJsonController() {
        
        Response response = makeRequest(
            Request
                .GET()
                .url(testServerUrl().path(URL))
                .contentTypeApplicationJson());
        
        assertThat(
            response.payloadAs(Message.class).message, 
            CoreMatchers.is("Hello, world"));
        
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
