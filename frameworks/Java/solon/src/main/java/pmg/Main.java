package pmg;

import org.noear.solon.Solon;
import org.noear.solon.SolonApp;

public class Main {
	
	public static final byte[] PLAIN_TEXT_MESSAGE = "Hello world!".getBytes();
	
	public static void main(String[] args) {
		  SolonApp app = Solon.start(Main.class,args);	 
		  
	      app.get("/plaintext",(c)->{
	    	 
	    	  c.contentLength(PLAIN_TEXT_MESSAGE.length);
	    	  c.contentType("text/plain; charset=UTF-8");
	    	  c.outputStream().write(PLAIN_TEXT_MESSAGE);	    	  
	    	  });
	      app.get("/json",(c)->JsonUtil.writeJsonBytes(c, new Message("Hello, World!")));
	      
	}

}
