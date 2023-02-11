package pmg;

import org.noear.solon.Solon;

/**
 * 我调整了下代码，让下面指令可以运行（machine translation: I tweaked the code so that the following command works）:
 *
 * wrk -t10 -c100 -d10s --latency "http://localhost:8080/plaintext"
 * wrk -t10 -c100 -d10s --latency "http://localhost:8080/json"
 * */
public class Main {
	
	public static final byte[] PLAIN_TEXT_MESSAGE = "Hello world!".getBytes();
	
	public static void main(String[] args) {
		  Solon.start(Main.class,args, app->{
			  app.get("/plaintext",(c)->{
				  //c.contentLength(PLAIN_TEXT_MESSAGE.length);
				  c.contentType("text/plain; charset=UTF-8");
				  c.output(PLAIN_TEXT_MESSAGE);
			  });

			  app.get("/json",(c)->JsonUtil.writeJsonBytes(c, new Message("Hello, World!")));
		  });
	}
}
