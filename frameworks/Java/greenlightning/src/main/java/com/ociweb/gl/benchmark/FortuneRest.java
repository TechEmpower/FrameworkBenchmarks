package com.ociweb.gl.benchmark;

import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.gl.api.HTTPRequestReader;
import com.ociweb.gl.api.HTTPResponseService;
import com.ociweb.gl.api.RestMethodListener;
import com.ociweb.gl.api.TickListener;
import com.ociweb.pronghorn.network.config.HTTPContentTypeDefaults;
import com.ociweb.pronghorn.pipe.ObjectPipe;
import com.ociweb.pronghorn.util.AppendableBuilder;
import com.ociweb.pronghorn.util.Appendables;
import com.ociweb.pronghorn.util.template.StringTemplateBuilder;
import com.ociweb.pronghorn.util.template.StringTemplateRenderer;

import io.reactiverse.pgclient.PgClient;
import io.reactiverse.pgclient.PgIterator;
import io.reactiverse.pgclient.PgPool;
import io.reactiverse.pgclient.PgPoolOptions;
import io.reactiverse.pgclient.Row;

public class FortuneRest implements RestMethodListener, TickListener {

	private static final byte[] ROW_FINISH = "</td></tr>\n".getBytes();
	private static final byte[] ROW_MIDDLE = "</td><td>".getBytes();
	private static final byte[] ROW_START = "<tr><td>".getBytes();
	private final HTTPResponseService service; 
	
	private final transient PgPoolOptions options;
	private transient PgPool pool;
			
	//SQL results write to these object, these same objects are used by template
	private transient ObjectPipe<FortunesObject> inFlight;
	
	private static final transient StringTemplateRenderer<FortunesObject> template =		
			new StringTemplateBuilder<FortunesObject>()
				   .add("<!DOCTYPE html> <html> <head><title>Fortunes</title></head> <body> <table> <tr><th>id</th><th>message</th></tr>\n")
			       .add((t,s,i)-> {
						if (i<s.list().size()) {													
							t.write(ROW_START);
							Appendables.appendValue(t, s.list().get(i).getId());
							t.write(ROW_MIDDLE);							
							Appendables.appendHTMLEntityEscaped(t, s.list().get(i).getFortune());							
							t.write(ROW_FINISH);
							return true;
						} else {
							return false;
						}
			         })		
			       .add("</table></body></html>")
			       .finish();

	
	public FortuneRest(GreenRuntime runtime, PgPoolOptions options, int pipelineBits, int responseCount, int maxResponseSize) {;
	    
		this.options = options;	
		this.service = runtime.newCommandChannel().newHTTPResponseService(responseCount, maxResponseSize);
		this.inFlight =  new ObjectPipe<FortunesObject>(pipelineBits, FortunesObject.class,	FortunesObject::new);
		
	}
	
	
	private PgPool pool() {
		if (null==pool) {
			pool = PgClient.pool(options);
		}
		return pool;
	}

	public boolean restRequest(HTTPRequestReader request) {
	
		final FortunesObject target = inFlight.headObject(); 
		if (null!=target) {
			target.setConnectionId(request.getConnectionId());
			target.setSequenceId(request.getSequenceCode());
	
			target.setStatus(-2);//out for work	
			target.clear();
		
			pool().preparedQuery( "SELECT id, message FROM fortune", r -> {
				    //NOTE: we want to do as little work here a s possible since
				    //      we want this thread to get back to work on other calls.
					if (r.succeeded()) {
						PgIterator resultSet = r.result().iterator();						
						while (	resultSet.hasNext() ) {
					        Row next = resultSet.next();
							target.addFortune(next.getInteger(0), next.getString(1));						
						}
						target.setStatus(200);
					} else {
						System.out.println("fail: "+r.cause().getLocalizedMessage());
						target.setStatus(500);
					}		
					
				});
			
			inFlight.moveHeadForward(); //always move to ensure this can be read.  //TODO: remove and combined with above
			return true;
		} else {
			return false;//can not pick up new work now			
		}		
	}
	

	
	@Override
	public void tickEvent() { //TODO: remove tickEvent here and replace with  pub sub to take next...
		
		FortunesObject temp = inFlight.tailObject();
		while (isReady(temp)) {			
			if (consumeResultObject(temp)) {
				temp = inFlight.tailObject();
			} else {
				break;
			}
		}		
	}
	
	private boolean isReady(FortunesObject temp) {
		return null!=temp && temp.getStatus()>=0;
	}

	private int htmlPos=0;
	private final transient AppendableBuilder htmlBuffer = new AppendableBuilder();
	
	
	private boolean consumeResultObject(final FortunesObject t) {
					
		if (0 == htmlBuffer.byteLength()) {
			//capture all the output text
			t.addFortune(0, "Additional fortune added at request time.");
			t.sort();
			template.render(htmlBuffer, t);
			htmlPos = 0;
		}
		
		
		int bytesRemaining = htmlBuffer.byteLength() - htmlPos;
		int roomForWrite = service.maxVarLength();
		boolean hasContinuation  = bytesRemaining >roomForWrite;
		
		//as long as htmlPos does not match the total bytes of the payload keep 
		//sending out continuation chunks. We do not know how many rows of fortunes
		//may be in the database.
		boolean ok;
		if (0 == htmlPos) {	
			
			ok = service.publishHTTPResponse(t.getConnectionId(), t.getSequenceId(), 200, hasContinuation,
						   HTTPContentTypeDefaults.HTML, 
						   w-> {
							   htmlPos += htmlBuffer.copyTo(w, htmlPos);								   
							   assert(hasContinuation == (htmlPos!=htmlBuffer.byteLength())) : "internal error";
							   
						   });
		} else {		
			ok =service.publishHTTPResponseContinuation(t.getConnectionId(), t.getSequenceId(), hasContinuation,  
							w-> {
								htmlPos += htmlBuffer.copyTo(w,htmlPos);	
								assert(hasContinuation == (htmlPos!=htmlBuffer.byteLength())) : "internal error";
								
							});
		}
		
		if (ok) {
			if (htmlPos == htmlBuffer.byteLength()) {
				
				t.setStatus(-1);
				inFlight.moveTailForward();//only move forward when it is consumed.
				inFlight.publishTailPosition();
				t.list().clear();
				htmlBuffer.clear();
				return true;//do consume this since it is now fully sent
			} else {
				assert(htmlPos < htmlBuffer.byteLength()) : "internal error";			
				return false;//still have more to send later
			}	
		} else {
			return false;
		}		
		
	}
	

}
