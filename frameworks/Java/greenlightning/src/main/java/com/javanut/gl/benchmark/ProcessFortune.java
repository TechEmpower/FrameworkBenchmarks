package com.javanut.gl.benchmark;

import com.javanut.gl.api.HTTPRequestReader;
import com.javanut.gl.api.HTTPResponseService;
import com.javanut.pronghorn.network.config.HTTPContentTypeDefaults;
import com.javanut.pronghorn.pipe.ObjectPipe;
import com.javanut.pronghorn.util.AppendableBuilder;

import io.vertx.pgclient.PgPool;


public class ProcessFortune {

	private transient ObjectPipe<FortunesObject> fortuneInFlight;
	private int htmlFortunePos=0;
	private final transient AppendableBuilder htmlFortuneBuffer = new AppendableBuilder();
	private final transient PoolManager pm;
	private final HTTPResponseService service;
	
	public ProcessFortune(int pipelineBits, HTTPResponseService service, PoolManager pm) {

		this.fortuneInFlight = new ObjectPipe<FortunesObject>(pipelineBits, FortunesObject.class,	FortunesObject::new);
		this.pm = pm;
		this.service = service;
	}
	
	private PgPool myPool = null;
	
	
	public void tickEvent() { 
		
		//for fortune
		{
			FortunesObject temp;
			while (isReadyFortune(temp = fortuneInFlight.tailObject())) {			
				if (consumeResultObjectFortune(temp)) {
					temp = fortuneInFlight.tailObject();
				} else {
					break;
				}
			}	
			if (null==temp && myPool!=null) { //new test
				//myPool.close();
				myPool = null;
			}
		}
		
	}
	
	
	public boolean restFortuneRequest(HTTPRequestReader request) {
	
		final FortunesObject target = fortuneInFlight.headObject(); 
		if (null!=target) {
			target.setConnectionId(request.getConnectionId());
			target.setSequenceId(request.getSequenceCode());
	
			target.setStatus(-2);//out for work	
			target.clear();

			if (null==myPool) {
				myPool = pm.pool();
			}
			
			gatherData(target);
			
			fortuneInFlight.moveHeadForward(); //always move to ensure this can be read.  
			return true;
		} else {
			return false;//can not pick up new work now			
		}		
	}


    //TODO: generate non DB version for tight local testing.
	private void gatherData(final FortunesObject target) {
		myPool.preparedQuery( "SELECT id, message FROM fortune", r -> {
			    //NOTE: we want to do as little work here a s possible since
			    //      we want this thread to get back to work on other calls.
				if (r.succeeded()) {
					r.result().forEach((row)-> {
						target.addFortune((Integer)row.getInteger(0), (String)row.getString(1));						
					});
					target.setStatus(200);
				} else {
					System.out.println("fail: "+r.cause().getLocalizedMessage());
					target.setStatus(500);
				}		
				
			});
	}

	private boolean isReadyFortune(FortunesObject temp) {
		return null!=temp && temp.getStatus()>=0;
	}


	
	private boolean consumeResultObjectFortune(final FortunesObject t) {
					
		if (0 == htmlFortuneBuffer.byteLength()) {
			//capture all the output text
			t.addFortune(0, "Additional fortune added at request time.");
			t.sort();
			Templates.fortuneTemplate.render(htmlFortuneBuffer, t);
			htmlFortunePos = 0;
		}
		
		
		int bytesRemaining = htmlFortuneBuffer.byteLength() - htmlFortunePos;
		int roomForWrite = service.maxVarLength();
		boolean hasContinuation  = bytesRemaining >roomForWrite;
		
		//as long as htmlPos does not match the total bytes of the payload keep 
		//sending out continuation chunks. We do not know how many rows of fortunes
		//may be in the database.
		boolean ok;
		if (0 == htmlFortunePos) {	
			
			ok = service.publishHTTPResponse(t.getConnectionId(), t.getSequenceId(), 200, hasContinuation,
						   HTTPContentTypeDefaults.HTML, 
						   w-> {
							   htmlFortunePos += htmlFortuneBuffer.copyTo(w, htmlFortunePos);								   
							   assert(hasContinuation == (htmlFortunePos!=htmlFortuneBuffer.byteLength())) : "internal error";
							   
						   });
		} else {		
			ok =service.publishHTTPResponseContinuation(t.getConnectionId(), t.getSequenceId(), hasContinuation,  
							w-> {
								htmlFortunePos += htmlFortuneBuffer.copyTo(w,htmlFortunePos);	
								assert(hasContinuation == (htmlFortunePos!=htmlFortuneBuffer.byteLength())) : "internal error";
								
							});
		}
		
		if (ok) {
			if (htmlFortunePos == htmlFortuneBuffer.byteLength()) {
				
				t.setStatus(-1);
				fortuneInFlight.moveTailForward();//only move forward when it is consumed.
				fortuneInFlight.publishTailPosition();
				t.list().clear();
				htmlFortuneBuffer.clear();
				return true;//do consume this since it is now fully sent
			} else {
				assert(htmlFortunePos < htmlFortuneBuffer.byteLength()) : "internal error";			
				return false;//still have more to send later
			}	
		} else {
			return false;
		}		
		
	}
	
	
	
	
}
