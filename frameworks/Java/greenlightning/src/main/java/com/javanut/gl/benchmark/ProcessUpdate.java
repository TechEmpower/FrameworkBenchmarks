package com.javanut.gl.benchmark;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import com.javanut.gl.api.HTTPRequestReader;
import com.javanut.gl.api.HTTPResponseService;
import com.javanut.pronghorn.network.config.HTTPContentTypeDefaults;
import com.javanut.pronghorn.pipe.ObjectPipe;

import io.vertx.core.AsyncResult;
import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.SqlConnection;
import io.vertx.sqlclient.Tuple;


public class ProcessUpdate {
	
	private transient ObjectPipe<ResultObject> DBUpdateInFlight;	
	private final transient List<ResultObject> collectorDBUpdate = new ArrayList<ResultObject>();
	private final transient ThreadLocalRandom localRandom = ThreadLocalRandom.current();
	private final HTTPResponseService service;
	private final transient PoolManager pm;
	private final AtomicInteger requestsInFlight = new AtomicInteger();
		
	public ProcessUpdate(int pipelineBits, HTTPResponseService service, PoolManager pm) {
		this.DBUpdateInFlight = new ObjectPipe<ResultObject>(pipelineBits, ResultObject.class,	ResultObject::new);
		this.service = service;
		this.pm = pm;
		
	}
	
	
	public void tickEvent() { 

			ResultObject temp = DBUpdateInFlight.tailObject();
			while (null!=temp && temp.getStatus()>=0) {			
				consumeResultObjectDBUpdate(temp);
				temp = DBUpdateInFlight.tailObject();				
			}	   
		
	}
	

	private int randomValue() {
		return 1+localRandom.nextInt(10000);
	}
	
	public boolean updateRestRequest(HTTPRequestReader request) {
		int queries;
		if (Struct.UPDATES_ROUTE_INT == request.getRouteAssoc() ) {		
			queries = Math.min(Math.max(1, (request.structured().readInt(Field.QUERIES))),500);		
		} else {
			queries = 1;
		}
		long conId = request.getConnectionId();
		long seqCode = request.getSequenceCode();
		int temp = requestsInFlight.incrementAndGet();
		
		if (DBUpdateInFlight.hasRoomFor(queries) && service.hasRoomFor(temp) ) {		
	
		   // final AtomicBoolean ok = new AtomicBoolean();
		    
			//pm.pool().getConnection(result -> {
				
				//if (result.succeeded()) {
				
					//SqlConnection connection = result.result();					
					processConnection(queries, conId, seqCode);
					//connection.close();
					
				//	ok.set(true);
					
				//} else {
				//	requestsInFlight.decrementAndGet();			
				//	ok.set(false);
				//}
				
			//});
				
			//return ok.get();
			return true;
		} else {
			requestsInFlight.decrementAndGet();
			return false;
		}
	}


	private void processConnection(int queries, long conId, long seqCode) {
		
		List<ResultObject> objs = new ArrayList<ResultObject>(queries);
		int q = queries;
		while (--q >= 0) {
				processSingleUpdate(queries, conId, seqCode, objs);
		
		}
	}


	private void processSingleUpdate(int queries, long conId, long seqCode, List<ResultObject> objs) {
		//testing one per query 

		final ResultObject worldObject = DBUpdateInFlight.headObject();
		assert(null!=worldObject);
							
		worldObject.setConnectionId(conId);
		worldObject.setSequenceId(seqCode);
		worldObject.setStatus(-2);//out for work	
		worldObject.setGroupSize(queries);
		
		worldObject.setId(randomValue());
		objs.add(worldObject);					
		
		pm.pool().preparedQuery("SELECT * FROM world WHERE id=$1", Tuple.of(worldObject.getId()), r -> {
				if (r.succeeded()) {
														
					RowIterator<Row> resultSet = r.result().iterator();
			        Tuple row = resultSet.next();			        
			        
			        assert(worldObject.getId()==row.getInteger(0));
			        
			        //read the existing random value and store it in the world object
			        worldObject.setResult(row.getInteger(1));
			        ///////////////////////////////////
			        //the object can be used here with the old value
			        ///////////////////////////////////
			        //set the new random value in this object
			        worldObject.setResult(randomValue());							        
			        
			        
			        pm.pool().preparedQuery("UPDATE world SET randomnumber=$1 WHERE id=$2", 							        		
		        			Tuple.of(worldObject.getResult(), worldObject.getId()), ar -> {							        	
									setStatus(worldObject, ar);																												
		        			}
		        			);					        
			        
				} else {	
				
					System.out.println("unable to query");
					if (r.cause()!=null) {
						r.cause().printStackTrace();
					}
					
					worldObject.setStatus(500);
				}		
				
				//on all N responses.....
												
			});	
					
		DBUpdateInFlight.moveHeadForward(); //always move to ensure this can be read.
	}


	private static void setStatus(final ResultObject worldObject, AsyncResult<RowSet<Row>> ar) {
		if (ar.succeeded()) {														
			worldObject.setStatus(200);			
			
		} else {	
			System.out.println("unable to update");
			if (ar.cause()!=null) {
				ar.cause().printStackTrace();
			}
			
			worldObject.setStatus(500);
		}
	}

	
//	private void execUpdate(List<ResultObject> toUpdate, List<Tuple> args, int i) {
//				
//		pm.pool().preparedBatch("UPDATE world SET randomnumber=$1 WHERE id=$2", 							        		
//				args, ar -> {	
//					
//			pause.addAndGet(i);
//			int status;		
//			if (ar.succeeded()) {
//		    	status = 200;
//		    	pause.decrementAndGet();
//			} else {
//				execUpdate(toUpdate, args, 0);
//				return;
////				System.out.println("unable to update");
////				if (ar.cause()!=null) {
////					ar.cause().printStackTrace();
////				}			
////				status = 500;
//			}
//			toUpdate.forEach(w->{
//				w.setStatus(status);
//			});
//
//
//		});
//			
//	}
	
	private void consumeResultObjectDBUpdate(final ResultObject t) {

		//collect all the objects
		collectorDBUpdate.add(t);
		DBUpdateInFlight.moveTailForward();//only move forward when it is consumed.
		if (collectorDBUpdate.size() == t.getGroupSize()) {
			//now ready to send, we have all the data						
			publishMultiResponseDBUpdate(t.getConnectionId(), t.getSequenceId());
		}
	}

	private void publishMultiResponseDBUpdate(long conId, long seqCode) {
		boolean result =  service.publishHTTPResponse(conId, seqCode, 200,
					    				   HTTPContentTypeDefaults.JSON,
					    				   w-> {
					    					   Templates.multiTemplate.render(w, collectorDBUpdate);
					    					   int c = collectorDBUpdate.size();
					    					   while (--c>=0) {
					    						   assert(collectorDBUpdate.get(c).getConnectionId() == conId);
					    						   assert(collectorDBUpdate.get(c).getSequenceId() == seqCode);					    						   
					    						   collectorDBUpdate.get(c).setStatus(-1);
					    					   }
					    					   collectorDBUpdate.clear();
					    					   DBUpdateInFlight.publishTailPosition();
					    				   });
		assert(result) : "internal error, we should not pick up more work than we can send";
		requestsInFlight.decrementAndGet();
	}
	
	
	
	
	
}
