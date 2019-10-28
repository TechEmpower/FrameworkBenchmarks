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
import io.vertx.sqlclient.PreparedQuery;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.SqlConnection;
import io.vertx.sqlclient.Tuple;


public class ProcessUpdate {
	
	private transient ObjectPipe<ResultObject> dbUpdateInFlight;	
	private final transient List<ResultObject> collectorDBUpdate = new ArrayList<ResultObject>();
	private final transient ThreadLocalRandom localRandom = ThreadLocalRandom.current();
	private final HTTPResponseService service;
	private final transient PoolManager pm;
	private final AtomicInteger requestsInFlight = new AtomicInteger();

	private PreparedQuery selectQuery;
	private boolean building = false;
	private PreparedQuery updateQuery;
		
	public ProcessUpdate(int pipelineBits, HTTPResponseService service, PoolManager pm) {
		this.dbUpdateInFlight = new ObjectPipe<ResultObject>(pipelineBits, ResultObject.class,	ResultObject::new);
		this.service = service;
		this.pm = pm;
		selectableQuery();
		updateQuery();
	}
	
	
	public void tickEvent() { 

			ResultObject temp = dbUpdateInFlight.tailObject();
			while (null!=temp && temp.getStatus()>=0) {			
				consumeResultObjectDBUpdate(temp);
				temp = dbUpdateInFlight.tailObject();				
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
		
		if (dbUpdateInFlight.hasRoomFor(queries) && service.hasRoomFor(temp) ) {		
						
			processConnection(queries, conId, seqCode);
	
			return true;
		} else {
			requestsInFlight.decrementAndGet();
			return false;
		}
	}
	
	
	private PreparedQuery selectableQuery() {
		
		if (null!=selectQuery || building) {
			return selectQuery;		
		} else {
			building = true;
			pm.pool().getConnection(h -> {
				
				if (h.succeeded()) {
					SqlConnection connection = h.result();
					
					connection.prepare("SELECT * FROM world WHERE id=$1", ph -> {
						if (ph.succeeded()) {							
							selectQuery = ph.result();	
							
							building = false;
							if (updateQuery==null) {
								updateQuery();
							}
							
						} else {							
							ph.cause().printStackTrace();
						}
					});
					
					connection.close();
				} else {
					h.cause().printStackTrace();
				}
			});
			return null;
		}
	}

	private PreparedQuery updateQuery() {
		
		if (null!=updateQuery || building) {
			return updateQuery;		
		} else {
			building = true;
			pm.pool().getConnection(h -> {
				
				if (h.succeeded()) {
					SqlConnection connection = h.result();
					
					connection.prepare("UPDATE world SET randomnumber=$1 WHERE id=$2", ph -> {
						if (ph.succeeded()) {							
							updateQuery = ph.result();	
							building = false;
							if (selectQuery == null) {
								selectableQuery();
							}
						} 
					});
					
					connection.close();
				} 
				
				
			});
			return null;
		}
	}	
	

	private void processConnection(int queries, long conId, long seqCode) {
		
		//only process after we have the prepared statements built.
		PreparedQuery query = selectableQuery();
		if (query==null) {
			return;
		}
		
		PreparedQuery update= updateQuery();
		if (update==null) {
			return;
		}
		
		List<ResultObject> objs = new ArrayList<ResultObject>(queries);
		int q = queries;
		while (--q >= 0) {
				processSingleUpdate(queries, conId, seqCode, objs, query, update);
		
		}
	}


	private void processSingleUpdate(int queries, long conId, long seqCode,
			                         List<ResultObject> objs, 
			                         PreparedQuery query, PreparedQuery update) {
		//testing one per query 

		final ResultObject worldObject = dbUpdateInFlight.headObject();
		assert(null!=worldObject);
							
		worldObject.setConnectionId(conId);
		worldObject.setSequenceId(seqCode);
		worldObject.setStatus(-2);//out for work	
		worldObject.setGroupSize(queries);
		
		worldObject.setId(randomValue());
		objs.add(worldObject);					
		
		try {
			query.execute(Tuple.of(worldObject.getId()), r -> {
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
				        
				        try {
					        update.execute( 							        		
					        			Tuple.of(worldObject.getResult(), worldObject.getId()), ar -> {							        	
												setStatus(worldObject, ar);																												
					        			}
				        			);					        
				        } catch (Throwable t) {
				        	t.printStackTrace();
				        	this.updateQuery = null; //TODO: need to try again.
				        }
				        
					} else {	
					
						System.out.println("unable to query");
						if (r.cause()!=null) {
							r.cause().printStackTrace();
						}
						
						worldObject.setStatus(500);
					}		
					
					//on all N responses.....
													
				});	
						
			dbUpdateInFlight.moveHeadForward(); //always move to ensure this can be read.
		} catch (Throwable t) {
			t.printStackTrace();
			this.selectQuery = null;
			//TODO: rollabck??
		}
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
		dbUpdateInFlight.moveTailForward();//only move forward when it is consumed.
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
					    					   dbUpdateInFlight.publishTailPosition();
					    				   });
		assert(result) : "internal error, we should not pick up more work than we can send";
		requestsInFlight.decrementAndGet();
	}
	
	
	
	
	
}
