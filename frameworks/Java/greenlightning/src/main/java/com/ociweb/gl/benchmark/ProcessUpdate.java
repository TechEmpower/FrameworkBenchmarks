package com.ociweb.gl.benchmark;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import com.ociweb.gl.api.HTTPRequestReader;
import com.ociweb.gl.api.HTTPResponseService;
import com.ociweb.pronghorn.network.config.HTTPContentTypeDefaults;
import com.ociweb.pronghorn.pipe.ObjectPipe;

import io.reactiverse.pgclient.PgIterator;
import io.reactiverse.pgclient.Tuple;

public class ProcessUpdate {
	
	private transient ObjectPipe<ResultObject> DBUpdateInFlight;	
	private boolean collectionPendingDBUpdate = false;	
	private final transient List<ResultObject> collectorDBUpdate = new ArrayList<ResultObject>();
	private final transient ThreadLocalRandom localRandom = ThreadLocalRandom.current();
	private final HTTPResponseService service;
	private final transient PoolManager pm;
	
	public ProcessUpdate(int pipelineBits, HTTPResponseService service, PoolManager pm) {
		this.DBUpdateInFlight = new ObjectPipe<ResultObject>(pipelineBits, ResultObject.class,	ResultObject::new);
		this.service = service;
		this.pm = pm;
	}
	
	
	public void tickEvent() { 

		{
			ResultObject temp = DBUpdateInFlight.tailObject();
			while (isReadyDBUpdate(temp)) {			
				if (consumeResultObjectDBUpdate(temp)) {
					temp = DBUpdateInFlight.tailObject();
				} else {
					break;
				}
			}	   
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

		if (DBUpdateInFlight.hasRoomFor(queries)) {		
				    	
				int q = queries;
				while (--q >= 0) {
				
						final ResultObject worldObject = DBUpdateInFlight.headObject();
						assert(null!=worldObject);
											
						worldObject.setConnectionId(conId);
						worldObject.setSequenceId(seqCode);
						worldObject.setStatus(-2);//out for work	
						worldObject.setGroupSize(queries);
						
						worldObject.setId(randomValue());
											 	
						pm.pool().preparedQuery("SELECT * FROM world WHERE id=$1", Tuple.of(worldObject.getId()), r -> {
								if (r.succeeded()) {
																		
									PgIterator resultSet = r.result().iterator();
							        Tuple row = resultSet.next();			        
							        
							        assert(worldObject.getId()==row.getInteger(0));
							        
							        //read the existing random value and store it in the world object
							        worldObject.setResult(row.getInteger(1));
							        
							        ///////////////////////////////////
							        //set the new random value in this object
							        worldObject.setResult(randomValue());			       
							        
							        pm.pool().preparedQuery("UPDATE world SET randomnumber=$1 WHERE id=$2", 							        		
							        			Tuple.of(worldObject.getResult(), worldObject.getId()), ar -> {							        	
										if (ar.succeeded()) {
											
								        	worldObject.setStatus(200);							
								        	
										} else {	
											System.out.println("unable to update");
											if (ar.cause()!=null) {
												ar.cause().printStackTrace();
											}
											
											worldObject.setStatus(500);
										}	
																													
							        });
								} else {	
									System.out.println("unable to query");
									if (r.cause()!=null) {
										r.cause().printStackTrace();
									}
									
									worldObject.setStatus(500);
								}		
								
								
							});	
									
						DBUpdateInFlight.moveHeadForward(); //always move to ensure this can be read.
				
				}
				
			return true;
		} else {
			return false;
		}
	}

	private boolean isReadyDBUpdate(ResultObject temp) {

		if (collectionPendingDBUpdate) {
			//now ready to send, we have all the data	
			if (!publishMultiResponseDBUpdate(collectorDBUpdate.get(0).getConnectionId(), collectorDBUpdate.get(0).getSequenceId() )) {
				return false;
			}
		}
		
		return null!=temp && temp.getStatus()>=0;
	}

	private boolean consumeResultObjectDBUpdate(final ResultObject t) {
		boolean ok;
		//collect all the objects
		collectorDBUpdate.add(t);
		DBUpdateInFlight.moveTailForward();//only move forward when it is consumed.
		if (collectorDBUpdate.size() == t.getGroupSize()) {
			//now ready to send, we have all the data						
			ok =publishMultiResponseDBUpdate(t.getConnectionId(), t.getSequenceId());
		} else {
			ok = true;//added to list
		}				
		
		return ok;
	}

	private boolean publishMultiResponseDBUpdate(long conId, long seqCode) {
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
		collectionPendingDBUpdate = !result;
		return result;
	}
	
	
	
	
	
}
