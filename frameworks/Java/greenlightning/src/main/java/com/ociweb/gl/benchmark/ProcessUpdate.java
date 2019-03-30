package com.ociweb.gl.benchmark;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicInteger;

import com.ociweb.gl.api.HTTPRequestReader;
import com.ociweb.gl.api.HTTPResponseService;
import com.ociweb.pronghorn.network.config.HTTPContentTypeDefaults;
import com.ociweb.pronghorn.pipe.ObjectPipe;

import io.reactiverse.pgclient.PgConnection;
import io.reactiverse.pgclient.PgIterator;
import io.reactiverse.pgclient.PgPreparedQuery;
import io.reactiverse.pgclient.Tuple;

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
	
	private int randomValue() {
		return 1+localRandom.nextInt(10000);
	}
	
	public void tickEvent() { 

			ResultObject temp = DBUpdateInFlight.tailObject();
			while (null!=temp && temp.getStatus()>=0) {			
				consumeResultObjectDBUpdate(temp);
				temp = DBUpdateInFlight.tailObject();				
			}	   
		
	}
	
	//AtomicBoolean run = new AtomicBoolean(true);
	
	public boolean updateRestRequest(HTTPRequestReader request) {
		int queries;
		if (Struct.UPDATES_ROUTE_INT == request.getRouteAssoc() ) {		
			queries = Math.min(Math.max(1, (request.structured().readInt(Field.QUERIES))),500);		
		} else {
			queries = 1;
		}
		
		
		long conId = request.getConnectionId();
		long seqCode = request.getSequenceCode();		
		
		int temp = requestsInFlight.get();
		
		if ( DBUpdateInFlight.hasRoomFor(queries) || service.hasRoomFor(1+temp) ) {		
			
				PgConnection connection = null;
				PgPreparedQuery pQuery = null;
				try {					
					connection = findConnection();
					pQuery = prepQuery(connection, "SELECT * FROM world WHERE id=$1");		
				} catch (InterruptedException e) {
					if (null!=connection) {
						connection.close();
					}
					return false;
				} catch (ExecutionException e) {
					if (null!=connection) {
						connection.close();
					}
					return false;
				}
			    
				final PgConnection con = connection;
				final PgPreparedQuery pu = pQuery;
				
				final AtomicInteger outstanding = new AtomicInteger(queries);
				final List<ResultObject> toUpdate = new ArrayList<ResultObject>();
				requestsInFlight.incrementAndGet();
				
				int q = queries;
				while (--q >= 0) {
				
						final ResultObject worldObject = DBUpdateInFlight.headObject();
						assert(null!=worldObject);
											
						worldObject.setConnectionId(conId);
						worldObject.setSequenceId(seqCode);
						worldObject.setStatus(-2);//out for work	
						worldObject.setGroupSize(queries);
						
						worldObject.setId(randomValue());
							
						exeQuery(pQuery, con, pu, outstanding, toUpdate, worldObject);	
									
						DBUpdateInFlight.moveHeadForward(); //always move to ensure this can be read.
				
				}
				
			return true;
		} else {
			return false;
		}
	}

	private void exeQuery(PgPreparedQuery pQuery, final PgConnection con, final PgPreparedQuery pu,
			final AtomicInteger outstanding, final List<ResultObject> toUpdate, final ResultObject worldObject) {
		
		pQuery.execute(
				Tuple.of(worldObject.getId()), r -> {
				if (r.succeeded()) {
														
					PgIterator resultSet = r.result().iterator();
			        Tuple row = resultSet.next();			        
			        
			        assert(worldObject.getId()==row.getInteger(0));
			        
			        //read the existing random value and store it in the world object
			        worldObject.setResult(row.getInteger(1));
			        ///////////////////////////////////
			        //the object can be used here with the old value
			        ///////////////////////////////////
			        //set the new random value in this object
			        worldObject.setResult(randomValue());							        
			        toUpdate.add(worldObject);
			        
			        
				} else {	
					//TODO: urgent, unable to call so we must back off and try again!!!!
					exeQuery(pQuery, con, pu, outstanding, toUpdate, worldObject);
					return;
					
//					System.out.println("unable to query");
//					if (r.cause()!=null) {
//						r.cause().printStackTrace();
//					}
//					
//					worldObject.setStatus(500);
				}		
				
				if (0 == outstanding.decrementAndGet()) {
					//call update for all the query updates...
														
					List<Tuple> args = new ArrayList<Tuple>();
					toUpdate.forEach(w-> {										
						args.add(Tuple.of(w.getResult(), w.getId()));										
					});
					Collections.sort(args, (a,b) -> {
						return Integer.compare( ((Tuple)a).getInteger(0),
										        ((Tuple)b).getInteger(0));
					
					});

					execUpdate(con, pu, toUpdate, args);
				}
			});
	}

	private void execUpdate(final PgConnection con, final PgPreparedQuery pu, final List<ResultObject> toUpdate,
			List<Tuple> args) {
		con.preparedBatch("UPDATE world SET randomnumber=$1 WHERE id=$2", 							        		
				args, ar -> {	
					
			int status;		
			if (ar.succeeded()) {
		    	status = 200;	
			} else {	
				execUpdate(con, pu, toUpdate, args);
				return;
//				System.out.println("unable to update");
//				if (ar.cause()!=null) {
//					ar.cause().printStackTrace();
//				}			
//				status = 500;
			}
			toUpdate.forEach(w->{
				w.setStatus(status);
			});
			pu.close();
			con.close();

		});
	}

	private PgConnection findConnection() throws InterruptedException, ExecutionException {
		PgConnection connection;
		CompletableFuture<PgConnection> conFu = new CompletableFuture<PgConnection>();
		pm.pool().getConnection(h-> {
			if (h.succeeded()) {
				conFu.complete(h.result());				
			} else {
				conFu.completeExceptionally(h.cause());
			}			
		});
		
		return conFu.get();
	}
	

	private PgPreparedQuery prepQuery(PgConnection con, String sql) throws InterruptedException, ExecutionException {
		
		CompletableFuture<PgPreparedQuery> prepFu = new CompletableFuture<PgPreparedQuery>();
		con.prepare(sql, h->{
			if (h.succeeded()) {
				prepFu.complete(h.result());
			} else {
				prepFu.completeExceptionally(h.cause());
			}
		});
		return prepFu.get();
	}

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
