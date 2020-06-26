package com.javanut.gl.benchmark;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicBoolean;

import com.javanut.gl.api.HTTPRequestReader;
import com.javanut.gl.api.HTTPResponseService;
import com.javanut.pronghorn.network.config.HTTPContentTypeDefaults;
import com.javanut.pronghorn.pipe.ObjectPipe;

import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.Tuple;


public class ProcessQuery {
	
	private final transient ObjectPipe<ResultObject> DBRestInFlight;
	private AtomicBoolean collectionPendingDBRest = new AtomicBoolean(false);
	private transient final List<ResultObject> collectorDBRest = new ArrayList<ResultObject>();
	private final HTTPResponseService service;
	private transient final PoolManager pm;
	private final transient ThreadLocalRandom localRandom = ThreadLocalRandom.current();

	public ProcessQuery(int pipelineBits, HTTPResponseService service, PoolManager pm) {
		
		
		this.DBRestInFlight = new ObjectPipe<ResultObject>(pipelineBits, ResultObject.class,	ResultObject::new);
		this.service = service;
		this.pm = pm;
		
		
	}
	
	public void tickEvent() { 
		//for DBRest
		{
			ResultObject temp;
			while (isReadyDBRest(temp = DBRestInFlight.tailObject())) {
				if (consumeResultObjectDBRest(temp)) {
					temp = DBRestInFlight.tailObject();
				} else {
					break;
				}
			}
		}
		
		
	}
	
	private int randomValue() {
		return 1+localRandom.nextInt(10000);
	}		

	
	public boolean multiRestRequest(HTTPRequestReader request) { 

		final int queries;
		if (Struct.DB_MULTI_ROUTE_INT == request.getRouteAssoc() ) {		
			queries = Math.min(Math.max(1, (request.structured().readInt(Field.QUERIES))),500);		
		} else {
			queries = 1;
		}
		
	
		if (DBRestInFlight.hasRoomFor(queries)) {
			
			sendQueries(pm.pool(),queries,request.getConnectionId(),request.getSequenceCode());
									
			return true;
		} else {
			return false;
		}	
	}

		
	private void sendQueries(PgPool p, int queries, long con, long seq) {
		int q = queries;
		
		while (--q >= 0) {
			
				final ResultObject target = DBRestInFlight.headObject();
				
				//already released but not published yet: TODO: we have a problem here!!!
				assert(null!=target && -1==target.getStatus()) : "found status "+target.getStatus()+" on query "+q+" of "+queries ; //must block that this has been consumed?? should head/tail rsolve.
								
				target.setConnectionId(con);
				target.setSequenceId(seq);
				assert(target.getStatus()==-1);//waiting for work
				target.setStatus(-2);//out for work	
				target.setGroupSize(queries);
			
				p.preparedQuery("SELECT * FROM world WHERE id=$1", Tuple.of(randomValue()), r -> {
						if (r.succeeded()) {

							RowIterator<Row> resultSet = r.result().iterator();
							Tuple row = resultSet.next();			        
							
							target.setId(row.getInteger(0));
							target.setResult(row.getInteger(1));					
							target.setStatus(200);

						} else {
							System.out.println("fail: "+r.cause().getLocalizedMessage());
							target.setStatus(500); 
						}		
						
					});	
							
				DBRestInFlight.moveHeadForward(); //always move to ensure this can be read.
		
		}
	}

	public boolean singleRestRequest(HTTPRequestReader request) { 

		final ResultObject target = DBRestInFlight.headObject();

		if (null!=target && -1==target.getStatus()) {
			target.setConnectionId(request.getConnectionId());
			target.setSequenceId(request.getSequenceCode());
			assert(target.getStatus()==-1);//waiting for work
			target.setStatus(-2);//out for work	
			target.setGroupSize(0);//do not put in a list so mark as 0.
		
			pm.pool().preparedQuery("SELECT * FROM world WHERE id=$1", Tuple.of(randomValue()), r -> {
					if (r.succeeded()) {
						
						RowIterator<Row> resultSet = r.result().iterator();
				        Row row = resultSet.next();			        
				        
				        target.setId((Integer)row.getInteger(0));
				        target.setResult((Integer)row.getInteger(1));					
						target.setStatus(200);
						
					} else {
						System.out.println("fail: "+r.cause().getLocalizedMessage());
						target.setStatus(500);
					}				
				});
	   			
			DBRestInFlight.moveHeadForward(); //always move to ensure this can be read.
			return true;
		} else {
			return false;//can not pick up new work now			
		}
	}
	

	private boolean isReadyDBRest(ResultObject temp) {

		if (collectionPendingDBRest.get()) {
			//now ready to send, we have all the data	
			ResultObject resultObject = collectorDBRest.get(0);
			if (!publishMultiDBResponse(resultObject.getConnectionId(), 
					                    resultObject.getSequenceId() )) {				
				return false;
			}
		}
		
		return null!=temp && temp.getStatus()>=0;
	}

	private boolean consumeResultObjectDBRest(final ResultObject t) {
		boolean ok;
						
		///////////////////////////////
		if (0 == t.getGroupSize()) {	
			ok = service.publishHTTPResponse(t.getConnectionId(), t.getSequenceId(), 200,
				   HTTPContentTypeDefaults.JSON,
				   w-> {
					   Templates.singleTemplateDBRest.render(w, t);
					   t.setStatus(-1);
					   DBRestInFlight.moveTailForward();//only move forward when it is consumed.
					   DBRestInFlight.publishTailPosition();

				   });					
		} else {
			//collect all the objects
			assert(isValidToAdd(t, collectorDBRest));
			collectorDBRest.add(t);					
			DBRestInFlight.moveTailForward();
			if (collectorDBRest.size() == t.getGroupSize()) {
				//now ready to send, we have all the data						
				ok =publishMultiDBResponse(t.getConnectionId(), t.getSequenceId());
				
			} else {
				ok = true;//added to list
			}	
			//moved forward so we can read next but write logic will still be blocked by state not -1			 
			
		}
		return ok;
	}

	private boolean isValidToAdd(ResultObject t, List<ResultObject> collector) {
		if (collector.isEmpty()) {
			return true;
		}
		if (collector.get(0).getSequenceId() != t.getSequenceId()) {
			
			System.out.println("show collection: "+showCollection(collector));
			System.out.println("new result adding con "+t.getConnectionId()+" seq "+t.getSequenceId());
			
		};
		
		
		return true;
	}

	private boolean publishMultiDBResponse(long conId, long seqCode) {
		final boolean result =  service.publishHTTPResponse(conId, seqCode, 200,
					    				   HTTPContentTypeDefaults.JSON,
					    				   w-> {
					    					   Templates.multiTemplateDBRest.render(w, collectorDBRest);
					    					   
					    					   int c = collectorDBRest.size();
					    					   assert(collectorDBRest.get(0).getGroupSize()==c);
					    					   while (--c >= 0) {
					    						   assert(collectorDBRest.get(0).getGroupSize()==collectorDBRest.size());
					    						   assert(collectorDBRest.get(c).getConnectionId() == conId) : c+" expected conId "+conId+" error: "+showCollection(collectorDBRest);
					    						   assert(collectorDBRest.get(c).getSequenceId() == seqCode) : c+" sequence error: "+showCollection(collectorDBRest);    						   
					    						   collectorDBRest.get(c).setStatus(-1);
					    						 
					    					   }
					    					   collectorDBRest.clear();					    					   
					    					   DBRestInFlight.publishTailPosition();
					    				   });
		collectionPendingDBRest.set(!result);
		return result;
	}

	private String showCollection(List<ResultObject> collector) {
		
		StringBuilder builder = new StringBuilder();
		builder.append("\n");
		int i = 0;
		for(ResultObject ro: collector) {
			builder.append(++i+" Con:"+ro.getConnectionId()).append(" Id:").append(ro.getId()).append(" Seq:").append(ro.getSequenceId());
			builder.append("\n");
		}
		
		
		return builder.toString();
	}
	
	
}
