package com.ociweb.gl.benchmark;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicInteger;

import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.gl.api.HTTPRequestReader;
import com.ociweb.gl.api.HTTPResponseService;
import com.ociweb.gl.api.PubSubMethodListener;
import com.ociweb.gl.api.RestMethodListener;
import com.ociweb.gl.api.TickListener;
import com.ociweb.json.encode.JSONRenderer;
import com.ociweb.pronghorn.network.config.HTTPContentTypeDefaults;
import com.ociweb.pronghorn.pipe.ObjectPipe;

import io.reactiverse.pgclient.PgConnection;
import io.reactiverse.pgclient.PgIterator;
import io.reactiverse.pgclient.PgPool;
import io.reactiverse.pgclient.Tuple;

public class DBRest implements RestMethodListener, PubSubMethodListener, TickListener {

	private final PgPool pool;
	private final ThreadLocalRandom localRandom = ThreadLocalRandom.current();
	private final ObjectPipe<ResultObject> inFlight;
		
	public DBRest(GreenRuntime runtime, PgPool pool, int pipelineBits, int maxResponseCount, int maxResponseSize) {
		this.pool = pool;		
		this.inFlight = new ObjectPipe<ResultObject>(pipelineBits, ResultObject.class,	ResultObject::new);
		this.service = runtime.newCommandChannel().newHTTPResponseService(maxResponseCount, maxResponseSize);
	}		
	
	private int randomValue() {
		return 1+localRandom.nextInt(10000);
	}		
	
	AtomicInteger totalCountInFlight = new AtomicInteger(0);//patch needed until we add better access methods to the ObjectPipe.
	
	public boolean multiRestRequest(HTTPRequestReader request) { 

		final int queries;
		if (Struct.DB_MULTI_ROUTE_INT == request.getRouteAssoc() ) {		
			queries = Math.min(Math.max(1, (request.structured().readInt(Field.QUERIES))),500);		
		} else {
			queries = 1;
		}
		
	
		if (inFlight.hasRoomFor(queries) &&  (totalCountInFlight.get()==inFlight.count() || totalCountInFlight.get()==0)) {
			
			
			int q = queries;
			while (--q >= 0) {
				    totalCountInFlight.incrementAndGet();
				
					final ResultObject target = inFlight.headObject();
					
					assert(null!=target && -1==target.getStatus()) : "found status "+target.getStatus()+" on query "+q+" of "+queries ; //must block that this has been consumed?? should head/tail rsolve.
									
					target.setConnectionId(request.getConnectionId());
					target.setSequenceId(request.getSequenceCode());
					assert(target.getStatus()==-1);//waiting for work
					target.setStatus(-2);//out for work	
					target.setGroupSize(queries);
				
					pool.preparedQuery("SELECT * FROM world WHERE id=$1", Tuple.of(randomValue()), r -> {
							if (r.succeeded()) {
								
								PgIterator resultSet = r.result().iterator();
						        Tuple row = resultSet.next();			        
						        
						        target.setId(row.getInteger(0));
						        target.setResult(row.getInteger(1));					
								target.setStatus(200);
								
							} else {
								System.out.println("fail: "+r.cause().getLocalizedMessage());
								target.setStatus(500); 
							}				
						});	
								
					inFlight.moveHeadForward(); //always move to ensure this can be read.
			
			}
				
			return true;
		} else {
			return false;
		}	
	}

	

	
	public boolean singleRestRequest(HTTPRequestReader request) { 

		final ResultObject target = inFlight.headObject();
		if (null!=target && -1==target.getStatus()) {
			target.setConnectionId(request.getConnectionId());
			target.setSequenceId(request.getSequenceCode());
			assert(target.getStatus()==-1);//waiting for work
			target.setStatus(-2);//out for work	
			target.setGroupSize(0);//do not put in a list so mark as 0.
		
			pool.preparedQuery("SELECT * FROM world WHERE id=$1", Tuple.of(randomValue()), r -> {
					if (r.succeeded()) {
						
						PgIterator resultSet = r.result().iterator();
				        Tuple row = resultSet.next();			        
				        
				        target.setId(row.getInteger(0));
				        target.setResult(row.getInteger(1));					
						target.setStatus(200);
						
					} else {
						System.out.println("fail: "+r.cause().getLocalizedMessage());
						target.setStatus(500);
					}				
				});

			
			inFlight.moveHeadForward(); //always move to ensure this can be read.
			return true;
		} else {
			return false;//can not pick up new work now			
		}
	}


	
	////////////////////////////////////
	////////////////////////////////////
	
	private final JSONRenderer<List<ResultObject>> multiTemplate = new JSONRenderer<List<ResultObject>>()
	    	  .array((o,i) -> i<o.size()?o:null)
		          .startObject((o, i) -> o.get(i))
					.integer("id", o -> o.getId() )
					.integer("randomNumber", o -> o.getResult())
		          .endObject();
	
	private final JSONRenderer<ResultObject> singleTemplate = new JSONRenderer<ResultObject>()
		   	  .startObject()
				.integer("id", o -> o.getId() )
				.integer("randomNumber", o -> o.getResult())
	          .endObject();
	
	private boolean collectionPending = false;

	//this collector is for the multi db test so we can collect all the objects until we have them all for 
	//the request we are currently sending back
	private final List<ResultObject> collector = new ArrayList<ResultObject>();
	private final HTTPResponseService service;


	@Override
	public void tickEvent() { 
		
		ResultObject temp = inFlight.tailObject();
		while (isReady(temp)) {			
			if (consumeResultObject(temp)) {
				temp = inFlight.tailObject();
			} else {
				break;
			}
		}	   
		
	}

	private boolean isReady(ResultObject temp) {

		if (collectionPending) {
			//now ready to send, we have all the data	
			if (!publishMultiResponse(collector.get(0).getConnectionId(), collector.get(0).getSequenceId() )) {				
				return false;
			}
		}
		
		return null!=temp && temp.getStatus()>=0;
	}

	private boolean consumeResultObject(final ResultObject t) {
		boolean ok;
						
		///////////////////////////////
		if (0 == t.getGroupSize()) {	
			ok = service.publishHTTPResponse(t.getConnectionId(), t.getSequenceId(), 200,
				   HTTPContentTypeDefaults.JSON,
				   w-> {
					   singleTemplate.render(w, t);
					   t.setStatus(-1);
					   inFlight.moveTailForward();//only move forward when it is consumed.
					   totalCountInFlight.decrementAndGet();
				   });					
		} else {
			//collect all the objects
			assert(isValidToAdd(t, collector));
			collector.add(t);					
			if (collector.size() == t.getGroupSize()) {
				//now ready to send, we have all the data						
				ok =publishMultiResponse(t.getConnectionId(), t.getSequenceId());
				
			} else {
				ok = true;//added to list
			}	
			inFlight.moveTailForward();
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

	private boolean publishMultiResponse(long conId, long seqCode) {
		final boolean result =  service.publishHTTPResponse(conId, seqCode, 200,
					    				   HTTPContentTypeDefaults.JSON,
					    				   w-> {
					    					   multiTemplate.render(w, collector);
					    					   
					    					   int c = collector.size();
					    					   assert(collector.get(0).getGroupSize()==c);
					    					   while (--c >= 0) {
					    						   assert(collector.get(0).getGroupSize()==collector.size());
					    						   assert(collector.get(c).getConnectionId() == conId) : c+" expected conId "+conId+" error: "+showCollection(collector);
					    						   assert(collector.get(c).getSequenceId() == seqCode) : c+" sequence error: "+showCollection(collector);    						   
					    						   collector.get(c).setStatus(-1);
					    						   totalCountInFlight.decrementAndGet();
					    					   }
					    					   collector.clear();					    					   
					    				   });
		collectionPending = !result;
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

