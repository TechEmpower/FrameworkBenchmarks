package com.ociweb.gl.benchmark;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.gl.api.HTTPRequestReader;
import com.ociweb.gl.api.HTTPResponseService;
import com.ociweb.gl.api.PubSubMethodListener;
import com.ociweb.gl.api.RestMethodListener;
import com.ociweb.gl.api.TickListener;
import com.ociweb.pronghorn.network.config.HTTPContentTypeDefaults;
import com.ociweb.pronghorn.pipe.ObjectPipe;
import com.ociweb.pronghorn.util.AppendableBuilder;

import io.reactiverse.pgclient.PgClient;
import io.reactiverse.pgclient.PgIterator;
import io.reactiverse.pgclient.PgPool;
import io.reactiverse.pgclient.PgPoolOptions;
import io.reactiverse.pgclient.Row;
import io.reactiverse.pgclient.Tuple;

public class DBRest implements RestMethodListener, PubSubMethodListener, TickListener {

	private final transient PgPoolOptions options;
	private transient PgPool pool;
	private final ThreadLocalRandom localRandom = ThreadLocalRandom.current();
	
	private final ObjectPipe<ResultObject> DBRestInFlight;
	private boolean collectionPendingDBRest = false;

	//this collector is for the multi db test so we can collect all the objects until we have them all for 
	//the request we are currently sending back
	private transient final List<ResultObject> collectorDBRest = new ArrayList<ResultObject>();
	private final HTTPResponseService service;


	public DBRest(GreenRuntime runtime, PgPoolOptions options, int pipelineBits, int maxResponseCount, int maxResponseSize) {
		
		this.options = options;		
		this.service = runtime.newCommandChannel().newHTTPResponseService(maxResponseCount, maxResponseSize);
		
		this.DBRestInFlight = new ObjectPipe<ResultObject>(pipelineBits, ResultObject.class,	ResultObject::new);
		this.DBUpdateInFlight = new ObjectPipe<ResultObject>(pipelineBits, ResultObject.class,	ResultObject::new);
		this.fortuneInFlight = new ObjectPipe<FortunesObject>(pipelineBits, FortunesObject.class,	FortunesObject::new);
		
	}		
	
	private PgPool pool() {
		if (null==pool) {
			pool = PgClient.pool(options);
		}
		return pool;
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
			
			
			int q = queries;
			while (--q >= 0) {
				
					final ResultObject target = DBRestInFlight.headObject();
					
					//already released but not published yet: TODO: we have a problem here!!!
					assert(null!=target && -1==target.getStatus()) : "found status "+target.getStatus()+" on query "+q+" of "+queries ; //must block that this has been consumed?? should head/tail rsolve.
									
					target.setConnectionId(request.getConnectionId());
					target.setSequenceId(request.getSequenceCode());
					assert(target.getStatus()==-1);//waiting for work
					target.setStatus(-2);//out for work	
					target.setGroupSize(queries);
				
					pool().preparedQuery("SELECT * FROM world WHERE id=$1", Tuple.of(randomValue()), r -> {
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
								
					DBRestInFlight.moveHeadForward(); //always move to ensure this can be read.
			
			}
				
			return true;
		} else {
			return false;
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
		
			pool().preparedQuery("SELECT * FROM world WHERE id=$1", Tuple.of(randomValue()), r -> {
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

			
			DBRestInFlight.moveHeadForward(); //always move to ensure this can be read.
			return true;
		} else {
			return false;//can not pick up new work now			
		}
	}


	
	////////////////////////////////////
	////////////////////////////////////
	
	@Override
	public void tickEvent() { 
		//for DBRest
		{
			ResultObject temp = DBRestInFlight.tailObject();
			while (isReadyDBRest(temp)) {			
				if (consumeResultObjectDBRest(temp)) {
					temp = DBRestInFlight.tailObject();
				} else {
					break;
				}
			}
		}
		
		//forDBUpdate
		{
			ResultObject temp = DBUpdateInFlight.tailObject();
			while (isReady(temp)) {			
				if (consumeResultObject(temp)) {
					temp = DBUpdateInFlight.tailObject();
				} else {
					break;
				}
			}	   
		}
		
		//for fortune
		{
			FortunesObject temp = fortuneInFlight.tailObject();
			while (isReadyFortune(temp)) {			
				if (consumeResultObjectFortune(temp)) {
					temp = fortuneInFlight.tailObject();
				} else {
					break;
				}
			}		
		}
		
	}

	private boolean isReadyDBRest(ResultObject temp) {

		if (collectionPendingDBRest) {
			//now ready to send, we have all the data	
			if (!publishMultiDBResponse(collectorDBRest.get(0).getConnectionId(), collectorDBRest.get(0).getSequenceId() )) {				
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
		collectionPendingDBRest = !result;
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
	
	///////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////
	///DB Update
	/////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////

	private ObjectPipe<ResultObject> DBUpdateInFlight;	
	private boolean collectionPendingDBUpdate = false;	
	private final List<ResultObject> collectorDBUpdate = new ArrayList<ResultObject>();

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
												
						pool().preparedQuery("SELECT * FROM world WHERE id=$1", Tuple.of(worldObject.getId()), r -> {
								if (r.succeeded()) {
																		
									PgIterator resultSet = r.result().iterator();
							        Tuple row = resultSet.next();			        
							        
							        assert(worldObject.getId()==row.getInteger(0));
							        
							        //read the existing random value and store it in the world object
							        worldObject.setResult(row.getInteger(1));
							        
							        ///////////////////////////////////
							        //set the new random value in this object
							        worldObject.setResult(randomValue());
							        							       
							        
							        pool().preparedQuery("UPDATE world SET randomnumber=$1 WHERE id=$2", 							        		
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

	private boolean isReady(ResultObject temp) {

		if (collectionPendingDBUpdate) {
			//now ready to send, we have all the data	
			if (!publishMultiResponse(collectorDBUpdate.get(0).getConnectionId(), collectorDBUpdate.get(0).getSequenceId() )) {
				return false;
			}
		}
		
		return null!=temp && temp.getStatus()>=0;
	}

	private boolean consumeResultObject(final ResultObject t) {
		boolean ok;
		//collect all the objects
		collectorDBUpdate.add(t);
		DBUpdateInFlight.moveTailForward();//only move forward when it is consumed.
		if (collectorDBUpdate.size() == t.getGroupSize()) {
			//now ready to send, we have all the data						
			ok =publishMultiResponse(t.getConnectionId(), t.getSequenceId());
		} else {
			ok = true;//added to list
		}				
		
		return ok;
	}

	private boolean publishMultiResponse(long conId, long seqCode) {
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
	
	//////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////
	//Fortune
	/////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////
	

	
	//SQL results write to these object, these same objects are used by template
	private transient ObjectPipe<FortunesObject> fortuneInFlight;
	
	public boolean restFortuneRequest(HTTPRequestReader request) {
	
		final FortunesObject target = fortuneInFlight.headObject(); 
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
			
			fortuneInFlight.moveHeadForward(); //always move to ensure this can be read.  //TODO: remove and combined with above
			return true;
		} else {
			return false;//can not pick up new work now			
		}		
	}

	private boolean isReadyFortune(FortunesObject temp) {
		return null!=temp && temp.getStatus()>=0;
	}

	private int htmlFortunePos=0;
	private final transient AppendableBuilder htmlFortuneBuffer = new AppendableBuilder();
	
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

