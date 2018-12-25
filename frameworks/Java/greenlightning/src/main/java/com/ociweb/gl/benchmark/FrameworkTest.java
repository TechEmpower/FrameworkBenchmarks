package com.ociweb.gl.benchmark;

import java.util.concurrent.atomic.AtomicBoolean;

import com.ociweb.gl.api.GreenApp;
import com.ociweb.gl.api.GreenCommandChannel;
/**
 * ************************************************************************
 * For greenlightning support, training or feature reqeusts please contact:
 *   info@objectcomputing.com   (314) 579-0066
 * ************************************************************************
 */
import com.ociweb.gl.api.GreenFramework;
import com.ociweb.gl.api.GreenRuntime;
import com.ociweb.pronghorn.network.ServerSocketWriterStage;

import io.reactiverse.pgclient.PgClient;
import io.reactiverse.pgclient.PgPoolOptions;

public class FrameworkTest implements GreenApp {

	static final String payloadText="Hello, World!";
	static final byte[] payload = payloadText.getBytes();

	private int bindPort;
    private String host;
    private int concurrentWritesPerChannel;
    private int queueLengthOfPendingRequests;
    private int telemetryPort;//for monitoring
    private int minMemoryOfInputPipes;
    private int dbCallMaxResponseSize;
	private	final int dbCallMaxResponseCount;
    private int pipelineBits;
    
    private final int jsonMaxResponseCount;
    private final int jsonMaxResponseSize;
    
    private PgPoolOptions options;
    
    
    private int maxQueueOut;
    private int maxConnectionBits;
    private int requestBlockSize;
    
    private int connectionsPerTrack;
    private int connectionPort;
	
	
	public AtomicBoolean foundDB = new AtomicBoolean(false);
	public static String connectionHost =     "localhost";
	public static String connectionDB =       "testdb";
	public static String connectionUser =     "postgres";
	public static String connectionPassword = "postgres";
			    
    public FrameworkTest() {
    	// use this in commit messages to narrow travis testing to just this project
    	// ebase before using this:  [ci fw-only Java/greenlightning]
    	
    	//this server works best with  -XX:+UseNUMA    	
    	this(System.getProperty("host","0.0.0.0"), 
    		 8080,    //default port for test 
    		 7,       //default concurrency per track
    		 256,     //default max blocks from network, each contains multiple calls 
    		 1<<22,   //default total size of network buffer used by blocks
    		 Integer.parseInt(System.getProperty("telemetry.port", "-1")),
    		 "tfb-database", // jdbc:postgresql://tfb-database:5432/hello_world
    		 "hello_world",
    		 "benchmarkdbuser",
    		 "benchmarkdbpass"
    		 );    	
    }   
        
    public FrameworkTest(String host, int port, 
    		             int concurrentWritesPerChannel, 
    		             int queueLengthOfPendingRequests, 
    		             int minMemoryOfInputPipes,
    		             int telemetryPort,
    		             String dbHost,
    		             String dbName,
    		             String dbUser,
    		             String dbPass) {
    	
    	this.connectionsPerTrack = 4;
    	this.connectionPort = 5432;
    	this.bindPort = port;
    	this.host = host;
    	this.concurrentWritesPerChannel = concurrentWritesPerChannel;
    	this.queueLengthOfPendingRequests = queueLengthOfPendingRequests;
    	this.minMemoryOfInputPipes = minMemoryOfInputPipes;
    	this.telemetryPort = telemetryPort;
    	this.pipelineBits = 16;//max concurrent in flight database requests 1<<pipelineBits
    	
    	this.dbCallMaxResponseCount = 1<<6;
    	this.jsonMaxResponseCount = 1<<16;
    	
    	this.dbCallMaxResponseSize = 20_000; //for 500 mult db call in JSON format
    	this.jsonMaxResponseSize = 1<<9;

    	this.maxQueueOut = 256;
    	this.maxConnectionBits = 13;
    	this.requestBlockSize = 1<<12;
    	
    	
    	if (!"127.0.0.1".equals(System.getProperty("host",null))) { 
    		    		
	    	if (null!=dbHost) {
	    		this.connectionHost = dbHost;
	    	}
	    	if (null!=dbName) {
	    		this.connectionDB = dbName;
	    	}
	    	if (null!=dbUser) {
	    		this.connectionUser = dbUser;
	    	}
	    	if (null!=dbPass) {
	    		this.connectionPassword = dbPass;
	    	}    	
    	}
    	
	    		
    	try {
    		options = new PgPoolOptions()
    				.setPort(connectionPort)
    				.setPipeliningLimit(1<<pipelineBits)
    				.setTcpFastOpen(true)
    				.setHost(connectionHost)
    				.setDatabase(connectionDB)
    				.setUser(connectionUser)
    				.setIdleTimeout(20)
    				.setPassword(connectionPassword)
    				.setCachePreparedStatements(true)
    				.setMaxSize(connectionsPerTrack);	    	

    		///early check to know if we have a database or not,
	    	///this helps testing to know which tests should be run on different boxes.
	    	PgClient.pool(options).getConnection(a->{
	    		foundDB.set(a.succeeded());
	    		if (null!=a.result()) {
	    			a.result().close();
	    		}
	    	});
    	} catch (Throwable t) {
    		//t.printStackTrace();
    		System.out.println("No database in use");
    	}
    	
    }

    
	@Override
    public void declareConfiguration(GreenFramework framework) {
		
		framework.setDefaultRate(10_000);
		
		//for 14 cores this is expected to use less than 16G, must use next largest prime to ensure smaller groups are not multiples.
		framework.useHTTP1xServer(bindPort, this::parallelBehavior) //standard auto-scale
    			 .setHost(host)
    			 .setMaxConnectionBits(maxConnectionBits) //8K max client connections.
    			 .setConcurrentChannelsPerDecryptUnit(concurrentWritesPerChannel)
    			 .setConcurrentChannelsPerEncryptUnit(concurrentWritesPerChannel)
    			 
    			 .setMaxQueueIn(queueLengthOfPendingRequests)
    			 .setMaxRequestSize(requestBlockSize)
    	
    			 .setMinimumInputPipeMemory(minMemoryOfInputPipes)
    			 .setMaxQueueOut(maxQueueOut)
    			 .setMaxResponseSize(dbCallMaxResponseSize) //big enough for large mult db response
    	         .useInsecureServer(); //turn off TLS

		framework.defineRoute()
		         .path("/plaintext")
		         .routeId(Struct.PLAINTEXT_ROUTE);
		
		framework.defineRoute()
		        .path("/json")
		        .routeId(Struct.JSON_ROUTE);
		
		framework.defineRoute()
		        .path("/db")
		        .routeId(Struct.DB_SINGLE_ROUTE);
			
		framework.defineRoute()
		        .path("/queries?queries=#{queries}")
		        .path("/queries")
		        .refineInteger("queries", Field.QUERIES, 1)
		        .routeId(Struct.DB_MULTI_ROUTE_INT);
		
		framework.defineRoute()
		        .path("/queries?queries=${queries}")
			    .routeId(Struct.DB_MULTI_ROUTE_TEXT);
		
		framework.defineRoute()
		        .path("/updates?queries=#{queries}")
		        .path("/updates")
		        .refineInteger("queries", Field.QUERIES, 1)
		        .routeId(Struct.UPDATES_ROUTE_INT);

		framework.defineRoute()
		        .path("/updates?queries=${queries}") 
		        .routeId(Struct.UPDATES_ROUTE_TEXT);
		
		framework.defineRoute()
		        .path("/fortunes")
		        .routeId(Struct.FORTUNES_ROUTE);		
		
		if (telemetryPort>0) {
			framework.enableTelemetry(host,telemetryPort);
		} else {
			framework.enableTelemetryLogging();
		}
				
		framework.setTimerPulseRate(20 * 1000);//3x per minute
    }


	public void parallelBehavior(GreenRuntime runtime) {


		SimpleRest restTest = new SimpleRest(runtime, jsonMaxResponseCount, jsonMaxResponseSize);		
		runtime.registerListener("Simple", restTest)
		       .includeRoutes(Struct.PLAINTEXT_ROUTE, restTest::plainRestRequest)
		       .includeRoutes(Struct.JSON_ROUTE, restTest::jsonRestRequest);
		 

		DBRest dbRestInstance = new DBRest(runtime, options, pipelineBits, dbCallMaxResponseCount, dbCallMaxResponseSize);
		runtime.registerListener("DBReadWrite", dbRestInstance)
				.includeRoutes(Struct.DB_SINGLE_ROUTE, dbRestInstance::singleRestRequest)
				.includeRoutes(Struct.DB_MULTI_ROUTE_TEXT, dbRestInstance::multiRestRequest)		
		        .includeRoutes(Struct.DB_MULTI_ROUTE_INT, dbRestInstance::multiRestRequest)
				.includeRoutes(Struct.UPDATES_ROUTE_TEXT, dbRestInstance::updateRestRequest)
				.includeRoutes(Struct.UPDATES_ROUTE_INT,  dbRestInstance::updateRestRequest)
		        .includeRoutes(Struct.FORTUNES_ROUTE, dbRestInstance::restFortuneRequest);	
		
	}
	 
    @Override
    public void declareBehavior(GreenRuntime runtime) {
    	
    	//log the telemetry snapshot upon every pulse
    	final GreenCommandChannel cmd = runtime.newCommandChannel();    	
    	runtime.addTimePulseListener("log",(t,i)->{
    		cmd.logTelemetrySnapshot();
    	});
    	
    }
  
}
