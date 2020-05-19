package com.javanut.gl.benchmark;

import java.util.concurrent.atomic.AtomicBoolean;

import com.javanut.gl.api.GreenApp;
import com.javanut.gl.api.GreenCommandChannel;
import com.javanut.gl.api.GreenFramework;
import com.javanut.gl.api.GreenRuntime;
import com.javanut.pronghorn.network.ServerSocketWriterStage;

import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.PoolOptions;


public class FrameworkTest implements GreenApp {

	private final String payloadText;
	final byte[] payload;

	private int bindPort;
    private String host;
    private int concurrentWritesPerChannel;
  
    private int telemetryPort;
    private int minMemoryOfInputPipes;
    private int dbCallMaxResponseSize;
	private	final int dbCallMaxResponseCount;
    private int pipelineBits;
    
    private final int jsonMaxResponseCount;
    private final int jsonMaxResponseSize;
    
    private PgConnectOptions options;
    private PoolOptions poolOptions;
    
    private int maxQueueOut;
    private int maxConnectionBits;
    private int maxRequestSize;
    
    private int connectionsPerTrack;
    private int connectionPort;
	
	
	public AtomicBoolean foundDB = new AtomicBoolean(false);
	public static String connectionHost =     "localhost";
	public static String connectionDB =       "testdb";
	public static String connectionUser =     "postgres";
	public static String connectionPassword = "postgres";
	
	//TODO: add utility to compute this based on need.
	static final int c = 148;//293;//592; // to reach 16K simultaneous calls

	private final long defaultRate = Long.parseLong(System.getProperty("xx.rate", "120000")); //was 180000
	//Need to record how many records per pass are done...
	
	static {
		System.setProperty("java.lang.Integer.IntegerCache.high", ""+Integer.MAX_VALUE);

		ServerSocketWriterStage.BASE_ADJUST = Float.parseFloat(System.getProperty("xx.ratio", "1"));
		ServerSocketWriterStage.HARD_LIMIT_NS = Long.parseLong(System.getProperty("xx.limitns", "120000"));		
	}
	
    public FrameworkTest() {
    	    	
    	// use this in commit messages to narrow travis testing to just this project
    	// rebase before using this:  [ci fw-only Java/greenlightning]
    	
    	
    	//this server works best with  -XX:+UseNUMA    	
    	this(System.getProperty("host","0.0.0.0"), 
    		 Integer.parseInt(System.getProperty("port","8080")),    	//default port for test 
    		 c,         //pipes per track 
    		 Integer.parseInt(System.getProperty("telemetry.port", "-1")),
    		 "tfb-database", // jdbc:postgresql://tfb-database:5432/hello_world
    		 "hello_world",
    		 "benchmarkdbuser",
    		 "benchmarkdbpass",
    		 System.getProperty("custom.payload", "Hello, World!")    		 
    		 );
    	   	
    	System.out.println("xx.rate "+defaultRate+"  xx.ratio "+ServerSocketWriterStage.BASE_ADJUST+" xx.limitns "+ServerSocketWriterStage.HARD_LIMIT_NS);
		
    }   
        
    public FrameworkTest(String host, int port, 
    		             int concurrentWritesPerChannel, 
    		             int telemetryPort,
    		             String dbHost,
    		             String dbName,
    		             String dbUser,
    		             String dbPass,
    		             String payloadResponse) {
    	
    	this.payloadText = payloadResponse;
    	this.payload = payloadText.getBytes();
    	
    	this.connectionsPerTrack = 2;
    	this.connectionPort = 5432;
    	this.bindPort = port;
    	this.host = host;
    	this.concurrentWritesPerChannel = concurrentWritesPerChannel;

    	this.telemetryPort = telemetryPort;
    	this.pipelineBits = 15;//max concurrent in flight database requests 1<<pipelineBits
    	            
    	this.dbCallMaxResponseCount = c*8; //this will limit the in flight DB calls so make it larger
    	this.jsonMaxResponseCount = c*16*4;
    	
    	this.dbCallMaxResponseSize = 20_000; //for 500 mult db call in JSON format
    	this.jsonMaxResponseSize = 1<<8;

    	this.maxQueueOut = 8*20;   	
    	this.maxConnectionBits = 15;//16K connections, for test plus overhead MUST be 32K
    	
    	//do not make much larger than what is required to hold 16 in flight requests
    	this.maxRequestSize = 1<<11;//between ServerSocketBulkReader and ServerSocketBulkRouter
    	    	
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
    		options = new PgConnectOptions()
    				.setPort(connectionPort)
    				.setPipeliningLimit(1<<pipelineBits)
    				.setHost(connectionHost)
    				.setDatabase(connectionDB)
    				.setUser(connectionUser)
    				.setPassword(connectionPassword)    		
    				.setCachePreparedStatements(true)
    				.setTcpNoDelay(true)
    				.setTcpKeepAlive(true);	    	

    		poolOptions = new PoolOptions()    				
    				  .setMaxSize(connectionsPerTrack);
    		
    		///early check to know if we have a database or not,
	    	///this helps testing to know which tests should be run on different boxes.
	    	PgPool pool = PgPool.pool(options, poolOptions);
			pool.getConnection(a->{
	    		foundDB.set(a.succeeded());
	    		if (null!=a.result()) {
	    			a.result().close();
	    		}
	    	});			
			pool.close();

    	} catch (Throwable t) {
    		//t.printStackTrace();
    		System.out.println("No database in use");
    	}
    	
    }

    
	@Override
    public void declareConfiguration(GreenFramework framework) {
		
		framework.setDefaultRate(defaultRate);
	
		//for 14 cores this is expected to use less than 16G, must use next largest prime to ensure smaller groups are not multiples.
		framework.useHTTP1xServer(bindPort, this::parallelBehavior) //standard auto-scale
    			 .setHost(host)
    			 .setMaxConnectionBits(maxConnectionBits)
    			 .setConcurrentChannelsPerDecryptUnit(concurrentWritesPerChannel)                //16K   14 bits
    	
    			 //NOTE: not sure this is optimal yet ...
    			 //TODO: neeed to allow for multiple writes one pipe! big dif.
    			// .setConcurrentChannelsPerEncryptUnit(Math.max(1,concurrentWritesPerChannel/2))  //8K    
    			 .setConcurrentChannelsPerEncryptUnit(concurrentWritesPerChannel/25)  ///80) ///16) // /8)//4)

    			 .setMaxRequestSize(maxRequestSize)
    			 .setMaxQueueIn(c*16)
    	
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
				
		framework.setTimerPulseRate(60 * 1000);//1x per minute
    }


	public void parallelBehavior(GreenRuntime runtime) {

		SimpleRest restTest = new SimpleRest(runtime, jsonMaxResponseCount, jsonMaxResponseSize, payload);		
		runtime.registerListener("Simple", restTest)
		       .includeRoutes(Struct.PLAINTEXT_ROUTE, restTest::plainRestRequest)
		       .includeRoutes(Struct.JSON_ROUTE, restTest::jsonRestRequest);		 

		DBRest dbRestInstance = new DBRest(runtime, options, poolOptions, pipelineBits, 
				                           dbCallMaxResponseCount, dbCallMaxResponseSize);		
		
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
