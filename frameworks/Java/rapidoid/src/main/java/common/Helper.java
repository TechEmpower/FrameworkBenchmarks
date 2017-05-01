package common;

public class Helper {

	// most of this configuration was copied from the other projects
	public static final String MYSQL_CONFIG = "useSSL=false&jdbcCompliantTruncation=false" +
		"&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true" +
		"&alwaysSendSetIsolation=false&cacheServerConfiguration=true" +
		"&zeroDateTimeBehavior=convertToNull&traceProtocol=false&enableQueryTimeouts=false" +
		"&useUnbufferedIO=false&useReadAheadInput=false&maintainTimeStats=false" +
		"&cacheRSMetadata=true&useServerPrepStmts=true";

	public static final String POSTGRES_CONFIG = "";

}
