package common;

public class Helper {

	// most of this configuration was copied from the other projects
	public static final String MYSQL_CONFIG = "useSSL=false&jdbcCompliantTruncation=false" +
		"&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true" +
		"&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048" +
		"&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useServerPrepStmts=true&enableQueryTimeouts=false" +
		"&useUnbufferedIO=false&useReadAheadInput=false&maintainTimeStats=false&cacheRSMetadata=true";

	// most of this configuration was copied from the other projects
	public static final String POSTGRES_CONFIG = "jdbcCompliantTruncation=false" +
		"&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true" +
		"&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048" +
		"&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useServerPrepStmts=true&enableQueryTimeouts=false" +
		"&useUnbufferedIO=false&useReadAheadInput=false&maintainTimeStats=false&cacheRSMetadata=true";

}