package common;

public class Helper {

	// this configuration was copied from the undertow-example project
	public static final String MYSQL_CONFIG = "jdbcCompliantTruncation=false" +
		"&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true" +
		"&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048" +
		"&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false" +
		"&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true";

}