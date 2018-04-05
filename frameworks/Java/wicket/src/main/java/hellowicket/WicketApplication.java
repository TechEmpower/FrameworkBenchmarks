package hellowicket;

import com.zaxxer.hikari.HikariDataSource;
import hellowicket.dbupdates.HelloDbUpdatesReference;
import hellowicket.fortune.FortunePage;
import hellowicket.plaintext.HelloTextReference;
import javax.naming.InitialContext;
import javax.sql.DataSource;
import org.apache.wicket.protocol.http.WebApplication;
import org.apache.wicket.settings.RequestCycleSettings;

/**
 * Application object for your web application..
 */
public class WicketApplication extends WebApplication
{
	private DataSource db;

	@Override
	public Class<HomePage> getHomePage()
	{
		return HomePage.class;
	}

	@Override
	public void init()
	{
		super.init();

		db = newDataSource();

		// disable response caching to be more close to other
		// test applications' behavior
		RequestCycleSettings requestCycleSettings = getRequestCycleSettings();
		requestCycleSettings.setBufferResponse(false);

		// set UTF-8 for /fortunes test
		requestCycleSettings.setResponseRequestEncoding("UTF-8");

		mountResource("json", new HelloJsonReference());
		mountResource("db", new HelloDbReference());
		mountResource("updates", new HelloDbUpdatesReference());
		mountResource("plaintext", new HelloTextReference());
		mountPage("fortunes", FortunePage.class);
	}

	@Override
	protected void onDestroy()
	{
		if (db instanceof HikariDataSource) {
			((HikariDataSource)db).close();
		}
		super.onDestroy();
	}

	private boolean useResinDataSource()
	{
		return false;
	}

	public static WicketApplication get()
	{
		return (WicketApplication) WebApplication.get();
	}

	public DataSource getDataSource()
	{
		return db;
	}

	private DataSource newDataSource()
	{
		DataSource dataSource;
		try
		{
			if (useResinDataSource())
			{
				InitialContext jndiContext = new InitialContext();
				dataSource = (DataSource) jndiContext.lookup("java:comp/env/jdbc/hello_world");
			}
			else 
			{
				// allocating a resource for future use should not (auto) close the resource 
				@SuppressWarnings("resource")
				HikariDataSource ds = new HikariDataSource();

				// use faster DataSource impl
				ds.setJdbcUrl("jdbc:mysql://tfb-database:3306/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts=true&cacheRSMetadata=true&useSSL=false");
				ds.setDriverClassName("com.mysql.jdbc.Driver");
				ds.setUsername("benchmarkdbuser");
				ds.setPassword("benchmarkdbpass");
				dataSource = ds;
			}
		}
        catch (Exception x)
		{
			throw new RuntimeException("Cannot create the data source", x);
		}

		return dataSource;
	}
}
