package hellowicket;

import javax.naming.InitialContext;
import javax.sql.DataSource;

import org.apache.wicket.protocol.http.WebApplication;
import org.apache.wicket.settings.IRequestCycleSettings;

import com.zaxxer.hikari.HikariDataSource;

import hellowicket.dbupdates.HelloDbUpdatesReference;
import hellowicket.fortune.FortunePage;
import hellowicket.plaintext.HelloTextReference;

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

		// mount the resources under test
		mountResource("/json", new HelloJsonReference());
		mountResource("/db", new HelloDbReference());
		mountResource("/updates", new HelloDbUpdatesReference());
		mountResource("/plaintext", new HelloTextReference());

		mountPage("/fortunes", FortunePage.class);

		// disable response caching to be more close to other
		// test applications' behavior
		IRequestCycleSettings requestCycleSettings = getRequestCycleSettings();
		requestCycleSettings.setBufferResponse(false);

		// set UTF-8 for /fortunes test
		requestCycleSettings.setResponseRequestEncoding("UTF-8");
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
			Class.forName("com.mysql.jdbc.Driver");

			if (useResinDataSource())
			{
				InitialContext jndiContext = new InitialContext();
				dataSource = (DataSource) jndiContext.lookup("java:comp/env/jdbc/hello_world");
			}
			else
			{
				// use faster DataSource impl
				HikariDataSource ds = new HikariDataSource();
				ds.setJdbcUrl("jdbc:mysql://localhost:3306/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true");
				ds.setDriverClassName("com.mysql.jdbc.Driver");
				ds.setUsername("benchmarkdbuser");
				ds.setPassword("benchmarkdbpass");
				dataSource = ds;
			}
		} catch (Exception x)
		{
			throw new RuntimeException("Cannot create the data source", x);
		}

		return dataSource;
	}
}
