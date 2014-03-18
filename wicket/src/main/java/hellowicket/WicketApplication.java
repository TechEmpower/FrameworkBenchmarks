package hellowicket;

import java.io.IOException;
import java.util.Properties;

import javax.sql.DataSource;

import org.apache.wicket.protocol.http.WebApplication;
import org.apache.wicket.settings.IRequestCycleSettings;

import com.jolbox.bonecp.BoneCPDataSource;

import hellowicket.dbupdates.HelloDbUpdatesReference;
import hellowicket.fortune.FortunePage;
import hellowicket.plaintext.HelloTextReference;

/**
 * Application object for your web application..
 */
public class WicketApplication extends WebApplication
{
	private BoneCPDataSource db;

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
		db.close();
		super.onDestroy();
	}

//	@Override
//	public RuntimeConfigurationType getConfigurationType()
//	{
//		return RuntimeConfigurationType.DEVELOPMENT;
//	}

	public static WicketApplication get()
	{
		return (WicketApplication) WebApplication.get();
	}

	public DataSource getDataSource()
	{
		return db;
	}

	private BoneCPDataSource newDataSource()
	{
		try
		{
			Class.forName("com.mysql.jdbc.Driver");
		} catch (ClassNotFoundException e)
		{
			throw new RuntimeException("Cannot load MySQL JDBC driver", e);
		}
		BoneCPDataSource ds = new BoneCPDataSource();
		Properties settings = loadSettings();
		ds.setJdbcUrl(settings.getProperty("mysql.uri"));
		ds.setUsername(settings.getProperty("mysql.user"));
		ds.setPassword(settings.getProperty("mysql.password"));

		return ds;
	}

	private Properties loadSettings()
	{
		ClassLoader classLoader = WicketApplication.class.getClassLoader();
		Properties settings = new Properties();

		try
		{
			if (usesDeploymentConfig())
			{
				settings.load(classLoader.getResourceAsStream("prod.properties"));
			}
			else
			{
				settings.load(classLoader.getResourceAsStream("dev.properties"));
			}
		} catch (IOException e)
		{
			throw new RuntimeException("Cannot load the settings!", e);
		}
		return settings;
	}
}
