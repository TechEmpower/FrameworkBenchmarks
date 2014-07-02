package hellowicket;

import hellowicket.plaintext.HelloTextReference;
import hellowicket.dbupdates.HelloDbUpdatesReference;
import hellowicket.fortune.FortunePage;
import org.apache.wicket.RuntimeConfigurationType;
import org.apache.wicket.protocol.http.WebApplication;
import org.apache.wicket.settings.IRequestCycleSettings;

/**
 * Application object for your web application..
 */
public class WicketApplication extends WebApplication
{
	@Override
	public Class<HomePage> getHomePage()
	{
		return HomePage.class;
	}

	@Override
	public void init()
	{
		super.init();

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
	public RuntimeConfigurationType getConfigurationType()
	{
		return RuntimeConfigurationType.DEPLOYMENT;
	}
}
