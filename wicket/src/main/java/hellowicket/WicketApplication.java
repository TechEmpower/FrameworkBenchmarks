package hellowicket;

import hellowicket.dbupdates.HelloDbUpdatesReference;
import org.apache.wicket.protocol.http.WebApplication;

/**
 * Application object for your web application. If you want to run this application without deploying, run the Start class.
 * 
 * @see hellowicket.Start#main(String[])
 */
public class WicketApplication extends WebApplication
{
	/**
	 * @see org.apache.wicket.Application#getHomePage()
	 */
	@Override
	public Class<HomePage> getHomePage()
	{
		return HomePage.class;
	}

	/**
	 * @see org.apache.wicket.Application#init()
	 */
	@Override
	public void init()
	{
		super.init();

		// add your configuration here

		// mount the resources under test
		mountResource("/json", new HelloJsonReference());
		mountResource("/db", new HelloDbReference());
		mountResource("/updates", new HelloDbUpdatesReference());

		// disable response caching to be more close to other
		// test applications' behavior
		getRequestCycleSettings().setBufferResponse(false);
	}
}
