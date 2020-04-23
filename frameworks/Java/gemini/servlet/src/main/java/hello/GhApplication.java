package hello;

import com.techempower.data.*;
import com.techempower.data.jdbc.*;
import com.techempower.*;
import com.techempower.gemini.*;
import com.techempower.gemini.exceptionhandler.*;
import com.techempower.gemini.path.*;
import com.techempower.js.*;
import com.techempower.js.legacy.*;

import hello.home.entity.*;
import hello.home.handler.*;

/**
 * GeminiHello Application.  As a subclass of GeminiApplication, this
 * class acts as the central "hub" of references to components such as
 * the Dispatcher and EntityStore.
 *
 * Development history:
 *   2012-04-19 - mh - Created
 *   2020-04-17 - ms - Updated to Gemini 3.0.2
 *
 * @author mhixson
 */
public class GhApplication
     extends ResinGeminiApplication
{
  /**
   * Constructs a Dispatcher.
   */
  @Override
  protected Dispatcher constructDispatcher()
  {
    final PathDispatcher.Configuration<Context> config = new PathDispatcher.Configuration<>();

    config.setDefault(new HelloHandler(this))
          .add(new BasicExceptionHandler(this));

    return new PathDispatcher<>(this, config);
  }

  @Override
  protected ConnectorFactory constructConnectorFactory()
  {
    return new BasicConnectorFactory(this, null);
  }

}
