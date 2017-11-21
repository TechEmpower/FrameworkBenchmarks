package hellowicket.plaintext;

import java.nio.charset.Charset;

import org.apache.wicket.request.http.WebResponse;
import org.apache.wicket.request.resource.IResource;

/**
 * A resource that implements the requirements for
 * <a href="http://www.techempower.com/benchmarks/#section=code">Test type 6: Plaintext</a>
 */
public class HelloTextResource implements IResource
{
  private static final long serialVersionUID = 1L;

  private static final String CONTENT_TYPE = "text/plain";
  private static final byte[] DATA = "Hello, World!".getBytes(Charset.forName("UTF-8"));

  @Override
  public void respond(Attributes attributes)
  {

    final WebResponse webResponse = (WebResponse) attributes.getResponse();
    webResponse.setContentType(CONTENT_TYPE);
    webResponse.setContentLength(DATA.length);
    webResponse.write(DATA);
  }
}
