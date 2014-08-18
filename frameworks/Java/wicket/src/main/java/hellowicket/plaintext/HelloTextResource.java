package hellowicket.plaintext;

import java.nio.charset.Charset;

import org.apache.wicket.request.resource.AbstractResource;

/**
 * A resource that implements the requirements for
 * <a href="http://www.techempower.com/benchmarks/#section=code">Test type 6: Plaintext</a>
 */
public class HelloTextResource extends AbstractResource
{
  private static final long serialVersionUID = 1L;

  private static final String CONTENT_TYPE = "text/plain";
  private static final byte[] DATA = "Hello, World!".getBytes(Charset.forName("UTF-8"));

  protected ResourceResponse newResourceResponse(Attributes attributes)
  {
    ResourceResponse response = new ResourceResponse();
    response.setContentType(CONTENT_TYPE);
    response.setContentLength(DATA.length);
    response.setWriteCallback(new WriteCallback() {
      public void writeData(Attributes attributes)
      {
        attributes.getResponse().write(DATA);
      }
    });
    return response;
  }
}
