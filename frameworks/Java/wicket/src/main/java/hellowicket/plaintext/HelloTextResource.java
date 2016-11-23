package hellowicket.plaintext;

import org.apache.wicket.request.http.WebResponse;
import org.apache.wicket.request.resource.AbstractResource;

import java.nio.charset.Charset;

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
    response.setWriteCallback(new WriteCallback() {
      public void writeData(Attributes attributes)
      {
        final WebResponse webResponse = (WebResponse) attributes.getResponse();
        webResponse.setContentType(CONTENT_TYPE);
        webResponse.setContentLength(DATA.length);
        webResponse.write(DATA);
      }
    });
    return response;
  }

  @Override
  protected void setResponseHeaders(final ResourceResponse resourceResponse, final Attributes attributes) {
  }
}
