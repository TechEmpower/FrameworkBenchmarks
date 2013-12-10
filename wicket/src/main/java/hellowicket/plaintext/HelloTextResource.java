package hellowicket.plaintext;

import org.apache.wicket.request.resource.AbstractResource;

/**
 * A resource that implements the requirements for
 * <a href="http://www.techempower.com/benchmarks/#section=code">Test type 6: Plaintext</a>
 */
public class HelloTextResource extends AbstractResource
{
  private static final long serialVersionUID = 1L;

  protected ResourceResponse newResourceResponse(Attributes attributes)
  {
    ResourceResponse response = new ResourceResponse();
    response.setContentType("text/plain");
    response.setContentLength(13);
    response.setWriteCallback(new WriteCallback() {
      public void writeData(Attributes attributes)
      {
        attributes.getResponse().write("Hello, World!");
      }
    });
    return response;
  }
}
