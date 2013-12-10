package hellowicket.plaintext;

import org.apache.wicket.request.resource.IResource;
import org.apache.wicket.request.resource.ResourceReference;

public class HelloTextReference extends ResourceReference
{
  private static final long serialVersionUID = 1L;

  private final HelloTextResource resource = new HelloTextResource();

  public HelloTextReference()
  {
    super(HelloTextReference.class, "plaintext");
  }

  @Override
  public IResource getResource()
  {
    return resource;
  }
}
