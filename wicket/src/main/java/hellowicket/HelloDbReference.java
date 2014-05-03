package hellowicket;

import org.apache.wicket.request.resource.IResource;
import org.apache.wicket.request.resource.ResourceReference;

public class HelloDbReference extends ResourceReference
{
  private static final long serialVersionUID = 1L;

  private final HelloDbResponse resource = new HelloDbResponse();

  public HelloDbReference()
  {
    super(HelloDbReference.class, "hello_json");
  }

  @Override
  public IResource getResource()
  {
    return resource;
  }
}
