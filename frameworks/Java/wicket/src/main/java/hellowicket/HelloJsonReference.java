package hellowicket;

import org.apache.wicket.request.resource.IResource;
import org.apache.wicket.request.resource.ResourceReference;

public class HelloJsonReference extends ResourceReference
{
  private static final long serialVersionUID = 1L;

  private final HelloJsonResponse resource = new HelloJsonResponse();

  public HelloJsonReference()
  {
    super(HelloJsonReference.class, "json");
  }

  @Override
  public IResource getResource()
  {
    return resource;
  }
}
