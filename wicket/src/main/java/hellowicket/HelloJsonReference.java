package hellowicket;

import org.apache.wicket.request.resource.*;

public class HelloJsonReference extends ResourceReference
{
  private static final long serialVersionUID = 1L;

  public HelloJsonReference()
  {
    super(HelloJsonReference.class, "json");
  }

  @Override
  public IResource getResource()
  {
    return new HelloJsonResponse();
  }
}
