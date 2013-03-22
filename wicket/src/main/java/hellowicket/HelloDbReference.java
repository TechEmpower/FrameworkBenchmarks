package hellowicket;

import org.apache.wicket.request.resource.*;

public class HelloDbReference extends ResourceReference
{
  private static final long serialVersionUID = 1L;

  public HelloDbReference()
  {
    super(HelloDbReference.class, "hello_json");
  }

  @Override
  public IResource getResource()
  {
    return new HelloDbResponse();
  }
}
