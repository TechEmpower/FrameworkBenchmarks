package hellowicket.dbupdates;

import org.apache.wicket.request.resource.IResource;
import org.apache.wicket.request.resource.ResourceReference;

public class HelloDbUpdatesReference extends ResourceReference
{
  private static final long serialVersionUID = 1L;

  private final HelloDbUpdatesResource resource = new HelloDbUpdatesResource();

  public HelloDbUpdatesReference()
  {
    super(HelloDbUpdatesReference.class, "dbupdates");
  }

  @Override
  public IResource getResource()
  {
	  return resource;
  }
}
