package hello;

import javax.ws.rs.core.*;
import javax.ws.rs.core.Response.*;
import javax.ws.rs.ext.*;

public class RequestExceptionMapper
    implements ExceptionMapper<Exception>
{
//  private static final Logger LOGGER = LoggerFactory.getLogger(
//      RequestExceptionMapper.class);

  @Override
  public Response toResponse(Exception exception)
  {
    exception.printStackTrace();
    System.err.println(exception.toString());
    return Response.status(Status.NOT_FOUND).build();
  }
}
