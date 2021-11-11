package hello;

import com.fasterxml.jackson.databind.*;

import javax.inject.*;
import javax.ws.rs.*;
import javax.ws.rs.core.*;
import javax.ws.rs.ext.*;
import javax.ws.rs.ext.Provider;
import java.io.*;
import java.lang.annotation.*;
import java.lang.reflect.*;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;

@Provider
@Singleton
@Produces(APPLICATION_JSON)
public class JsonMessageBodyWriter
    implements MessageBodyWriter<Object>
{

  private final ObjectMapper mapper = new ObjectMapper();

  @Override
  public boolean isWriteable(final Class<?> type, final Type genericType,
      final Annotation[] annotations, final MediaType mediaType)
  {
    return "json".equals(mediaType.getSubtype());
  }

  @Override
  public long getSize(final Object t, final Class<?> type,
      final Type genericType, final Annotation[] annotations,
      final MediaType mediaType)
  {
    return -1; // We can't predict the output size at this point
  }

  @Override
  public void writeTo(final Object t, final Class<?> type,
      final Type genericType, final Annotation[] annotations,
      final MediaType mediaType,
      final MultivaluedMap<String, Object> httpHeaders,
      final OutputStream entityStream)
      throws IOException, WebApplicationException
  {
    mapper.writeValue(entityStream, t);
  }
}
