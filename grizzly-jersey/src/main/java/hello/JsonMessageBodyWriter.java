package hello;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;

import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.MessageBodyWriter;
import javax.ws.rs.ext.Provider;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.jersey.spi.resource.Singleton;

@Provider
@Singleton
@Produces(APPLICATION_JSON)
public class JsonMessageBodyWriter implements MessageBodyWriter<Object> {
  
  private final ObjectMapper mapper = new ObjectMapper();

  @Override
  public boolean isWriteable(
      final Class<?> type,
      final Type genericType,
      final Annotation[] annotations,
      final MediaType mediaType) {
    return "json".equals(mediaType.getSubtype());
  }

  @Override
  public long getSize(
      final Object t,
      final Class<?> type,
      final Type genericType,
      final Annotation[] annotations,
      final MediaType mediaType) {
    return -1; // We can't predict the output size at this point
  }

  @Override
  public void writeTo(
      final Object t,
      final Class<?> type,
      final Type genericType,
      final Annotation[] annotations,
      final MediaType mediaType,
      final MultivaluedMap<String, Object> httpHeaders,
      final OutputStream entityStream)
      throws IOException, WebApplicationException {
    mapper.writeValue(entityStream, t);
  }
}
