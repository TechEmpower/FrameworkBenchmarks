package hello;

import static javax.ws.rs.core.MediaType.*;

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

@Provider
@Produces(APPLICATION_JSON)
public class JsonMessageBodyWriter implements MessageBodyWriter<Object> {
  private static final ObjectMapper mapper = new ObjectMapper();

  @Override
  public boolean isWriteable(
      Class<?> type,
      Type genericType,
      Annotation[] annotations,
      MediaType mediaType) {
    return APPLICATION_JSON_TYPE.equals(mediaType);
  }

  @Override
  public long getSize(
      Object t,
      Class<?> type,
      Type genericType,
      Annotation[] annotations,
      MediaType mediaType) {
    return -1; // We can't predict the output size at this point
  }

  @Override
  public void writeTo(
      Object t,
      Class<?> type,
      Type genericType,
      Annotation[] annotations,
      MediaType mediaType,
      MultivaluedMap<String, Object> httpHeaders,
      OutputStream entityStream)
      throws IOException, WebApplicationException {
    mapper.writeValue(entityStream, t);
  }
}
