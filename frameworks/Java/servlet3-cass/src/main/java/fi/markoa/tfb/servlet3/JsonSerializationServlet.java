package fi.markoa.tfb.servlet3;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * Web Framework Benchmarks
 * Test type 1: JSON serialization
 *
 * @author marko asplund
 */
@WebServlet("/json")
public class JsonSerializationServlet extends HttpServlet {
  private static final Logger LOGGER = LoggerFactory.getLogger(JsonSerializationServlet.class);
  private static final ObjectMapper mapper = new ObjectMapper();
  private static final String MEDIATYPE_APPLICATION_JSON = "application/json";

  @Override
  protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
    LOGGER.debug("doGet");
    resp.setContentType(MEDIATYPE_APPLICATION_JSON);
    mapper.writeValue(resp.getOutputStream(), new HelloMessage());
  }

}
