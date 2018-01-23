package fi.markoa.tfb.servlet3;

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
 * Test type 6: Plaintext
 *
 * @author marko asplund
 */
@WebServlet("/plaintext")
public class PlaintextServlet extends HttpServlet {
  private static final Logger LOGGER = LoggerFactory.getLogger(PlaintextServlet.class);
  private static final String MEDIATYPE_TEXT_PLAIN = "text/plain";
  private static final byte[] CONTENT = "Hello, World!".getBytes();

  @Override
  protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
    LOGGER.debug("doGet");
    resp.setContentType(MEDIATYPE_TEXT_PLAIN);
    resp.getOutputStream().write(CONTENT);
  }

}
