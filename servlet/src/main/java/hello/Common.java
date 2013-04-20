package hello;

import org.apache.commons.lang3.*;

import com.fasterxml.jackson.databind.*;

/**
 * Some common functionality and constants used by the Servlet tests.
 */
public class Common
{

  // Constants for setting the content type.
  protected static final String HEADER_CONTENT_TYPE    = "Content-Type";
  protected static final String CONTENT_TYPE_JSON      = "application/json";
  protected static final String CONTENT_TYPE_HTML      = "text/html";

  // Jackson encoder, reused for each response.
  protected static final ObjectMapper MAPPER = new ObjectMapper();

  /**
   * Use the OWASP ESAPI HTML encoder to process an untrusted String into a
   * form suitable for rendering in output HTML.
   */
  public static String render(String input)
  {
    return StringEscapeUtils.escapeHtml4(input);        
  }
  
}
