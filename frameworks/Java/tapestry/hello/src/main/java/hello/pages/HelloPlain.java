package hello.pages;

import org.apache.tapestry5.StreamResponse;
import org.apache.tapestry5.annotations.PageLoaded;
import org.apache.tapestry5.util.TextStreamResponse;

/**
 * Plain-text response test
 */
public class HelloPlain
{
  private TextStreamResponse response;
  
  @PageLoaded
  void initializeResponse(){
    response = new TextStreamResponse("text/plain", "Hello, World!");
  }

  StreamResponse onActivate() {
    return response;
  }
}
