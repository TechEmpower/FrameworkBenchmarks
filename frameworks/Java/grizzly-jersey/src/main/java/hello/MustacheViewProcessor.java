package hello;

import static java.nio.charset.StandardCharsets.UTF_8;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;
import com.github.mustachejava.resolver.ClasspathResolver;
import com.sun.jersey.api.view.Viewable;
import com.sun.jersey.spi.resource.Singleton;
import com.sun.jersey.spi.template.ViewProcessor;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import javax.ws.rs.ext.Provider;

@Singleton
@Provider
public class MustacheViewProcessor implements ViewProcessor<Mustache> {

  private final MustacheFactory factory;

  public MustacheViewProcessor() {
    factory = new DefaultMustacheFactory(new ClasspathResolver());
  }

  @Override
  public Mustache resolve(String name) {
    return factory.compile(name);
  }

  @Override
  public void writeTo(Mustache t,
                      Viewable viewable,
                      OutputStream out) throws IOException {
    try (BufferedWriter writer =
             new BufferedWriter(new OutputStreamWriter(out, UTF_8))) {
      t.execute(writer, viewable.getModel());
    }
  }
}
