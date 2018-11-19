package benchmark.view;


import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import io.micronaut.core.io.ResourceLoader;
import io.micronaut.core.io.Writable;
import io.micronaut.core.io.scan.ClassPathResourceLoader;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Produces;
import io.micronaut.views.ViewsRenderer;

import javax.annotation.Nullable;
import javax.inject.Singleton;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

@Singleton
@Produces(MediaType.TEXT_HTML)
public class MustacheViewRenderer implements ViewsRenderer {
    protected final ResourceLoader resourceLoader;
    private final Mustache mustache;

    public MustacheViewRenderer(ClassPathResourceLoader resourceLoader) {
        this.resourceLoader = resourceLoader;
        this.mustache = new DefaultMustacheFactory().compile("fortunes.mustache");
    }

    @Override
    public Writable render(String viewName, @Nullable Object data) {
        return out -> mustache.execute(out, data);
    }

    @Override
    public boolean exists(String viewName) {
        return viewName.equals("fortunes");
    }

}
