package micronaut;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;
import io.micronaut.context.annotation.Bean;
import io.micronaut.context.annotation.Factory;

@Factory
public class TemplateFactory {

    @Bean
    public Mustache template() {
        MustacheFactory mf = new DefaultMustacheFactory();
        return mf.compile("fortunes.mustache");
    }
}
