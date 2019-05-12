package conf;

import servletfilter.DateHeaderFilter;
import ninja.servlet.NinjaServletDispatcher;

public class ServletModule extends com.google.inject.servlet.ServletModule {

    @Override
    protected void configureServlets() {
        bind(DateHeaderFilter.class).asEagerSingleton();
        bind(NinjaServletDispatcher.class).asEagerSingleton();

        filter("/*").through(DateHeaderFilter.class);
        serve("/*").with(NinjaServletDispatcher.class);
    }

}