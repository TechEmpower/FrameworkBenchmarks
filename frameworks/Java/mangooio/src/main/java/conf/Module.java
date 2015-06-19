package conf;

import mangoo.io.interfaces.MangooLifecycle;

import com.google.inject.AbstractModule;
import com.google.inject.Singleton;

@Singleton
public class Module extends AbstractModule {
    
	@Override
    protected void configure() {
        bind(MangooLifecycle.class).to(Lifecycle.class);
    }
}