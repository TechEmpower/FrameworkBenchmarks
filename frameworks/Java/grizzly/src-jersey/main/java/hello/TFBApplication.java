package hello;

import javax.inject.Singleton;

import org.glassfish.hk2.utilities.binding.AbstractBinder;
import org.glassfish.jersey.jackson.JacksonFeature;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.server.mvc.mustache.MustacheMvcFeature;
import org.hibernate.SessionFactory;

public class TFBApplication extends ResourceConfig {
	public TFBApplication() {
		super(ServerHeaderFilter.class, JacksonFeature.class, Jackson2MapperProvider.class,
				MustacheMvcFeature.class, PlaintextResource.class, JsonResource.class,
				FortunesResource.class, WorldResource.class);
		property("jersey.config.server.mvc.caching.mustache", "true");
		register(new AbstractBinder() {
			@Override
			protected void configure() {
				bindFactory(SessionFactoryFactory.class).to(SessionFactory.class).in(
						Singleton.class);
			}
		});
	}
}