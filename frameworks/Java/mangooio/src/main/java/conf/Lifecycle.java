package conf;

import mangoo.io.core.Application;
import mangoo.io.interfaces.MangooLifecycle;

import com.google.inject.Singleton;

import de.svenkubiak.embeddedmongodb.EmbeddedMongo;

@Singleton
public class Lifecycle implements MangooLifecycle {

	@Override
	public void applicationInitialized() {
		if (Application.inTestMode()) {
			EmbeddedMongo.DB.port(29019).start();
		}
	}

	@Override
    public void applicationStarted() {
		// Do nothing for now
	}
}