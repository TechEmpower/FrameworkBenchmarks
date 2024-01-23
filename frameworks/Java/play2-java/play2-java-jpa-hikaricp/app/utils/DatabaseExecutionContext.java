package utils;

import jakarta.inject.Inject;

import org.apache.pekko.actor.ActorSystem;
import play.libs.concurrent.CustomExecutionContext;

public class DatabaseExecutionContext extends CustomExecutionContext {

    @Inject
    public DatabaseExecutionContext(final ActorSystem actorSystem) {
        super(actorSystem, "database.dispatcher");
    }

}
