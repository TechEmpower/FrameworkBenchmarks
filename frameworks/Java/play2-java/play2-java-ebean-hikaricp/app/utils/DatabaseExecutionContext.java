package utils;

import javax.inject.Inject;

import akka.actor.ActorSystem;
import play.libs.concurrent.CustomExecutionContext;

public class DatabaseExecutionContext extends CustomExecutionContext {

    @Inject
    public DatabaseExecutionContext(final ActorSystem actorSystem) {
        super(actorSystem, "database.dispatcher");
    }

}
