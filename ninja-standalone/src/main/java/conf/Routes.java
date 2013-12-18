package conf;

import hello.controllers.HelloDbController;
import hello.controllers.HelloFortuneController;
import hello.controllers.HelloJsonController;
import hello.controllers.HelloPlaintextController;
import ninja.Router;
import ninja.application.ApplicationRoutes;

public class Routes implements ApplicationRoutes {

    @Override
    public void init(Router router) {

	router.GET().route("/plaintext").with(HelloPlaintextController.class, "index");
	router.GET().route("/json").with(HelloJsonController.class, "index");
	router.GET().route("/queries").with(HelloDbController.class, "multiGet");
	router.GET().route("/db").with(HelloDbController.class, "singleGet");
	router.GET().route("/fortunes").with(HelloFortuneController.class, "index");
        router.GET().route("/update").with(HelloDbController.class, "update");

    }
}