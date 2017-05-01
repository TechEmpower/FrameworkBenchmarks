package lowlevel;

import org.rapidoid.config.Conf;
import org.rapidoid.setup.App;

public class Main {

	public static void main(String[] args) {
		App.run(args);

		Conf.HTTP.set("maxPipeline", 128);
		Conf.HTTP.set("timeout", 0);

		new PlaintextAndJsonServer().listen(8080);
	}

}
