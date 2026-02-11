package hello;

import org.noear.solon.Solon;
import org.noear.solon.util.ScopeLocalJdk25;

public class Main {
	public static void main(String[] args) {
		Solon.start(Main.class, args, app->{
			app.factories().scopeLocalFactory(ScopeLocalJdk25::new);
		});
	}
}
