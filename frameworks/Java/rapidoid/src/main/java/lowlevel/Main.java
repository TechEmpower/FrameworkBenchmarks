package lowlevel;

import org.rapidoid.net.TCP;

public class Main {

	public static void main(String[] args) throws Exception {
		TCP.server()
			.protocol(new PlaintextAndJsonServer())
			.port(8080)
			.syncBufs(false)
			.build()
			.start();
	}

}
