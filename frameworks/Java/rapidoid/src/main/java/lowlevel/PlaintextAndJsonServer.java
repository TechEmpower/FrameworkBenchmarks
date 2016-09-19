package lowlevel;

import common.Message;
import org.rapidoid.buffer.Buf;
import org.rapidoid.data.BufRange;
import org.rapidoid.data.BufRanges;
import org.rapidoid.http.AbstractHttpServer;
import org.rapidoid.http.HttpStatus;
import org.rapidoid.http.MediaType;
import org.rapidoid.net.abstracts.Channel;

public class PlaintextAndJsonServer extends AbstractHttpServer {

	private static final byte[] URI_PLAINTEXT = "/plaintext".getBytes();

	private static final byte[] URI_JSON = "/json".getBytes();

	private static final byte[] HELLO_WORLD = "Hello, World!".getBytes();

	public PlaintextAndJsonServer() {
		super("X", "", "", false);
	}

	@Override
	protected HttpStatus handle(Channel ctx, Buf buf, BufRange verb, BufRange uri, BufRange path, BufRange query,
	                            BufRange protocol, BufRanges headers, boolean isGet, boolean isKeepAlive, BufRange body) {

		if (isGet) {
			if (matches(buf, path, URI_PLAINTEXT)) {
				return ok(ctx, isKeepAlive, HELLO_WORLD, MediaType.TEXT_PLAIN);

			} else if (matches(buf, path, URI_JSON)) {
				return serializeToJson(ctx, isKeepAlive, new Message("Hello, World!"));
			}
		}

		return HttpStatus.NOT_FOUND;
	}

}
