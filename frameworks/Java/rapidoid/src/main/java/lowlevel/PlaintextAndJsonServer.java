package lowlevel;

import common.Message;
import org.rapidoid.buffer.Buf;
import org.rapidoid.http.AbstractHttpServer;
import org.rapidoid.http.HttpStatus;
import org.rapidoid.http.HttpUtils;
import org.rapidoid.http.MediaType;
import org.rapidoid.net.abstracts.Channel;
import org.rapidoid.net.impl.RapidoidHelper;

public class PlaintextAndJsonServer extends AbstractHttpServer {

	private static final byte[] URI_PLAINTEXT = "/plaintext".getBytes();

	private static final byte[] URI_JSON = "/json".getBytes();

	private static final byte[] HELLO_WORLD = "Hello, World!".getBytes();

	public PlaintextAndJsonServer() {
		super("X", "", "", false);
	}

	@Override
	protected HttpStatus handle(Channel ctx, Buf buf, RapidoidHelper data) {

		if (data.isGet.value) {
			if (matches(buf, data.path, URI_PLAINTEXT)) {
				return ok(ctx, data.isKeepAlive.value, HELLO_WORLD, MediaType.TEXT_PLAIN);

			} else if (matches(buf, data.path, URI_JSON)) {
				return serializeToJson(HttpUtils.noReq(), ctx, data.isKeepAlive.value, new Message("Hello, World!"));
			}
		}

		return HttpStatus.NOT_FOUND;
	}

}
