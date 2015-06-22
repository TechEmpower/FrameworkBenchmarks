package hello;

/*
 * #%L
 * rapidoid-demo
 * %%
 * Copyright (C) 2014 - 2015 Nikolche Mihajlovski
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import org.rapidoid.buffer.Buf;
import org.rapidoid.bytes.BytesUtil;
import org.rapidoid.data.Range;
import org.rapidoid.data.Ranges;
import org.rapidoid.http.HttpParser;
import org.rapidoid.net.Protocol;
import org.rapidoid.net.abstracts.Channel;
import org.rapidoid.net.impl.RapidoidHelper;
import org.rapidoid.util.Dates;
import org.rapidoid.wrap.BoolWrap;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;

public class SimpleHttpProtocol implements Protocol {

	private static final byte[] HTTP_200_OK = "HTTP/1.1 200 OK\r\n".getBytes();

	private static final byte[] HTTP_404_NOT_FOUND = "HTTP/1.1 404 Not Found\r\nContent-Length: 10\r\n\r\nNot found!"
			.getBytes();

	private static final byte[] CONN_KEEP_ALIVE = "Connection: keep-alive\r\n"
			.getBytes();

	private static final byte[] CONN_CLOSE = "Connection: close\r\n".getBytes();

	private static final byte[] SERVER_X = "Server: X\r\n".getBytes();

	private static final byte[] CONTENT_LENGTH_HDR = "Content-Length: "
			.getBytes();

	private static final byte[] CONTENT_TYPE_PLAIN = "Content-Type: text/plain; charset=UTF-8\r\n"
			.getBytes();

	private static final byte[] CONTENT_TYPE_JSON = "Content-Type: application/json; charset=UTF-8\r\n"
			.getBytes();

	private static final byte[] CONTENT_LENGTH = "Content-Length:           "
			.getBytes();

	private static final byte[] RESPONSE = "Hello, World!".getBytes();

	private static final byte[] DATE_HDR = "Date: ".getBytes();

	private static final byte[] RESPONSE_LENGTH = String.valueOf(
			RESPONSE.length).getBytes();

	private static final byte[] URI_PLAIN = "/plaintext".getBytes();

	private static final byte[] URI_JSON = "/json".getBytes();

	private static final HttpParser HTTP_PARSER = new HttpParser();

	public static final ObjectMapper MAPPER = mapper();

	private static ObjectMapper mapper() {
		ObjectMapper mapper = new ObjectMapper();
		mapper.registerModule(new AfterburnerModule());
		return mapper;
	}

	public void process(Channel ctx) {
		if (ctx.isInitial()) {
			return;
		}

		Buf buf = ctx.input();
		RapidoidHelper helper = ctx.helper();

		Range[] ranges = helper.ranges1.ranges;
		Ranges headers = helper.ranges2;

		BoolWrap isGet = helper.booleans[0];
		BoolWrap isKeepAlive = helper.booleans[1];

		Range verb = ranges[ranges.length - 1];
		Range uri = ranges[ranges.length - 2];
		Range path = ranges[ranges.length - 3];
		Range query = ranges[ranges.length - 4];
		Range protocol = ranges[ranges.length - 5];
		Range body = ranges[ranges.length - 6];

		HTTP_PARSER.parse(buf, isGet, isKeepAlive, body, verb, uri, path,
				query, protocol, headers, helper);

		response(ctx, buf, path, isGet.value, isKeepAlive.value);
	}

	private void response(Channel ctx, Buf buf, Range path, boolean isGet,
			boolean isKeepAlive) {
		boolean processed = false;

		if (isGet) {

			ctx.write(HTTP_200_OK);

			ctx.write(isKeepAlive ? CONN_KEEP_ALIVE : CONN_CLOSE);

			ctx.write(SERVER_X);

			ctx.write(DATE_HDR);
			ctx.write(Dates.getDateTimeBytes());
			ctx.write(CR_LF);

			if (BytesUtil.matches(buf.bytes(), path, URI_PLAIN, true)
					|| path.length == 1) {
				handlePlaintext(ctx);
				processed = true;
			} else if (BytesUtil.matches(buf.bytes(), path, URI_JSON, true)) {
				handleJson(ctx);
				processed = true;
			}

			ctx.closeIf(!isKeepAlive);
		}

		if (!processed) {
			ctx.write(HTTP_404_NOT_FOUND);
			ctx.close();
		}
	}

	private void handlePlaintext(Channel ctx) {
		ctx.write(CONTENT_LENGTH_HDR);
		ctx.write(RESPONSE_LENGTH);
		ctx.write(CR_LF);

		ctx.write(CONTENT_TYPE_PLAIN);
		ctx.write(CR_LF);
		ctx.write(RESPONSE);
	}

	private void handleJson(Channel ctx) {
		Buf output = ctx.output();

		ctx.write(CONTENT_TYPE_JSON);
		ctx.write(CONTENT_LENGTH);

		int posConLen = output.size() - 10;
		ctx.write(CR_LF);
		ctx.write(CR_LF);

		int posBefore = output.size();

		Message msg = new Message("Hello, World!");
		try {
			MAPPER.writeValue(output.asOutputStream(), msg);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

		int posAfter = output.size();
		output.putNumAsText(posConLen, posAfter - posBefore, false);
	}

}
