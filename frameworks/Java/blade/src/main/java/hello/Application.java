package hello;

import com.blade.Blade;
import com.blade.server.netty.HttpConst;
import hello.model.Message;
import hello.model.World;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.util.AsciiString;
import io.netty.util.CharsetUtil;

import java.util.Optional;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Blade Application
 *
 * @author biezhi
 * @date 2017/9/22
 */
public class Application {

    private static final int          DB_ROWS                  = 308;
    private static final byte[]       STATIC_PLAINTEXT         = "Hello, World!".getBytes(CharsetUtil.UTF_8);
    private static final ByteBuf      PLAINTEXT_CONTENT_BUFFER = Unpooled.unreleasableBuffer(Unpooled.directBuffer().writeBytes(STATIC_PLAINTEXT));
    private static final CharSequence PLAINTEXT_CLHEADER_VALUE = AsciiString.cached(String.valueOf(STATIC_PLAINTEXT.length));

    private static int getQueries(Optional<String> queryCount) {
        if (!queryCount.isPresent()) {
            return 1;
        }
        int count;
        try {
            count = Integer.parseInt(queryCount.get());
        } catch (NumberFormatException ignored) {
            return 1;
        }
        count = count < 1 ? 1 : count;
        count = count > 500 ? 500 : count;
        return count;
    }

    public static void main(String[] args) {
        Blade.me()
                .get("/json", (request, response) -> {
                    response.contentType(HttpConst.getContentType("application/json"));
                    response.json(new Message());
                })
                .get("/db", (request, response) -> {
                    final Random random = ThreadLocalRandom.current();
                    response.contentType(HttpConst.getContentType("application/json"));
                    response.json(new World().find(random.nextInt(DB_ROWS) + 1));
                })
                .get("/queries", (request, response) -> {
                    int           queries = getQueries(request.query("queries"));
                    final World[] worlds  = new World[queries];
                    final Random  random  = ThreadLocalRandom.current();
                    for (int i = 0; i < queries; i++) {
                        worlds[i] = new World().find(random.nextInt(DB_ROWS) + 1);
                    }
                    response.contentType(HttpConst.getContentType("application/json"));
                    response.json(worlds);
                })
                .get("/plaintext", (request, response) -> {
                    response.contentType(HttpConst.getContentType("text/plain"));
                    response.header(HttpConst.CONTENT_LENGTH, PLAINTEXT_CLHEADER_VALUE);
                    response.body(PLAINTEXT_CONTENT_BUFFER.duplicate());
                })
                .disableSession()
                .start(Application.class, args);
    }

}
