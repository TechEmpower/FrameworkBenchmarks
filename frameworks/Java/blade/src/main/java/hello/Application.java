package hello;

import com.blade.Blade;
import hello.model.Message;
import hello.model.World;
import io.netty.buffer.Unpooled;
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

    private static final int    DB_ROWS           = 308;
    private static final byte[] STATIC_HELLO_TEXT = "Hello, World!".getBytes(CharsetUtil.UTF_8);

    private static int getQueries(Optional<Integer> queryCount) {
        int count = queryCount.orElse(1);
        count = count < 1 ? 1 : count;
        count = count > 500 ? 500 : count;
        return count;
    }

    public static void main(String[] args) {
        Blade.me()
                .get("/json", (request, response) -> response.json(new Message()))
                .get("/db", (request, response) -> {
                    final Random random = ThreadLocalRandom.current();
                    response.json(new World().find(random.nextInt(DB_ROWS) + 1));
                })
                .get("/queries", (request, response) -> {
                    int           queries = getQueries(request.queryInt("queries"));
                    final World[] worlds  = new World[queries];
                    final Random  random  = ThreadLocalRandom.current();
                    for (int i = 0; i < queries; i++) {
                        worlds[i] = new World().find(random.nextInt(DB_ROWS) + 1);
                    }
                    response.json(worlds);
                })
                .get("/plaintext", (request, response) -> response.body(Unpooled.unreleasableBuffer(Unpooled.directBuffer().writeBytes(STATIC_HELLO_TEXT)).duplicate()))
                .start(Application.class, args);
    }

}
