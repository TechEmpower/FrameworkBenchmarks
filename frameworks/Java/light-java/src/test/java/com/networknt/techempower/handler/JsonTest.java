package com.networknt.techempower.handler;

import com.dslplatform.json.DslJson;
import com.dslplatform.json.JsonWriter;
import com.dslplatform.json.MapConverter;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.networknt.config.Config;
import com.networknt.techempower.model.World;
import org.junit.Test;

import java.util.Collections;
import java.util.Map;

/**
 * Created by steve on 10/02/17.
 */
public class JsonTest {
    private static ObjectMapper mapper = Config.getInstance().getMapper();
    private DslJson<Object> dsl = new DslJson<>();
    private JsonWriter writer = dsl.newWriter(25000);

    @Test
    public void testJson() throws Exception {
        Map map = Collections.singletonMap("message", "Hello, World!");
        long start = System.nanoTime();

        for(int i = 0; i < 1000000; i++) {
            writer.reset();
            MapConverter.serialize(map, writer);
            byte[] bytes = writer.getByteBuffer();
        }
        long stop = System.nanoTime();
        System.out.println("JsonWriter duration = " + (stop - start)/1000);

        start = System.nanoTime();

        for(int i = 0; i < 1000000; i++) {
            byte[] bytes = mapper.writeValueAsBytes(map);
        }

        stop = System.nanoTime();
        System.out.println("Jackson duration = " + (stop - start)/1000);

        World world = new World(12, 30);
        writer.reset();
        world.serialize(writer, true);
        System.out.println("world = " + writer.toString());
    }
}
