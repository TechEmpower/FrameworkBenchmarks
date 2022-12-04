package benchmark;

import com.dslplatform.json.DslJson;
import com.dslplatform.json.runtime.Settings;
import io.javalin.http.ContentType;
import io.javalin.http.Context;
import io.javalin.json.JsonMapper;
import org.jetbrains.annotations.NotNull;

import java.io.*;

import java.lang.reflect.Type;
import java.util.Objects;

public class DslJsonMapper {

    public static DslJson<Object> dslJson = new DslJson<>(
        Settings
            .withRuntime()
            .allowArrayFormat(true)
            .includeServiceLoader()
    );

    public void writeJson(Object obj, Context context) {
        try {
            context.contentType(ContentType.APPLICATION_JSON);
            dslJson.serialize(obj, context.res().getOutputStream());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

}
