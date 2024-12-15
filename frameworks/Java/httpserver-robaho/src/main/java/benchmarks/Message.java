package benchmarks;

import java.io.IOException;
import java.io.Writer;
import java.util.Map;

import org.json.simple.JSONStreamAware;
import org.json.simple.JSONValue;

public class Message implements JSONStreamAware {

    private final String message;

    public Message(String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }

    @Override
    public void writeJSONString(Writer out) throws IOException {
        JSONValue.writeJSONString(Map.of("message",message), out);
    }
}
