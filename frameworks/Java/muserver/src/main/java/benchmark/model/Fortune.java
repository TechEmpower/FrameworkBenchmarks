package benchmark.model;

import java.util.Map;

public record Fortune(
        int id,
        String message
) {
    public Map<String, Object> toMap() {
        return Map.of("id", id, "message", message);
    }
}
