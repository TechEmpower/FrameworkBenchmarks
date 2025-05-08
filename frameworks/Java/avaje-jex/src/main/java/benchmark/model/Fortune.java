package benchmark.model;

import io.avaje.jsonb.Json;

@Json
public record Fortune(int id, String message) {}
