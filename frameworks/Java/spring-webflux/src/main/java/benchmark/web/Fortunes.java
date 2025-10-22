package benchmark.web;

import java.util.List;

import benchmark.model.Fortune;
import io.jstach.jstache.JStache;

@JStache(path = "fortunes.mustache")
public record Fortunes(List<Fortune> fortunes) {
}
