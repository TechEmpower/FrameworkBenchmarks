package benchmark.model;

import io.jstach.jstache.JStache;
import java.util.List;

@JStache(path = "fortunes.mustache")
public record FortuneTemplate(List<Fortune> fortunes) {}
