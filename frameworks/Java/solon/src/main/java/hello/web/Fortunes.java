package hello.web;

import hello.model.Fortune;
import io.jstach.jstache.JStache;

import java.util.List;

@JStache(path = "fortunes.mustache")
public record Fortunes(List<Fortune> fortunes) {
}
