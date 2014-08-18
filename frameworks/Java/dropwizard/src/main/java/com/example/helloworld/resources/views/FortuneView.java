package com.example.helloworld.resources.views;

import com.example.helloworld.db.model.Fortune;
import io.dropwizard.views.View;

import java.nio.charset.StandardCharsets;
import java.util.List;

public class FortuneView extends View {

    private final List<Fortune> fortunes;

    public FortuneView(List<Fortune> fortunes) {
        super("/fortunes.mustache", StandardCharsets.UTF_8);

        this.fortunes = fortunes;
    }

    @SuppressWarnings("unused")
    public List<Fortune> getFortunes() {
        return fortunes;
    }
}
