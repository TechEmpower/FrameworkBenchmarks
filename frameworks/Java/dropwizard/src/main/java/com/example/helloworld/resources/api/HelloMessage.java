package com.example.helloworld.resources.api;

import com.fasterxml.jackson.annotation.JsonProperty;

public final class HelloMessage {

    @JsonProperty
    private final String message;

    public HelloMessage(String m) {
        message = m;
    }

    public String getMessage() {
        return message;
    }
}
