package org.smartboot.http;

public enum State {
    verb,
    uri,
    protocol,
    request_line_end,
    head_line,
    head_line_LF,
    head_line_end,
    finished;
}