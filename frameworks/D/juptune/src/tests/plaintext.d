module tests.plaintext;

import juptune.http : Http1Writer, Http1Version, Http1MessageSummary;

import tests.common : putServerAndDate, log;

struct PlainTextHeaderInput
{
    // No header input, it's here just so the overarching logic exists.
}

void handlePlainText(
    scope ref PlainTextHeaderInput input, 
    scope ref Http1Writer writer,
    scope ref Http1MessageSummary summary,
) nothrow
{
    import juptune.core.util : then;
    import juptune.core.util.conv : IntToCharBuffer, toBase10;

    immutable RESPONSE = "Hello, World!";

    IntToCharBuffer contentLengthBuffer;
    const contentLength = toBase10(RESPONSE.length, contentLengthBuffer);

    auto result = writer.putResponseLine(Http1Version.http11, 200, "OK").then!(
        () => writer.putServerAndDate(),
        () => writer.putHeader("Content-Length", contentLength),
        () => writer.putHeader("Content-Type", "text/plain"),
        () => writer.finishHeaders(),
        () => writer.putBody("Hello, World!"),
        () => writer.finishBody(),
        () => writer.finishTrailers(),
        () => writer.finishMessage(summary)
    );
    if(result.isError)
    {
        log("writing response [plaintext] failed: ", result);
        return;
    }
}