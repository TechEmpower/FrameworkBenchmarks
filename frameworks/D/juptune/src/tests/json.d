module tests.json;

import juptune.core.ds : ArrayNonShrink;
import juptune.core.util : Result;
import juptune.data.json : Json;
import juptune.http : Http1Writer, Http1Version, Http1MessageSummary;

import tests.common : putServerAndDate, log;

struct JsonHeaderInput
{
    // No header input, it's here just so the overarching logic exists.
}

private struct Message
{
    @Json string message;
}

void handleJson(
    scope ref JsonHeaderInput input, 
    scope ref Http1Writer writer,
    scope ref Http1MessageSummary summary,
) nothrow
{
    import juptune.core.util      : then;
    import juptune.core.util.conv : IntToCharBuffer, toBase10;

    import juptune.data.json : JsonBuilder, jsonSerialise;

    ArrayNonShrink!char buffer;
    buffer.reserve(256);

    ubyte[8] jsonDepth;
    scope put = (scope const(char)[] slice) { buffer.put(slice); return Result.noError; };
    scope builder = JsonBuilder!(typeof(put))(put, jsonDepth);

    auto result = builder.jsonSerialise(Message("Hello, World!"));
    if(result.isError)
    {
        result = writer.putResponseLine(Http1Version.http11, 500, "Internal Error").then!(
            () => writer.putServerAndDate(),
            () => writer.finishHeaders(),
            () => writer.finishBody(),
            () => writer.finishTrailers(),
            () => writer.finishMessage(summary)
        );
        if(result.isError)
        {
            log("writing error response [json] failed: ", result);
            return;
        }

        log("serialising value failed: ", result);
        return;
    }

    IntToCharBuffer contentLengthBuffer;
    const contentLength = toBase10(buffer.length, contentLengthBuffer);

    result = writer.putResponseLine(Http1Version.http11, 200, "OK").then!(
        () => writer.putServerAndDate(),
        () => writer.putHeader("Content-Length", contentLength),
        () => writer.putHeader("Content-Type", "application/json"),
        () => writer.finishHeaders(),
        () => writer.putBody(buffer.slice),
        () => writer.finishBody(),
        () => writer.finishTrailers(),
        () => writer.finishMessage(summary)
    );
    if(result.isError)
    {
        log("writing response [json] failed: ", result);
        return;
    }
}