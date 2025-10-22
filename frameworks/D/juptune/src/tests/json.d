module tests.json;

import juptune.core.ds : ArrayNonShrink;
import juptune.core.util : Result;
import juptune.http : Http1Writer, Http1Version, Http1MessageSummary;

import tests.common : putServerAndDate, log;

struct JsonHeaderInput
{
    // No header input, it's here just so the overarching logic exists.
}

private struct Message
{
    string message;
}

void handleJson(
    scope ref JsonHeaderInput input, 
    scope ref Http1Writer writer,
    scope ref Http1MessageSummary summary,
) nothrow
{
    import juptune.core.util      : then;
    import juptune.core.util.conv : IntToCharBuffer, toBase10;

    ArrayNonShrink!char buffer;
    buffer.reserve(256);

    auto result = serialise(buffer, Message("Hello, World!"));
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

// There's currently no built-in serialiser, however because of D's incredibly powerful metaprogramming
// this watered-down serialiser would likely generate almost the exact same code as a full-blown serialiser
// in this simple case.
private Result serialise(T)(scope ref ArrayNonShrink!char buffer, T value)
if(is(T == struct))
{
    import juptune.data.json : JsonBuilder;
    scope append = (scope const(char)[] text) {
        buffer.put(text);
        return Result.noError;
    };

    ubyte[8] depthBuffer;
    auto builder = JsonBuilder!(typeof(append))(append, depthBuffer[]);

    auto result = builder.startObject();
    if(result.isError)
        return result;

    static foreach(fieldSymbol; value.tupleof)
    {{
        immutable FieldName = __traits(identifier, fieldSymbol);

        result = builder.putObjectValue(FieldName, mixin("value."~FieldName));
        if(result.isError)
            return result;
    }}

    result = builder.endObject();
    if(result.isError)
        return result;
    return builder.finish();
}