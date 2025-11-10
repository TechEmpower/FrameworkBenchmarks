import core.time : msecs;
import std.parallelism : totalCPUs;
import std.process : environment;

import  juptune.core.util, 
        juptune.core.ds, 
        juptune.event, 
        juptune.event.fiber, 
        juptune.http;

import tests.common : log;
import tests.plaintext, tests.json;

/++++ Constant config ++++/

enum SOCKET_BACKLOG_PER_THREAD  = 1000;
enum FIBER_CALL_STACK_BYTES     = 1024 * 100;
enum HTTP_READ_BUFFER_BYTES     = 1024;
enum HTTP_WRITE_BUFFER_BYTES    = 1024;

enum HTTP_CONFIG = Http1Config()
                    .withReadTimeout(1000.msecs)
                    .withWriteTimeout(1000.msecs);

static assert(
    HTTP_READ_BUFFER_BYTES + HTTP_WRITE_BUFFER_BYTES < FIBER_CALL_STACK_BYTES / 4,
    "To be safe, please ensure the buffer bytes are only a quarter the size of a fiber call stack."
);

/++++ Globals ++++/

__gshared TcpSocket server; // Currently there's no mechanism to directly pass data to new threads, so global state has to be used.

/++++ Functions ++++/

void main()
{
    auto loop = EventLoop(
        EventLoopConfig()
        .withFiberAllocatorConfig(
            FiberAllocatorConfig()
            .withBlockStackSize(FIBER_CALL_STACK_BYTES)
        )
    );

    // open() and listen() can't be ran outside of an event loop thread, so currently this is the janky way to setup the server.
    loop.addNoGCThread(() @nogc nothrow {
        server.open().resultAssert;
        server.listen("0.0.0.0:8080", SOCKET_BACKLOG_PER_THREAD*totalCPUs).resultAssert;
        juptuneEventLoopCancelThread();
    });
    loop.join();

    // Then we can setup the proper loop threads.
    foreach(i; 0..totalCPUs)
        loop.addGCThread(&router);
    loop.join();
}

// Juptune currently does not provide higher-level server features out of the box, so we have
// to hand-make a custom router.
//
// This is realistic in the sense that building a custom router is a completely valid, supported pattern
// for people who want/need something very specialised.
//
// This is unrealistic in the sense that once Juptune has a native router, the native router would
// almost certainly be used in a case like this (but since that's a TODO, this will have to do for now).
void router() nothrow
{
    try
    {
        enum Route
        {
            FAILSAFE,
            plaintext,
            json,
        }

        enum Method
        {
            FAILSAFE,
            get
        }

        union RouteInput
        {
            PlainTextHeaderInput plaintext;
            JsonHeaderInput json;
        }

        while(!juptuneEventLoopIsThreadCanceled())
        {
            TcpSocket client;

            auto result = server.accept(client);
            if(result.isError)
            {
                log("error accepting socket: ", result);
                continue;
            }

            result = async(function () nothrow {
                auto client = juptuneEventLoopGetContext!TcpSocket;
                scope(exit) if(client.isOpen)
                    auto _ = client.close();

                Http1MessageSummary readSummary, writeSummary;

                // Read & Write primitives
                ubyte[HTTP_READ_BUFFER_BYTES] readBuffer;
                ubyte[HTTP_WRITE_BUFFER_BYTES] writeBuffer;
                auto reader = Http1Reader(client, readBuffer, HTTP_CONFIG);
                auto writer = Http1Writer(client, writeBuffer, HTTP_CONFIG);

                do
                {
                    if(!client.isOpen)
                        return;

                    // Routing state
                    Route route;
                    Method method;
                    RouteInput input;

                    // Error handling
                    uint errorCode;
                    string errorMsg;
                    void setError(uint code, string msg)
                    {
                        if(errorMsg !is null)
                            return;
                        errorCode = code;
                        errorMsg = msg;
                    }

                    // Parse request line
                    {
                        Http1RequestLine requestLine;
                        auto result = reader.readRequestLine(requestLine);
                        if(result.isError)
                        {
                            log("readRequestLine() failed: ", result.error, ": ", result.context.slice);
                            return;
                        }

                        requestLine.access((scope methodString, scope uri){
                            switch(methodString)
                            {
                                case "GET":
                                    method = Method.get;
                                    break;

                                default:
                                    setError(405, "Unexpected method");
                                    break;
                            }

                            switch(uri.path)
                            {
                                case "/plaintext":
                                    route = Route.plaintext;
                                    break;

                                case "/json":
                                    route = Route.json;
                                    break;

                                default:
                                    setError(404, "Not found");
                                    break;
                            }
                        });
                    }

                    // Read headers
                    bool foundEndOfHeaders;
                    while(!foundEndOfHeaders)
                    {
                        auto result = reader.checkEndOfHeaders(foundEndOfHeaders);
                        if(result.isError)
                        {
                            log("checkEndOfHeaders() failed: ", result);
                            return;
                        }
                        else if(foundEndOfHeaders)
                            break;

                        Http1Header header;
                        result = reader.readHeader(header);
                        if(result.isError)
                        {
                            log("readHeader() failed: ", result);
                            return;
                        }

                        // Since we're using a custom router, we have the luxury of handling/ignoring headers during routing rather
                        // than stuffing them all into a hashmap, and doing the processing post-routing.
                        header.access((scope name, scope value){
                            final switch(route) with(Route)
                            {
                                case FAILSAFE: break;
                                
                                case plaintext:
                                    break;

                                case json:
                                    break;
                            }
                        });
                    }

                    // Read body
                    Http1BodyChunk chunk;
                    do {
                        chunk = Http1BodyChunk();
                        auto result = reader.readBody(chunk);
                        if(result.isError)
                        {
                            log("readBody() failed: ", result);
                            return;
                        }

                        // Likewise, we only need to deal with body data in certain routes, so we can ignore them in others.
                        chunk.access((scope data){
                            final switch(route) with(Route)
                            {
                                case FAILSAFE: break;
                                
                                case plaintext:
                                    break;

                                case json:
                                    break;
                            }
                        });
                    } while(chunk.hasDataLeft);

                    // Finish reading the message, and either dispatch it to a handler, or report an error back.
                    auto result = reader.finishMessage(readSummary);
                    if(result.isError)
                    {
                        log("finishMessage() failed: ", result);
                        return;
                    }

                    if(errorMsg !is null)
                    {
                        import tests.common : putServerAndDate;
                        result = writer.putResponseLine(Http1Version.http11, errorCode, errorMsg).then!(
                            () => writer.putServerAndDate(),
                            () => writer.finishHeaders(),
                            () => writer.finishBody(),
                            () => writer.finishTrailers(),
                            () => writer.finishMessage(writeSummary),
                        );
                        if(result.isError)
                        {
                            log("finishing a message [error variant] failed: ", result);
                            return;
                        }
                        continue;
                    }

                    final switch(route) with(Route)
                    {
                        case FAILSAFE: break;
                        
                        case plaintext:
                            handlePlainText(input.plaintext, writer, writeSummary);
                            break;

                        case json:
                            handleJson(input.json, writer, writeSummary);
                            break;
                    }
                } while(!readSummary.connectionClosed && !writeSummary.connectionClosed);
            }, client, &asyncMoveSetter!TcpSocket);
            if(result.isError)
            {
                log("error calling async(): ", result);
                continue;
            }
        }
    }
    catch(Throwable ex) // @suppress(dscanner.suspicious.catch_em_all)
    {
        import std.exception : assumeWontThrow;
        log("uncaught exception: ", ex.msg).assumeWontThrow;
        debug log(ex.info).assumeWontThrow;
    }
}