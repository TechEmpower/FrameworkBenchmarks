import Base: wait

function streamhandler(router)
    return function (stream::HTTP.Stream)
        request::HTTP.Request = stream.message
        request.body = read(stream)
        closeread(stream)
        request.response::HTTP.Response = try
            router(request)
        catch ex
            Response(500, [], "error")
        end
        request.response.request = request
        try
            startwrite(stream)
            write(stream, request.response.body)
        catch;
        end
        return nothing
    end
end

serve = streamhandler(router)

simple() = HTTP.listen(serve, "0.0.0.0", 8080; max_connections = 1024)

######################### Parallel (Oxygen's StreamUntil)

struct WebRequest
    http::HTTP.Stream
    done::Threads.Event
end

mutable struct Handler
    server
    queue::Channel{WebRequest}
    count::Threads.Atomic{Int}
    shutdown::Threads.Atomic{Bool}
    Handler( queuesize = 1024 ) = begin
        new(nothing, Channel{WebRequest}(queuesize), Threads.Atomic{Int}(0), Threads.Atomic{Bool}(false))
    end
end
wait(h::Handler) = wait(h.server)

function start(host, port, handle_stream, pool=:default, kwargs...)
    n = max(Threads.nthreads(pool) - 1, 1)
    handler = Handler(n * 1024)
    ready_to_accept = Threads.Condition()

    function parallelhandler(stream::HTTP.Stream)
        try
            if handler.count[] < n
                Threads.atomic_add!(handler.count, 1)
                local request = WebRequest(stream, Threads.Event())
                put!(handler.queue, request)
                wait(request.done)
            else
                @warn "Dropping connection..."
                HTTP.setstatus(stream, 500)
                write(stream, "Server overloaded.")
            end 
        catch e
            @error "ERROR: " exception=(e, catch_backtrace())
            HTTP.setstatus(stream, 500)
            write(stream, "The Server encountered a problem")
        end
    end


    function fork()
        id = Threads.threadid()
        id == 1 && return  # Leave thread 1 to handle IO
        id == 2 && return begin 
            server = HTTP.listen!(parallelhandler, host, port, kwargs...)
            lock(ready_to_accept)
            try
                handler.server = server
                notify(ready_to_accept)
            finally
                unlock(ready_to_accept)
            end
            return nothing
        end
        @info "Starting Worker Thread ~ id: $(id)"
        @async while handler.shutdown[] == false
            task = take!(handler.queue)
            Threads.atomic_sub!(handler.count, 1)
            @async begin
                try
                    handle_stream(task.http)
                catch error 
                    @error "ERROR: " exception=(error, catch_backtrace())
                    HTTP.setstatus(task.http, 500)
                    write(task.http, "The Server encountered a problem")
                finally
                    notify(task.done)
                end
            end
        end
    end
    Threads.@threads :static for i = 1:n
        fork()
    end
    lock(ready_to_accept)
    try
        while isnothing(handler.server)
            wait(ready_to_accept)
        end
    finally
        unlock(ready_to_accept)
    end
    handler
end

channelledmultiworker() = begin
    handler = start("0.0.0.0", 8080, serve)
    wait(handler)
end