import os, strtabs, strutils, math, algorithm
import nawak_mongrel, jdump
import model, fortunes_tmpl
when not defined(postgre_model) xor defined(redis_model):
    {.error: "please pass either -d:postgre_model or -d:redis_model to the compiler".}
when defined(postgre_model):
    import model_postgre
when defined(redis_model):
    import model_redis

get "/json":
    var j: THello
    j.message = "Hello, World!"
    # jdump serialize the tuples of the model as json
    return response(jdump(j), "application/json")

get "/plaintext":
    return response("Hello, World!", "text/plain")

get "/db":
    let w = getWorld(random(10_000)+1)
    return response(jdump(w), "application/json")

get "/fortunes":
    var fortunes = getAllFortunes()
    let new_fortune: TFortune =
        (id: 0,
         message: "Additional fortune added at request time.")
    fortunes.add new_fortune
    sort(fortunes, proc(x, y: TFortune): int =
        return cmp(x.message, y.message))

    return response(fortunes_tmpl(fortunes), "text/html; charset=utf-8")


proc limit_queries(query_params: PStringTable): int =
    result = 1
    if query_params.hasKey("queries"):
        try:
            result = parseInt(query_params["queries"])
        except EInvalidValue: discard
        # clamp the number of queries
        if result < 1: result = 1
        elif result > 500: result = 500

get "/queries":
    let queries = limit_queries(request.query)

    var world: seq[TWorld]
    world.newSeq(queries)
    for i in 0.. <queries:
        world[i] = getWorld random(10_000) + 1
    return response(jdump(world), "application/json")

get "/updates":
    let queries = limit_queries(request.query)

    var world: seq[TWorld]
    world.newSeq(queries)
    for i in 0.. <queries:
        world[i] = getWorld(random(10_000) + 1)
        world[i].randomNumber = random(10_000) + 1
        updateWorld world[i]

    return response(jdump(world), "application/json")

custom_page 404:
    # customize the content of the 404 page
    return response(404, """Nah, I've got nothing.<br>
                            Here's a <b>404 Page Not Found</b> error for you.""")

var nb_workers = 32
if paramCount() > 0:
    try:
        nb_workers = paramStr(1).parseInt
    except ValueError:
        echo "Usage: app [number of workers]"
        echo "       Will start with 32 workers by default"
echo "Starting with " & $nb_workers & " workers"

run(init=init_db, nb_threads=nb_workers)
