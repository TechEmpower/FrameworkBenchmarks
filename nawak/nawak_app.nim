import strtabs, strutils, math, algorithm
import nawak_mongrel, jdump
import model, fortunes_tmpl

# The following import belongs to the stdlib, but has been updated to support
# queries with parameters (that are safer to counter SQL injections) and
# prepared queries.
# It will be merged eventually. For now, I included it in the repository.
import lib/db_postgres_redone


var db {.threadvar.}: TDbConn
var qworld_prepared {.threadvar.}: TPreparedId
var qfortunes_prepared {.threadvar.}: TPreparedId
var qupdates_prepared {.threadvar.}: TPreparedId

const qworld = "SELECT id, randomNumber FROM World WHERE id = $1"
const qfortunes = "SELECT id, message FROM Fortune"
const qupdates = "UPDATE World SET randomNumber = $1 WHERE id = $2"

proc init() =
    db = open("", "benchmarkdbuser", "benchmarkdbpass", "host=localhost port=5432 dbname=hello_world")
    # prepare queries
    qworld_prepared = db.prepare("world", qworld, 1)
    qfortunes_prepared = db.prepare("fortunes", qfortunes, 0)
    qupdates_prepared = db.prepare("updates", qupdates, 2)


get "/json":
    var j: THello
    j.message = "Hello, World!"
    # jdump serialize the tuples of the model as json
    return response(jdump(j), "application/json")

get "/plaintext":
    return response("Hello, World!", "text/plain")


get "/db":
    let row = db.getPRow(qworld_prepared, random(10_000)+1)
    var r = unrowTWorld(row)
    return response(jdump(r), "application/json")

#get "/db_unprepared":
#    ## Yes, prepared queries are faster than unprepared ones
#    var r = unrowTWorld( db.getRow(qworld, random(10_000) + 1) )
#    return response(jdump(r), "application/json")

get "/queries":
    var queries = 1
    if request.query.hasKey("queries"):
        try:
            queries = parseInt(request.query["queries"])
        except EInvalidValue: discard
        if queries < 1: queries = 1
        elif queries > 500: queries = 500

    var world: seq[TWorld]
    world.newSeq(queries)
    for i in 0.. <queries:
        let row = db.getPRow(qworld_prepared, random(10_000) + 1)
        world[i] = unrowTWorld(row)
    return response(jdump(world), "application/json")

get "/fortunes":
    let rows = db.getAllPRows(qfortunes_prepared)
    var fortunes: seq[TFortune]
    fortunes.newSeq(rows.len)
    for j, row in rows.pairs:
        fortunes[j] = unrowTFortune(row)
    let new_fortune: TFortune = (id: 0,
                                 message: "Additional fortune added at request time.")
    fortunes.add new_fortune
    sort(fortunes, proc(x, y: TFortune): int =
        return cmp(x.message, y.message))

    return response(fortunes_tmpl(fortunes), "text/html; charset=utf-8")

get "/updates":
    var queries = 1
    if request.query.hasKey("queries"):
        try:
            queries = parseInt(request.query["queries"])
        except EInvalidValue: discard
        if queries < 1: queries = 1
        elif queries > 500: queries = 500

    var world: seq[TWorld]
    world.newSeq(queries)
    for i in 0.. <queries:
        world[i] = unrowTWorld(db.getPRow(qworld_prepared, random(10_000) + 1))
        world[i].randomNumber = random(10_000) + 1
        db.Exec(qupdates_prepared, $world[i].randomNumber, $world[i].id)

    return response(jdump(world), "application/json")

custom_page 404:
    # customize the content of the 404 page
    return response(404, """Nah, I've got nothing.<br>
                            Here's a <b>404 Page Not Found</b> error for you.""")

run(init=init, nb_threads=256)
