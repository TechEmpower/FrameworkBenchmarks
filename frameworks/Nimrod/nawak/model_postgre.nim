import strutils
# The following import belongs to the stdlib, but has been updated to support
# queries with parameters (that are safer to counter SQL injections) and
# prepared queries.
# It will be merged eventually. For now, I included it in the repository.
import lib/db_postgres_redone

import model
export model

const qworld = "SELECT id, randomNumber FROM World WHERE id = $1"
const qfortunes = "SELECT id, message FROM Fortune"
const qupdates = "UPDATE World SET randomNumber = $1 WHERE id = $2"

var db {.threadvar.}: TDbConn
var qworld_prepared {.threadvar.}: TPreparedId
var qfortunes_prepared {.threadvar.}: TPreparedId
var qupdates_prepared {.threadvar.}: TPreparedId

proc init_db*() {.procvar.} =
    db = open("", "benchmarkdbuser", "benchmarkdbpass",
              "host=localhost port=5432 dbname=hello_world")
    # prepare queries
    qworld_prepared = db.prepare("world", qworld, 1)
    qfortunes_prepared = db.prepare("fortunes", qfortunes, 0)
    qupdates_prepared = db.prepare("updates", qupdates, 2)


proc getWorld*(n: int): TWorld =
    #let row = db.getRow(qworld, n)
    ## Yes, prepared queries are faster than unprepared ones
    let row = db.getPRow(qworld_prepared, n)
    result.id = parseInt(row[0])
    result.randomNumber = parseInt(row[1])

proc updateWorld*(w: TWorld) =
    db.Exec(qupdates_prepared, $w.randomNumber, $w.id)

proc getAllFortunes*(): seq[TFortune] =
    let rows = db.getAllPRows(qfortunes_prepared)
    result.newSeq(rows.len)
    for j, row in rows.pairs:
        result[j] = (row[0].parseInt, row[1])
