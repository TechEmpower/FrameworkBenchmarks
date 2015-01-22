import strutils, db_postgres
import model

const qworld = sql"SELECT id, randomNumber FROM World WHERE id = $1"
const qfortunes = sql"SELECT id, message FROM Fortune"
const qupdates = sql"UPDATE World SET randomNumber = $1 WHERE id = $2"

var db {.threadvar.}: TDbConn
var qworld_prepared {.threadvar.}: TSqlPrepared
var qfortunes_prepared {.threadvar.}: TSqlPrepared
var qupdates_prepared {.threadvar.}: TSqlPrepared

proc init_db*() {.procvar.} =
    db = open("", "benchmarkdbuser", "benchmarkdbpass",
              "host=127.0.0.1 port=5432 dbname=hello_world")
    # prepare queries
    qworld_prepared = db.prepare("world", qworld, 1)
    qfortunes_prepared = db.prepare("fortunes", qfortunes, 0)
    qupdates_prepared = db.prepare("updates", qupdates, 2)


proc getWorld*(n: int): TWorld =
    #let row = db.getRow(qworld, n)
    ## Yes, prepared queries are faster than unprepared ones
    let row = db.getRow(qworld_prepared, n)
    result.id = parseInt(row[0])
    result.randomNumber = parseInt(row[1])

proc updateWorld*(w: TWorld) =
    db.exec(qupdates_prepared, $w.randomNumber, $w.id)

proc getAllFortunes*(): seq[TFortune] =
    let rows = db.getAllRows(qfortunes_prepared)
    result.newSeq(rows.len)
    for j, row in rows.pairs:
        result[j] = (row[0].parseInt, row[1])
