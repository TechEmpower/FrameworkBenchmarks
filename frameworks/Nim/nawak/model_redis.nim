import strutils, redis
import model

var db {.threadvar.}: TRedis

proc init_db*() {.procvar.} =
    db = open(host="foobar")

proc getWorld*(id: int): TWorld =
    let s = redis.get(db, "world:" & $id)
    if s == redisNil:
        raise newException(ERedis, "World Not Found")
    return (id, s.parseInt)

proc updateWorld*(w: TWorld) =
    db.setk("world:" & $w.id, $w.randomNumber)

proc getAllFortunes*(): seq[TFortune] =
    result = @[]
    var i = 1
    for s in db.lrange("fortunes", 0, -1):
        result.add((id: i, message: s))
        inc(i)
