
type THello* = tuple[message: string]
type TWorld* = tuple[id: int, randomNumber: int]
type TFortune* = tuple[id: int, message: string]


import strutils
import lib/db_postgres_redone

proc unrowTWorld*(x: TRow): TWorld =
    result.id = parseInt(x[0])
    result.randomNumber = parseInt(x[1])

proc unrowTFortune*(x: TRow): TFortune =
    return (x[0].parseInt, x[1])
