import { Generated, Insertable, Selectable, Updateable } from "kysely"

export interface Database {
  World: WorldTable
  Fortune: FortuneTable
}

export interface WorldTable {
  id: Generated<number>
  randomnumber: number
}

export type WorldRow = Selectable<WorldTable>
export type NewWorld = Insertable<WorldTable>
export type WorldUpdate = Updateable<WorldTable>

export interface FortuneTable {
  id: Generated<number>
  message: string
}

export type Fortune = Selectable<FortuneTable>
