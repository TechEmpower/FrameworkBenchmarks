import { Insertable, Selectable, Updateable } from 'kysely'

export interface HelloWorldDatabase {
  world: WorldTable
  fortune: FortuneTable
}

export interface WorldTable {
  id: number
  randomnumber: number
}

export type World = Selectable<WorldTable>
export type NewWorld = Insertable<WorldTable>
export type WorldUpdate = Updateable<WorldTable>

export interface FortuneTable {
  id: number
  message: string
}

export type Fortune = Selectable<FortuneTable>
export type NewFortune = Insertable<FortuneTable>
export type FortuneUpdate = Updateable<FortuneTable>