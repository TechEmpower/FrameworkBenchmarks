import { Kysely, PostgresDialect } from "kysely"
import { Pool } from "pg"
import { Database, WorldRow } from "./schema.js"

export const db = new Kysely<Database>({
  dialect: new PostgresDialect({
    pool: new Pool({ connectionString: process.env.DATABASE_URL }),
  }),
})

export type World = {
  [key in keyof WorldRow as key extends "randomnumber" ? "randomNumber" : key]: WorldRow[key]
}

export async function findWorld(id: number): Promise<World | undefined> {
  return db.selectFrom("World").
    where("id", "=", id).
    select(["id", "randomnumber as randomNumber"]).
    executeTakeFirst()
}

export async function upsertWorlds(worlds: World[]) {
  const values = worlds.map(world => ({ id: world.id, randomnumber: world.randomNumber }))
  return db.insertInto("World").values(values).onConflict(oc =>
    oc.column("id").doUpdateSet({ randomnumber: eb => eb.ref("excluded.randomnumber") })
  ).execute()
}
