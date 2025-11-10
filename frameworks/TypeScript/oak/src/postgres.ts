import postgres from 'postgres';
import type { World, Fortune } from './types.ts';

const sql = postgres({ 
  host: Deno.env.get("PG_HOST"),
  user: Deno.env.get("PG_USER"),
  database: Deno.env.get("PG_NAME"),
  password: Deno.env.get("PG_PSWD"),
  max: 1
});

export function allFortunes() {
  return sql<Fortune[]>`select id, message from fortune`;
}

export function getAllWorlds() {
  return sql<World[]>`SELECT id, randomNumber FROM world`;
}

export async function getWorld(id: number) {
  const result = await sql<World[]>`select id, randomNumber from world where id = ${id}`;
  return result[0];
}

export async function bulkUpdate(worlds: World[]) {
  const values = sql(
    worlds
      .map((world) => [world.id, world.randomnumber])
      .sort((a, b) => (a[0] < b[0] ? -1 : 1))
  );

  await sql`update world set randomNumber = (update_data.randomNumber)::int
    from (values ${values}) as update_data (id, randomNumber)
    where world.id = (update_data.id)::int`;
}
