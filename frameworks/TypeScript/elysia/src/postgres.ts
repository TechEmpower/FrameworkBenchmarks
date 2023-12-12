import postgres from 'postgres';
import { Fortune, World } from './types';

const sql = postgres({
  host: 'tfb-database',
  user: 'benchmarkdbuser',
  password: 'benchmarkdbpass',
  database: 'hello_world',
  max: 1,
});

export const fortunes = async () =>
  await sql<Fortune[]>`SELECT id, message FROM fortune`;

export const find = async (id: number) =>
  await sql<World[]>`SELECT id, randomNumber FROM world WHERE id = ${id}`.then(
    (arr) => arr[0]
  );

export const bulkUpdate = async (worlds: World[]) =>
  await sql`UPDATE world SET randomNumber = (update_data.randomNumber)::int
  FROM (VALUES ${sql(
    worlds
      .map((world) => [world.id, world.randomNumber])
      .sort((a, b) => (a[0] < b[0] ? -1 : 1))
  )}) AS update_data (id, randomNumber)
  WHERE world.id = (update_data.id)::int`;
