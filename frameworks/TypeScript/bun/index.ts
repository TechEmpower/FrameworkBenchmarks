import { Elysia, t } from 'elysia';
import { db } from './db/database';
import { sql } from 'kysely';
import Ajv, { JTDSchemaType } from 'ajv/dist/jtd';
import { Promise as BPromise } from 'bluebird';

const ajv = new Ajv();

interface helloMessage {
  message: string;
}

const messageSchema: JTDSchemaType<helloMessage> = {
  properties: {
    message: { type: 'string' },
  },
};

const messageSerialize = ajv.compileSerializer(messageSchema);

interface message {
  id: number;
  message: string;
}

interface returnWorld {
  id: number,
  randomNumber: number,
}

const worldSchema: JTDSchemaType<returnWorld> = {
  properties: {
    id: { type: 'int32' },
    randomNumber: { type: 'int32' },
  },
};

const worldSerialize = ajv.compileSerializer(worldSchema);

const worldArraySchema: JTDSchemaType<returnWorld[]> = {
  elements: worldSchema,
};

const worldArraySerialize = ajv.compileSerializer(worldArraySchema);

function randomNumber() {
  return Math.floor(Math.random() * 10000) + 1;
}

function messageCompare(a: message, b: message) {
  if (a.message < b.message) {
    return -1;
  } else if (a.message > b.message) {
    return 1;
  }
  return 0;
}

new Elysia()
  .get('/json', ({ }) => {
    return new Response(messageSerialize({ message: 'Hello, World!' }), {
      headers: {
        Server: 'bun',
        'Content-Type': 'application/json',
      },
    });
  })
  .get('/db', async ({ set }) => {
    const world = await db.selectFrom('world').where('id', '=', randomNumber()).select([
      'id',
      'randomnumber as randomNumber',
    ]).executeTakeFirstOrThrow();
    return new Response(worldSerialize(world), {
      headers: {
        Server: 'bun',
        'Content-Type': 'application/json',
      },
    });
  })
  .get(
    '/query',
    async ({ query }) => {
      const queries = Math.min(Math.max(parseInt(query.q, 10) || 1, 1), 500);
      const promisesArray = [];

      for (let i = 0; i < queries; i++) {
        promisesArray.push(randomNumber());
      }

      const worlds = await BPromise.map(
        promisesArray,
        id => db.selectFrom('world').where('id', '=', id).select([
          'id',
          'randomnumber as randomNumber',
        ]).executeTakeFirstOrThrow(),
        { concurrency: 20 },
      );

      return new Response(worldArraySerialize(worlds), {
        headers: {
          Server: 'bun',
          'Content-Type': 'application/json',
        },
      });
    },
    {
      query: t.Object({
        q: t.String(),
      }),
    },
  )
  .get('/fortunes', async () => {
    const fortunes = await db.selectFrom('fortune').selectAll().execute();
    fortunes.push({ id: 0, message: 'Additional fortune added at request time.' });
    fortunes.sort(messageCompare);
    return new Response(
      `<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
      ${fortunes.map(fortune => `<tr><td>${fortune.id}</td><td>${Bun.escapeHTML(fortune.message)}</td></tr>`).join('')}
      </table></body></html>`,
      {
        headers: {
          Server: 'bun',
          'Content-Type': 'text/html; charset=UTF-8',
        },
      },
    );
  })
  .get(
    '/updates',
    async ({ query }) => {

      const queries = Math.min(Math.max(parseInt(query.q, 10) || 1, 1), 500);
      const unqueIds = new Set();
      const ids = [];

      for (let i = 0; i < queries; i++) {
        let nextId = randomNumber();
        while (unqueIds.has(nextId)) {
          nextId = randomNumber();
        }
        ids.push(nextId);
      }

      const worlds = await BPromise.map(ids, id => db.selectFrom('world').where('id', '=', id).select([
        'id',
        'randomnumber as randomNumber',
      ]).executeTakeFirstOrThrow(), {
        concurrency: 20,
      });

      const signleWorlds: returnWorld[] = [];
      const updateWorlds: number[][] = [];
      const worldsLength = worlds.length;

      for (let i = 0; i < worldsLength; i++) {
        const world = worlds[i];
        world.randomNumber = randomNumber();
        signleWorlds.push(world);
        updateWorlds.push([world.id, world.randomNumber]);
      }

      let sucess = false;
      while (!sucess) {
        try {
          await sql`update world set randomnumber = (update_data.randomNumber):: int
            from(values ${sql.raw(updateWorlds.map(w => `(${w[0]},${w[1]})`).join(','))}) as update_data (id, randomNumber)
            where world.id = (update_data.id):: int`.execute(db);
          sucess = true;
        } catch (error) {
          if (!(error instanceof Error) || !error.message.includes('deadlock')) {
            throw error;
          }
        }
      }

      return new Response(worldArraySerialize(signleWorlds), {
        headers: {
          Server: 'bun',
          'Content-Type': 'application/json',
        },
      });
    },
    {
      query: t.Object({
        q: t.String(),
      }),
    },
  )
  .get('/plaintext', () => {
    return new Response('Hello, World!', {
      headers: {
        Server: 'bun',
        'Content-Type': 'text/plain',
      },
    });
  })
  .listen(8080);
