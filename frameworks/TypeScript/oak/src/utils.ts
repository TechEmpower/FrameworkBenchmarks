import { connect, Context, getQuery, html } from "./deps.ts";
import { Fortune } from "./models.ts";

export const randomNumber = () => {
  return Math.floor(Math.random() * 10000 + 1);
};

export const parseQuery = (ctx: Context) => {
  return Math.min(parseInt(getQuery(ctx).q) || 1, 500);
};

export const renderTemplate = (fortunes: Fortune[]) => {
  return `<!doctype html>
  <html>
    <head><title>Fortunes</title></head>
    <body>
      <table>
        <tr>
          <th>id</th>
          <th>message</th>
        </tr>
        ${
    fortunes.map(({ id, message }) =>
      html`<tr><td>${id}</td><td>${message}</td></tr>`
    ).join("")
  }
      </table>
    </body>
  </html>`;
};

export async function getDbClient() {
  return await connect({
    type: "postgres",
    port: 5432,
    database: "hello_world",
    hostname: "tfb-database",
    username: "benchmarkdbuser",
    password: "benchmarkdbpass",
  });
}
