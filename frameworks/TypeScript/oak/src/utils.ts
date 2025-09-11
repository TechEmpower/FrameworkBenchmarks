import { html } from "html";
import { Context } from "oak";
import type { Fortune } from "./types.ts";

export const randomNumber = () => {
  return Math.floor(Math.random() * 10000 + 1);
};

export const parseQuery = (ctx: Context) => {
  const queryParam = ctx.request.url.searchParams.get('q') ?? '1';
  const parseValue = parseInt(queryParam, 10);
  if (isNaN(parseValue) || parseValue < 1) {
    return 1;
  }
  return Math.min(parseValue, 500);
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
