import { Elysia, t } from "elysia";
import {
	find,
	fortunes as getFortunes,
	findThenRand,
	bulkUpdate,
} from "./postgres";

export function rand() {
	return ~~(Math.random() * 10000);
}

function parseQueriesNumber(q?: string) {
	// NaN is falsy, fallback to one.
	return Math.min(+q! || 1, 500);
}

export const dbHandlers = new Elysia()
	.headers({
		server: "Elysia",
	})
	// ? Mark as async for Promise result to prevent double Elysia's mapResponse execution
	.get("/db", async () => find(rand()))
	.get("/fortunes", async (c) => {
		const fortunes = await getFortunes();

		fortunes.push({
			id: 0,
			message: "Additional fortune added at request time.",
		});

		fortunes.sort((a, b) => {
			if (a.message < b.message) return -1;

			return 1;
		});

		c.set.headers["content-type"] = "text/html; charset=utf-8";

		const n = fortunes.length;

		let html = "";
		for (let i = 0; i < n; i++) {
			html += `<tr><td>${fortunes[i].id}</td><td>${Bun.escapeHTML(
				fortunes[i].message,
			)}</td></tr>`;
		}

		return `<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`;
	})
	// ? Mark as async for Promise result to prevent double Elysia's mapResponse execution
	.get("/queries", async (c) => {
		const num = parseQueriesNumber(c.query.queries);

		const worldPromises = new Array(num);
		for (let i = 0; i < num; i++) worldPromises[i] = find(rand());

		return await Promise.all(worldPromises);
	})
	.get("/updates", async (c) => {
		const num = parseQueriesNumber(c.query.queries);
		const worldPromises = new Array(num);

		for (let i = 0; i < num; i++) worldPromises[i] = findThenRand(rand());

		const worlds = await Promise.all(worldPromises);

		await bulkUpdate(worlds);

		return worlds;
	});
