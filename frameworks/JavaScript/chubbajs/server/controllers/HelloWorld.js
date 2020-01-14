const {
    GET,
    validateParams
} = require("chubbajs").routes.annotations;
const Entities = require("html-entities").AllHtmlEntities;
const World = require("../models/World");
const Fortune = require("../models/Fortune");

const entities = new Entities();

function rando() {
    return Math.floor(Math.random() * 10000) + 1;
}

function sanitizeQueries(q) {
    if (!parseInt(q) || q < 1) return 1;
    if (q > 500) return 500;
    return q;
}

class HelloWorldController {
    @GET("*")
    async setServer(ctx) {
        ctx.res.set('Server', 'Express');
    }

    @GET("/plaintext")
    async plaintext(ctx) {
        ctx.res.set('Content-Type', 'text/plain');
        ctx.res.send("Hello, World!");
    }

    @GET("/json")
    async json(ctx) {
        ctx.res.json({ message: "Hello, World!"});
    }

    @GET("/db")
    async db(ctx) {
        const world = await new World(rando());
        ctx.res.json({ id: world.id, randomnumber: world.randomnumber });
    }

    @GET("/query")
    @validateParams({
        query: "queries",
        sanitize: sanitizeQueries
    })
    async query(ctx, { queries }) {
        const ret = [];
        for (let i = 0; i < queries; i++) {
            const world = await new World(rando());
            ret.push({ id: world.id, randomnumber: world.randomnumber });
        }
        ctx.res.json(ret);
    }

    @GET("/update")
    @validateParams({
        query: "queries",
        sanitize: sanitizeQueries
    })
    async update(ctx, { queries }) {
        const ret = [];
        for (let i = 0; i < queries; i++) {
            const world = await new World(rando());
            world.randomnumber = rando();
            await world.save();
            ret.push({ id: world.id, randomnumber: world.randomnumber });
        }
        ctx.res.json(ret);
    }

    @GET("/fortune")
    async fortune(ctx) {
        let fortunes = (await ctx.db.query(`SELECT * FROM "Fortune"`)).rows;
        const newFortune = new Fortune();
        newFortune.message = "Additional fortune added at request time.";
        newFortune.id = 0;
        fortunes.push(newFortune);
        fortunes.sort((a, b) => (a.message < b.message) ? -1 : 1);

        ctx.res.send(`<!DOCTYPE html>
<html>
<head>
<title>Fortunes</title>
</head>
<body>
<table>
<tr>
<th>id</th>
<th>message</th>
</tr>
${fortunes.map(f => `
<tr>
<td>${f.id}</td>
<td>${entities.encode(f.message)}</td>
</tr>
`).join('')}
</table>
</body>
</html>`);
    }

}

module.exports = HelloWorldController;
