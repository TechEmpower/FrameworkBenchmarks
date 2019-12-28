const {
    GET,
    validateParams,
    ValidationTypes
} = require("chubbajs").routes.annotations;
const World = require("../models/World");

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
        if (queries > 500) queries = 500;
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
        if (queries > 500) queries = 500;
        for (let i = 0; i < queries; i++) {
            const world = await new World(rando());
            world.randomnumber = rando();
            await world.save();
            // await ctx.db.query(`UPDATE world SET randomnumber = ${world.randomnumber} WHERE id = ${world.id}`);
            ret.push({ id: world.id, randomnumber: world.randomnumber });
        }
        ctx.res.json(ret);
    }

}

module.exports = HelloWorldController;
