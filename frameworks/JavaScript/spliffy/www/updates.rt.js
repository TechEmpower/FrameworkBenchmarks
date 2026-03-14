const db = require('../db')
const { parseCount, randomId } = require("../fn");

module.exports = {
    GET: async ({ url, res }) => {
        const count = parseCount(url.query.queries);
        const results = [];
        for (let i = 0; i < count; i++) {
            results.push(db.worldById(randomId()));
        }
        const worlds = await Promise.all(results);
        for (let i = 0; i < count; i++) {
            worlds[i].randomNumber = randomId();
        }
        res.headers['server'] = 'spliffy';
        res.headers['content-type'] = 'application/json';

        if (worlds.length === 0) {
            return JSON.stringify([]);
        }

        await db.bulkUpdateWorld(worlds);
        return JSON.stringify(worlds);
    }
}
