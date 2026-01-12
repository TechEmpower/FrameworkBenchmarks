const db = require('../db')
const { parseCount, randomUniqueIds } = require("../fn");

module.exports = {
    GET: async ({ url, res }) => {
        const count = parseCount(url.query.queries);
        const ids = randomUniqueIds(count)
        const results = [];
        for (let i = 0; i < count; i++) {
            results.push(db.worldById(ids[i]));
        }
        const worlds = await Promise.all(results);
        res.headers['server'] = 'spliffy';
        res.headers['content-type'] = 'application/json';
        return JSON.stringify(worlds);
    }
}
