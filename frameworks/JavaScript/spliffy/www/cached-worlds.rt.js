const { parseCount, randomUniqueIds } = require("../fn");
const NodeCache = require( 'node-cache' )
const worldCache = new NodeCache()
const db = require('../db')
if(db.allWorlds){
    db.allWorlds().then( worlds => worlds.forEach( world => worldCache.set( world.id, world ) ) )
}
module.exports = {
    GET: async ({ url, res }) => {
        const count = parseCount(url.query.count);
        const ids = randomUniqueIds(count)
        const results = [];
        for (let i = 0; i < count; i++) {
            results.push(worldCache.get(ids[i]));
        }
        const worlds = await Promise.all(results);
        res.headers['server'] = 'spliffy';
        res.headers['content-type'] = 'application/json';
        return JSON.stringify(worlds);
    }
}
