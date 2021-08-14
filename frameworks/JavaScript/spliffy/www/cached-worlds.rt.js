const NodeCache = require( 'node-cache' )
const worldCache = new NodeCache()
const db = require( '../db' )
const { parseCount, randomUniqueIds } = require( '../fn' )
if(db.allWorlds){
    db.allWorlds().then( worlds => worlds.forEach( world => worldCache.set( world.id, world ) ) )
}

module.exports = {
    GET: ( { url: { query: { count } } } ) =>
        randomUniqueIds( parseCount( count ) )
            .map(worldCache.get)
}
