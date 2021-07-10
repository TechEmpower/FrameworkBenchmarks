const NodeCache = require( 'node-cache' )
const worldCache = new NodeCache()
const db = require( '../db' )
const { parseCount } = require( '../fn' )

db.allWorlds().then( worlds => worlds.forEach( world => worldCache.set( world.id, world ) ) )

module.exports = {
    GET: ( { url: { query: { count } } } ) =>
        db.randomUniqueIds( parseCount( count ) )
            .map( id => worldCache.get( id ) )
}