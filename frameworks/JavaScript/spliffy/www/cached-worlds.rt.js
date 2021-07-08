const NodeCache = require( 'node-cache' )
const worldCache = new NodeCache()
const db = require( '../db' )
const { parseIntBetween } = require( '../fn' )

//prime cache
db.getAllCachedWorlds().then( worlds => worlds.forEach( world => worldCache.set( world.id, world ) ) )

module.exports = {
    GET: async ( { url: { pathParameters: { count } } } ) => await Promise.all(
        db.randomUniqueIds( parseIntBetween( count, 1, 500 ) )
            .map( id =>
                new Promise( resolve => {
                    if( worldCache.has( id ) ) {
                        resolve( worldCache.get( id ) )
                    }
                    db.findCachedWorldById( id )
                        .then( world => {
                            worldCache.set( id, world )
                            resolve( world )
                        } )
                } )
            )
    )
}