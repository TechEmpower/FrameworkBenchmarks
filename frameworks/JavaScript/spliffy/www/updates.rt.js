const db = require( '../db' )
const { randomId, randomUniqueIds, parseCount } = require( "../fn" );
module.exports = {
    GET: async ( { url: { query: { queries } } } ) => await db.bulkUpdateWorld(
        await Promise.all(
            randomUniqueIds( parseCount( queries ) )
                .map( id =>
                    db.worldById( id )
                        .then( world => {
                            world.randomnumber = randomId()
                            return world
                        } )
                )
        )
    )
}
