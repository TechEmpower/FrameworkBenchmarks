const db = require( '../db' )
const { parseIntBetween } = require( '../fn' )
module.exports = {
    GET: async ( { url: { pathParameters: { queries } } } ) => await db.bulkUpdateWorld(
        await Promise.all(
            db.randomUniqueIds( parseIntBetween( queries, 1, 500 ) )
                .map( id =>
                    db.findWorldById( id )
                        .then( world => world.randomNumber = db.randomId() )
                )
        )
    )
}