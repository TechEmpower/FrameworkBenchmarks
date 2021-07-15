const db = require( '../db' )
const { parseCount } = require( '../fn' )
module.exports = {
    GET: async ( { url: { query: { queries } } } ) => await db.bulkUpdateWorld(
        await Promise.all(
            db.randomUniqueIds( parseCount( queries ) )
                .map( id =>
                    db.worldById( id )
                        .then( world => {
                            world.randomnumber = db.randomId()
                            return world
                        } )
                )
        )
    )
}
