const db = require( '../db' )
const { randomId, randomUniqueIds, parseCount } = require( "../fn" );

randomNumber = process.env.DATABASE === 'mongodb' ? 'randomNumber' : 'randomnumber'

module.exports = {
    GET: async ( { url: { query: { queries } } } ) => await db.bulkUpdateWorld(
        await Promise.all(
            randomUniqueIds( parseCount( queries ) )
                .map( id =>
                    db.worldById( id )
                        .then( world => {
                            world[randomNumber] = randomId()
                            return world
                        } )
                )
        )
    )
}
