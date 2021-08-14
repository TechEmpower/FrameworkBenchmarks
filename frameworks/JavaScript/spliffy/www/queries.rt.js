const db = require( '../db' )
const { parseCount, randomUniqueIds } = require( '../fn' )
module.exports = {
    GET: async ( { url: { query: { queries } } } ) => await Promise.all(
        randomUniqueIds( parseCount( queries ) )
            .map( id => db.worldById( id ) )
    )
}
