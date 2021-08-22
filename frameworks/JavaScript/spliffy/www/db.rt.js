const db = require( '../db' )
const { randomId } = require( "../fn" );

module.exports = {
    GET: () => db.worldById( randomId() )
}
