const db = require( '../db' )

module.exports = {
    GET: () => db.worldById( db.randomId() )
}