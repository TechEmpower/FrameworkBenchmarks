let dbImpl

switch( process.env.DATABASE ) {
    case 'mysql':
        dbImpl = require( './db/mysql' )
        break
    case 'mongodb':
        dbImpl = require( './db/mongodb' )
        break
    case 'postgres':
        dbImpl = require( './db/postgres' )
        break
    default:
        dbImpl = {
            init: async () => {
            }
        }
}

module.exports = dbImpl