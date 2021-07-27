const db = require( '../db' )
const { escape } = require( 'html-escaper' )

const runTimeFortune = {
    id: 0,
    message: 'Additional fortune added at request time.'
}

const renderBody = fortunes => {
    let body = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
    for( let fortune of fortunes ) {
        body += `<tr><td>${fortune.id}</td><td>${escape( fortune.message )}</td></tr>`
    }
    body += '</table></body></html>'
    return body
}

module.exports = {
    GET: async () => {
        let fortunes = await db.allFortunes()
        fortunes.push( runTimeFortune )
        fortunes.sort( ( a, b ) => a.message.localeCompare( b.message ) )
        return {
            headers: {
                'Content-Type': 'text/html; charset=utf-8'
            },
            body: renderBody( fortunes )
        }
    }
}
