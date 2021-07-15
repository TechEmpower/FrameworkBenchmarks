const db = require( '../db' )
const { escape } = require( 'html-escaper' )
const renderFortunes = fortunes => {
    let rendered = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
    for( let i = 0; i < fortunes.length; i++ ) {
        rendered += `<tr><td>${fortunes[i].id}</td><td>${escape( fortunes[i].message )}</td></tr>`
    }
    rendered += '</table></body></html>'
    return rendered
}

module.exports = {
    GET: async () => ( {
        headers: {
            'Content-Type': 'text/html; charset=utf-8'
        },
        body: renderFortunes(
            [
                ...await db.allFortunes(),
                {
                    id: 0,
                    message: 'Additional fortune added at request time.'
                }
            ].sort( ( a, b ) => a.message.localeCompare( b.message ) )
        )
    } )
}
