const db = require('../db')
const { escape } = require('html-escaper')

const runTimeFortune = () => ({
    id: 0,
    message: 'Additional fortune added at request time.'
})

function sortByMessage(arr) {
    const n = arr.length
    for (let i = 1; i < n; i++) {
        const c = arr[i]
        let j = i - 1
        while ((j > -1) && (c.message < arr[j].message)) {
            arr[j + 1] = arr[j]
            j--
        }
        arr[j + 1] = c
    }
    return arr
}

module.exports = {
    GET: async ({ res }) => {
        let fortunes = await db.allFortunes()
        fortunes.push(runTimeFortune())
        sortByMessage(fortunes)

        let html = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>';
        for (let i = 0; i < fortunes.length; i++) {
            html += `<tr><td>${fortunes[i].id}</td><td>${escape(fortunes[i].message)}</td></tr>`;
        }
        html += '</table></body></html>';

        res.headers['server'] = 'spliffy';
        res.headers['content-type'] = 'text/html; charset=utf-8';
        return html;
    }
}
