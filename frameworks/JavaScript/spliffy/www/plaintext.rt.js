module.exports = {
    GET: ( { res } ) => {
        res.headers['content-type'] = 'text/plain'
        return 'Hello, World!'
    }
}
