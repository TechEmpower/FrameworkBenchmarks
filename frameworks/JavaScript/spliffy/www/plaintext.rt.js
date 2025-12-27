module.exports = {
    GET: ({ res }) => {
        res.headers['server'] = 'spliffy';
        res.headers['content-type'] = 'text/plain';
        return 'Hello, World!';
    }
}
