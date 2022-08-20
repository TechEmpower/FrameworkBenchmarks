/**
 * Add Benchmark HTTP response headers.
 *
 * Add HTTP response headers `Server` and `Date` which are required by the test suite.
 * https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview
 *
 * @param {import('uWebSockets.js').HttpResponse} response
 */
 export function addBenchmarkHeaders(response) {
    response.writeHeader('Server', 'uWebSockets.js');
    response.writeHeader('Date', new Date().toUTCString());
}

/**
 * Handle error for response
 *
 * @param {Error} error
 * @param {import('uWebSockets.js').HttpResponse} response
 */
export function handleError(error, response) {
    console.error(error);
    response.cork(() => {
        addBenchmarkHeaders(response);
        response.writeHeader('Content-Type', 'text/plain');
        response.end('Internal Server Error');
    });
}
