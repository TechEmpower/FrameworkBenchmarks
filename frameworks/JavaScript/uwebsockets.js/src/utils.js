/**
 * Add Benchmark HTTP response headers.
 *
 * Add HTTP response headers `Server` and `Date` which are required by the test suite.
 * https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview
 *
 * @param {import('uWebSockets.js').HttpResponse} response
 */
export function addBenchmarkHeaders(response) {
  response.writeHeader("Server", "uWebSockets.js");
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
    response.writeHeader("Content-Type", "text/plain");
    response.end("Internal Server Error");
  });
}

/**
 * Get queries count
 *
 * @param {import('uWebSockets.js').HttpRequest} request
 */
export function getQueriesCount(request) {
  if (request.getQuery("queries")) {
    const queries = parseInt(request.getQuery("queries"), 10) || 1;
    return queries >= 1 && queries <= 500 ? queries : 500;
  }

  return 1;
}

/**
 * Generate random number
 *
 */
export function generateRandomNumber() {
  return Math.floor(Math.random() * 9999) + 1;
}

/**
 * Escape unsafe HTML Code
 *
 */
const escapeHTMLRules = { '&': '&#38;', '<': '&#60;', '>': '&#62;', '"': '&#34;', "'": '&#39;', '/': '&#47;' }

const unsafeHTMLMatcher = /[&<>"'\/]/g

export function escape(text) {
  if (unsafeHTMLMatcher.test(text) === false) return text;
  return text.replace(unsafeHTMLMatcher, function (m) { return escapeHTMLRules[m] || m; });
}
