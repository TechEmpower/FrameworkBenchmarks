import { sjs, attr } from 'slow-json-stringify'

/**
 * Add Benchmark HTTP response headers.
 *
 * Add HTTP response headers `Server` which is required by the test suite.
 * Header `Date` is automatically added by uWebsockets
 * https://github.com/uNetworking/uWebSockets/blob/master/src/HttpResponse.h#L78
 * 
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
  return Math.min(parseInt(request.getQuery("queries")) || 1, 500);
}

/**
 * Generate random number
 *
 */
export function generateRandomNumber() {
  return Math.ceil(Math.random() * 10000);
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

/**
 * Using Slow json stringify module to get faster results 
 */
export const jsonSerializer = sjs({ message: attr("string")}); 
export const worldObjectSerializer = sjs({ id: attr('number'), randomnumber: attr('number') });
// export const worldObjectsSerializer = sjs({ rows: attr("array", worldObjectSerializer) });

/**
 * Using Sort method which is performant for the test scenario
 * @returns 
 */
export function sortByMessage (arr) {
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