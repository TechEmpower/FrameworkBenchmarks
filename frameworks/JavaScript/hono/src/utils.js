/**
 * Add Benchmark HTTP response headers.
 *
 * Add HTTP response headers `Server` and `Date` which is required by the test suite.
 *
 * https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview
 *
 * @param {import('hono').Context} c
 */
export function addBenchmarkHeaders(c) {
  c.header("Server", "Hono");
}

/**
 * Handle error for response
 *
 * @param {Error} err
 * @param {import('hono').Context} c
 */
export function handleError(err, c) {
  console.error(err);
  addBenchmarkHeaders(c);
  c.text("Internal Server Error");
}

/**
 * Get queries count
 *
 * @param {import('hono').Context} c
 */
export function getQueriesCount(c) {
  return Math.min(parseInt(c.req.query("queries")) || 1, 500);
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
const escapeHTMLRules = {
  "&": "&#38;",
  "<": "&#60;",
  ">": "&#62;",
  '"': "&#34;",
  "'": "&#39;",
  "/": "&#47;",
};

const unsafeHTMLMatcher = /[&<>"'\/]/g;

export function escape(text) {
  if (unsafeHTMLMatcher.test(text) === false) return text;
  return text.replace(unsafeHTMLMatcher, function (m) {
    return escapeHTMLRules[m] || m;
  });
}

/**
 * Using Sort method which is performant for the test scenario
 * @returns
 */
export function sortByMessage(arr) {
  const n = arr.length;
  for (let i = 1; i < n; i++) {
    const c = arr[i];
    let j = i - 1;
    while (j > -1 && c.message < arr[j].message) {
      arr[j + 1] = arr[j];
      j--;
    }
    arr[j + 1] = c;
  }
  return arr;
}
