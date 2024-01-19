import { sjs, attr } from "slow-json-stringify";

export const GREETING = "Hello, World!";

export const headerTypes = {
  plain: "text/plain",
  json: "application/json",
  html: "text/html; charset=UTF-8",
};

export function writeResponse(res, text, type = headerTypes["json"]) {
  res.writeHead(200, {
    "content-type": type,
    server: "Express",
  });
  res.end(text);
}

export function handleError(error, response) {
  console.error(error);
  response.end("Internal Server Error");
}

export function getQueriesCount(request) {
  return Math.min(parseInt(request.query["queries"]) || 1, 500);
}

export function generateRandomNumber() {
  return Math.ceil(Math.random() * 10000);
}

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

export const jsonSerializer = sjs({ message: attr("string") });
export const worldObjectSerializer = sjs({
  id: attr("number"),
  randomnumber: attr("number"),
});

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
