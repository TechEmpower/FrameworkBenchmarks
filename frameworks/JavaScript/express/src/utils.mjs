import fjs from 'fast-json-stringify';

export const GREETING = "Hello, World!";

export const maxQuery = 500
export const maxRows = 10000

export const parseQueries = (i) => i > maxQuery ? maxQuery : (i | 0) || 1;

export function generateRandomNumber() {
  return ((Math.random() * maxRows) | 0) + 1;
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
  return text.replace(unsafeHTMLMatcher, (m) => escapeHTMLRules[m] || m);
}

export const jsonSerializer = fjs({
  type: 'object',
  properties: {
    message: {
      type: 'string',
      format: 'unsafe',
    }
  }
});

export const worldObjectSerializer = fjs({
  type: 'object',
  properties: {
    id: { type: 'integer' },
    randomnumber: { type: 'integer' }
  }
});

export const worldsObjectSerializer = fjs({
  type: 'array',
  items: {
    type: 'object',
    properties: {
      id: { type: 'integer' },
      randomnumber: { type: 'integer' }
    }
  }
});

