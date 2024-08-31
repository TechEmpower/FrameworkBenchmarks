import { Fortune } from './types.js';

export function getRandomNumber() {
  return Math.floor(Math.random() * 10000) + 1;
}

export function getNumberOfObjects(queries: string) {
  return Math.min(Math.max(parseInt(queries) || 1, 1), 500);
}

export const additionalFortune = {
  id: 0,
  message: 'Additional fortune added at request time.',
};

export function compare(a: Fortune, b: Fortune) {
  if (a.message < b.message) {
    return -1;
  } else if (a.message > b.message) {
    return 1;
  }
  return 0;
}
