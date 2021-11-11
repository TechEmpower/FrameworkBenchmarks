/**
 * Generates a random number between 1 and 10000
 *
 * @function
 * @returns {*} A number between 1 and 10000
 * Math.floor(Math.random() * 10000) + 1}
 */

export default function (): number {
  const max = 10000;
  return truncate((Math.random() * max) + 1);
}

function truncate(n: number): number {
  return n | 0;
}
