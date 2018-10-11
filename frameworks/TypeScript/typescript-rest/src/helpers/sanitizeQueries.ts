import defaultTo from "./defaultTo";

/**
 * Provides an upper limit on queries.
 *
 * @function
 * @sig a -> b
 * @param {a} queries A string representation of a number.
 * @returns {1..500|n} A number-casted version of the provided string between 1 and 500.
 */

export default function (queries: string): number {
  const int = defaultTo(1, parseInt(queries, undefined));
  const max = 500;
  const min = 1;

  if (int > max) { return max; }
  if (int < min) { return min; }

  return int;
}
