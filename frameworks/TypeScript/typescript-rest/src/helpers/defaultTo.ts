// tslint:disable:max-line-length

/**
 * Returns the second argument if it is not `null`, `NaN`,
 * or `undefined`; otherwise the first argument is returned.
 * Arguments must be the same type. This function is not
 * curried, so all arguments must be supplied at once.
 *
 * @function
 * @sig a -> b -> a | b
 * @param {a} def The default value.
 * @param {b} val The value returned instead of `def` unles `val` is `null`, `NaN`, or `undefined`
 * @return {a|b} The second value if it is not `null`, `NaN`, or `undefined`, otherwise the default value
 * @example
 *
 *     const port = defaultTo(3000, +process.env.PORT);
 */

export default function<T>(def: T, val: T): T {
  return val == null // handles `null` and `undefined`
    || val !== val   // handles NaN
    ? def
    : val;
}
