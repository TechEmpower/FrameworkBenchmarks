<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\security;

use lithium\util\String;

/**
 * `Password` utility class that makes use of PHP's `crypt()` function. Includes a
 * cryptographically strong salt generator, and utility functions to hash and check
 * passwords.
 */
class Password {

	/**
	 * The default log2 number of iterations for Blowfish encryption.
	 */
	const BF = 10;

	/**
	 * The default log2 number of iterations for XDES encryption.
	 */
	const XDES = 18;

	/**
	 * Hashes a password using PHP's `crypt()` and an optional salt. If no
	 * salt is supplied, a cryptographically strong salt will be generated
	 * using `lithium\security\Password::salt()`.
	 *
	 * Using this function is the proper way to hash a password. Using naïve
	 * methods such as sha1 or md5, as is done in many web applications, is
	 * improper due to the lack of a cryptographically strong salt.
	 *
	 * Using `lithium\security\Password::hash()` ensures that:
	 *
	 * - Two identical passwords will never use the same salt, thus never
	 *   resulting in the same hash; this prevents a potential attacker from
	 *   compromising user accounts by using a database of most commonly used
	 *   passwords.
	 * - The salt generator's count iterator can be increased within Lithium
	 *   or your application as computer hardware becomes faster; this results
	 *   in slower hash generation, without invalidating existing passwords.
	 *
	 * Usage:
	 *
	 * {{{
	 * // Hash a password before storing it:
	 * $hashed  = Password::hash($password);
	 *
	 * // Check a password by comparing it to its hashed value:
	 * $check   = Password::check($password, $hashed);
	 *
	 * // Use a stronger custom salt:
	 * $salt    = Password::salt('bf', 16); // 2^16 iterations
	 * $hashed  = Password::hash($password, $salt); // Very slow
	 * $check   = Password::check($password, $hashed); // Very slow
	 *
	 * // Forward/backward compatibility
	 * $salt1   = Password::salt('bf', 6);
	 * $salt2   = Password::salt('bf', 12);
	 * $hashed1 = Password::hash($password, $salt1); // Fast
	 * $hashed2 = Password::hash($password, $salt2); // Slow
	 * $check1  = Password::check($password, $hashed1); // True
	 * $check2  = Password::check($password, $hashed2); // True
	 * }}}
	 *
	 * @see lithium\security\Password::check()
	 * @see lithium\security\Password::salt()
	 * @link http://php.net/manual/function.crypt.php
	 * @param string $password The password to hash.
	 * @param string $salt Optional. The salt string.
	 * @return string The hashed password.
	 *        The result's length will be:
	 *        - 60 chars long for Blowfish hashes
	 *        - 20 chars long for XDES hashes
	 *        - 34 chars long for MD5 hashes
	 */
	public static function hash($password, $salt = null) {
		return crypt($password, $salt ?: static::salt());
	}

	/**
	 * Compares a password and its hashed value using PHP's `crypt()`. Rather than a simple string
	 * comparison, this method uses a constant-time algorithm to defend against timing attacks.
	 *
	 * @see lithium\security\Password::hash()
	 * @see lithium\security\Password::salt()
	 * @param string $password The password to check.
	 * @param string $hash The hashed password to compare it to.
	 * @return boolean Returns a boolean indicating whether the password is correct.
	 */
	public static function check($password, $hash) {
		return String::compare(crypt($password, $hash), $hash);
	}

	/**
	 * Generates a cryptographically strong salt, using the best available
	 * method (tries Blowfish, then XDES, and fallbacks to MD5), for use in
	 * `Password::hash()`.
	 *
	 * Blowfish and XDES are adaptive hashing algorithms. MD5 is not. Adaptive
	 * hashing algorithms are designed in such a way that when computers get
	 * faster, you can tune the algorithm to be slower by increasing the number
	 * of hash iterations, without introducing incompatibility with existing
	 * passwords.
	 *
	 * To pick an appropriate iteration count for adaptive algorithms, consider
	 * that the original DES crypt was designed to have the speed of 4 hashes
	 * per second on the hardware of that time. Slower than 4 hashes per second
	 * would probably dampen usability. Faster than 100 hashes per second is
	 * probably too fast. The defaults generate about 10 hashes per second
	 * using a dual-core 2.2GHz CPU.
	 *
	 *  _Note 1_: this salt generator is different from naive salt implementations
	 * (e.g. `md5(microtime())`) in that it uses all of the available bits of
	 * entropy for the supplied salt method.
	 *
	 *  _Note2_: this method should not be use to generate custom salts. Indeed,
	 * the resulting salts are prefixed with information expected by PHP's
	 * `crypt()`. To get an arbitrarily long, cryptographically strong salt
	 * consisting in random sequences of alpha numeric characters, use
	 * `lithium\util\String::random()` instead.
	 *
	 * @link http://php.net/manual/en/function.crypt.php
	 * @link http://www.postgresql.org/docs/9.0/static/pgcrypto.html
	 * @see lithium\security\Password::hash()
	 * @see lithium\security\Password::check()
	 * @see lithium\util\String::random()
	 * @param string $type The hash type. Optional. Defaults to the best
	 *        available option. Supported values, along with their maximum
	 *        password lengths, include:
	 *        - `'bf'`: Blowfish (128 salt bits, max 72 chars)
	 *        - `'xdes'`: XDES (24 salt bits, max 8 chars)
	 *        - `'md5'`: MD5 (48 salt bits, unlimited length)
	 * @param integer $count Optional. The base-2 logarithm of the iteration
	 *        count, for adaptive algorithms. Defaults to:
	 *        - `10` for Blowfish
	 *        - `18` for XDES
	 * @return string The salt string.
	 */
	public static function salt($type = null, $count = null) {
		switch (true) {
			case CRYPT_BLOWFISH == 1 && (!$type || $type === 'bf'):
				return static::_genSaltBf($count);
			case CRYPT_EXT_DES == 1 && (!$type || $type === 'xdes'):
				return static::_genSaltXDES($count);
			default:
				return static::_genSaltMD5();
		}
	}

	/**
	 * Generates a Blowfish salt for use in `lithium\security\Password::hash()`. _Note_: Does not
	 * use the `'encode'` option of `String::random()` because it could result in 2 bits less of
	 * entropy depending on the last character.
	 *
	 * @param integer $count The base-2 logarithm of the iteration count.
	 *        Defaults to `10`. Can be `4` to `31`.
	 * @return string The Blowfish salt.
	 */
	protected static function _genSaltBf($count = null) {
		$count = (integer) $count;
		$count = ($count < 4 || $count > 31) ? static::BF : $count;

		$base64 = './ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
		$i = 0;

		$input = String::random(16);
		$output = '';

		do {
			$c1 = ord($input[$i++]);
			$output .= $base64[$c1 >> 2];
			$c1 = ($c1 & 0x03) << 4;
			if ($i >= 16) {
				$output .= $base64[$c1];
				break;
			}

			$c2 = ord($input[$i++]);
			$c1 |= $c2 >> 4;
			$output .= $base64[$c1];
			$c1 = ($c2 & 0x0f) << 2;

			$c2 = ord($input[$i++]);
			$c1 |= $c2 >> 6;
			$output .= $base64[$c1];
			$output .= $base64[$c2 & 0x3f];
		} while (1);

		$result = '$2a$';
		$result .= chr(ord('0') + $count / static::BF);
		$result .= chr(ord('0') + $count % static::BF);
		$result .= '$' . $output;

		return $result;
	}

	/**
	 * Generates an Extended DES salt for use in `lithium\security\Password::hash()`.
	 *
	 * @param integer $count The base-2 logarithm of the iteration count. Defaults to `18`. Can be
	 *                `1` to `24`. 1 will be stripped from the non-log value, e.g. 2^18 - 1, to
	 *                ensure we don't use a weak DES key.
	 * @return string The XDES salt.
	 */
	protected static function _genSaltXDES($count = null) {
		$count = (integer) $count;
		$count = ($count < 1 || $count > 24) ? static::XDES : $count;

		$count = (1 << $count) - 1;
		$base64 = './0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

		$output = '_' . $base64[$count & 0x3f] . $base64[($count >> 6) & 0x3f];
		$output .= $base64[($count >> 12) & 0x3f] . $base64[($count >> 18) & 0x3f];
		$output .= String::random(3, array('encode' => String::ENCODE_BASE_64));

		return $output;
	}

	/**
	 * Generates an MD5 salt for use in `lithium\security\Password::hash()`.
	 *
	 * @return string The MD5 salt.
	 */
	protected static function _genSaltMD5() {
		return '$1$' . String::random(6, array('encode' => String::ENCODE_BASE_64));
	}
}

?>