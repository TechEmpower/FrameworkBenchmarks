<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\g11n\multibyte\adapter;

/**
 * The `Intl` class is an adapter which uses certain string functions from
 * `ext/intl`. You will need to have the extension installed to use this adapter.
 *
 * Internally works with a fixed encoding of UTF-8. This means you can't use
 * this adapter for anything different than UTF-8 encoded strings. Silently
 * returns `null` or `false` when input string contains badly formed UTF-8
 * sequences.
 *
 * @link http://php.net/manual/en/book.intl.php
 */
class Intl extends \lithium\core\Object {

	/**
	 * Determines if this adapter is enabled by checking if the `intl` extension is loaded.
	 *
	 * @return boolean Returns `true` if enabled, otherwise `false`.
	 */
	public static function enabled() {
		return extension_loaded('intl');
	}

	/**
	 * Here used as a multibyte enabled equivalent of `strlen()`.
	 *
	 * @link http://php.net/manual/en/function.grapheme-strlen.php
	 * @param string $string
	 * @return integer|void
	 */
	public function strlen($string) {
		return grapheme_strlen($string);
	}

	/**
	 * Here used as a multibyte enabled equivalent of `strpos()`.
	 *
	 * @link http://php.net/manual/en/function.grapheme-strpos.php
	 * @param string $haystack
	 * @param string $needle
	 * @param integer $offset
	 * @return integer|boolean
	 */
	public function strpos($haystack, $needle, $offset) {
		return grapheme_strpos($haystack, $needle, $offset);
	}

	/**
	 * Here used as a multibyte enabled equivalent of `strrpos()`.
	 *
	 * @link http://php.net/manual/en/function.grapheme-strpos.php
	 * @param string $haystack
	 * @param string $needle
	 * @return integer|boolean
	 */
	public function strrpos($haystack, $needle) {
		return grapheme_strrpos($haystack, $needle);
	}

	/**
	 * Here used as a multibyte enabled equivalent of `substr()`.
	 *
	 * @link http://php.net/manual/en/function.grapheme-substr.php
	 * @param string $string
	 * @param integer $start
	 * @param integer $length
	 * @return string|boolean
	 */
	public function substr($string, $start, $length) {
		return grapheme_substr($string, $start, $length);
	}
}

?>