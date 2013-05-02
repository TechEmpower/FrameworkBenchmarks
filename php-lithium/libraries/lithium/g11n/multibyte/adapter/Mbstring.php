<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\g11n\multibyte\adapter;

/**
 * The `Mbstring` class is an adapter which uses certain string functions from
 * `ext/mbstring`. You will need to have the extension installed to use this adapter.
 *
 * No known limitations affecting used functionality. Silently strips
 * out badly formed UTF-8 sequences.
 *
 * @link http://php.net/manual/en/book.mbstring.php
 */
class Mbstring extends \lithium\core\Object {

	/**
	 * Determines if this adapter is enabled by checking if the `mbstring` extension is loaded.
	 *
	 * @return boolean Returns `true` if enabled, otherwise `false`.
	 */
	public static function enabled() {
		return extension_loaded('mbstring');
	}

	/**
	 * Here used as a multibyte enabled equivalent of `strlen()`.
	 *
	 * @link http://php.net/manual/en/function.mb-strlen.php
	 * @param string $string
	 * @return integer
	 */
	public function strlen($string) {
		return mb_strlen($string, 'UTF-8');
	}

	/**
	 * Here used as a multibyte enabled equivalent of `strpos()`.
	 *
	 * @link http://php.net/manual/en/function.mb-strpos.php
	 * @param string $haystack
	 * @param string $needle
	 * @param integer $offset
	 * @return integer|boolean
	 */
	public function strpos($haystack, $needle, $offset) {
		return mb_strpos($haystack, $needle, $offset, 'UTF-8');
	}

	/**
	 * Here used as a multibyte enabled equivalent of `strrpos()`.
	 *
	 * @link http://php.net/manual/en/function.mb-strpos.php
	 * @param string $haystack
	 * @param string $needle
	 * @return integer|boolean
	 */
	public function strrpos($haystack, $needle) {
		return mb_strrpos($haystack, $needle, 0, 'UTF-8');
	}

	/**
	 * Here used as a multibyte enabled equivalent of `substr()`.
	 *
	 * @link http://php.net/manual/en/function.mb-substr.php
	 * @param string $string
	 * @param integer $start
	 * @param integer $length
	 * @return string|boolean
	 */
	public function substr($string, $start, $length) {
		return mb_substr($string, $start, $length, 'UTF-8');
	}
}

?>