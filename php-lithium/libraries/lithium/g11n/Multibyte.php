<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */
namespace lithium\g11n;
use lithium\core\Libraries;

/**
 * The `Multibyte` class helps operating with UTF-8 encoded strings. Here
 * multibyte is synonymous to UTF-8 which is probably the most widespread
 * multibyte encoding in recent web application development.
 *
 * Over time - as the importance of multibyte encoding support grew - a variety
 * of extensions appeared. While each achieves its goal somewhat differently
 * and might be preferred over the other, they still all do that one thing.
 *
 * What can a framework provide, those extensions aren't? It can provide
 * abstractions that allow portable code. While this might not be a requirement
 * for application code, it's a definite must for the framework's core code.
 *
 * As previously mentioned extensions appeared in a semi-evolutionary way. This
 * leaves us with the situation where extensions are heterogeneously spread out
 * over environments. There certainly is no clear winner and we're left with
 * the situation of "supporting them all".
 *
 * Technically this class does very little in terms of abstraction. Its main
 * purpose is to allow adapting to changing environments: virtually creating
 * something you can rely on, something that's always there while it actually
 * is there only in one way or the other. And - yes - some convenience methods
 * are also on board.
 */
class Multibyte extends \lithium\core\Adaptable {

	/**
	 * Contains adapter configurations for `Multibyte` adapters.
	 *
	 * @var array
	 */
	protected static $_configurations = array();

	/**
	 * `Libraries::locate()`-compatible path to adapters for this class.
	 *
	 * @see lithium\core\Libraries::locate()
	 * @var string Dot-delimited path.
	 */
	protected static $_adapters = 'adapter.g11n.multibyte';

	/**
	 * Checks if a given string is UTF-8 encoded and is valid UTF-8.
	 *
	 * In _quick_ mode it will check only for non ASCII characters being used
	 * indicating any multibyte encoding. Don't use quick mode for integrity
	 * validation of UTF-8 encoded strings.
	 *
	 * @link http://www.w3.org/International/questions/qa-forms-utf-8.en
	 * @param string $string The string to analyze.
	 * @param array $options Allows to toggle mode via the `'quick'` option, defaults to `false`.
	 * @return boolean Returns `true` if the string is UTF-8.
	 */
	public static function is($string, array $options = array()) {
		$defaults = array('quick' => false);
		$options += $defaults;

		if ($options['quick']) {
			$regex = '/[^\x09\x0A\x0D\x20-\x7E]/m';
		} else {
			$regex  = '/\A(';
			$regex .= '[\x09\x0A\x0D\x20-\x7E]';            // ASCII
			$regex .= '|[\xC2-\xDF][\x80-\xBF]';            // non-overlong 2-byte
			$regex .= '|\xE0[\xA0-\xBF][\x80-\xBF]';        // excluding overlongs
			$regex .= '|[\xE1-\xEC\xEE\xEF][\x80-\xBF]{2}'; // straight 3-byte
			$regex .= '|\xED[\x80-\x9F][\x80-\xBF]';        // excluding surrogates
			$regex .= '|\xF0[\x90-\xBF][\x80-\xBF]{2}';     // planes 1-3
			$regex .= '|[\xF1-\xF3][\x80-\xBF]{3}';         // planes 4-15
			$regex .= '|\xF4[\x80-\x8F][\x80-\xBF]{2}';     // plane 16
			$regex .= ')*\z/m';
		}
		return (boolean) preg_match($regex, $string);
	}

	/**
	 * Gets the string length. Multibyte enabled version of `strlen()`.
	 *
	 * @link http://php.net/manual/en/function.strlen.php
	 * @param string $string The string being measured for length.
	 * @param array $options Allows for selecting the adapter to use via the
	 *               `name` options. Will use the `'default'` adapter by default.
	 * @return integer The length of the string on success.
	 */
	public static function strlen($string, array $options = array()) {
		$defaults = array('name' => 'default');
		$options += $defaults;
		return static::adapter($options['name'])->strlen($string);
	}

	/**
	 * Finds the position of the _first_ occurrence of a string within a string.
	 * Multibyte enabled version of `strpos()`.
	 *
	 * Not all adapters must support interpreting - thus applying - passed
	 * numeric values as ordinal values of a character.
	 *
	 * @link http://php.net/manual/en/function.strpos.php
	 * @param string $haystack The string being checked.
	 * @param string $needle The string to find in the haystack.
	 * @param integer $offset If specified, search will start this number of
	 *                characters counted from the beginning of the string. The
	 *                offset cannot be negative.
	 * @param array $options Allows for selecting the adapter to use via the
	 *               `name` options. Will use the `'default'` adapter by default.
	 * @return integer Returns the numeric position of the first occurrence of
	 *                 the needle in the haystack string. If needle is not found,
	 *                 it returns `false`.
	 */
	public static function strpos($haystack, $needle, $offset = 0, array $options = array()) {
		$defaults = array('name' => 'default');
		$options += $defaults;
		return static::adapter($options['name'])->strpos($haystack, $needle, $offset);
	}

	/**
	 * Finds the position of the _last_ occurrence of a string within a string.
	 * Multibyte enabled version of `strrpos()`.
	 *
	 * Not all adapters must support interpreting - thus applying - passed
	 * numeric values as ordinal values of a character. The `Iconv` adapter
	 * doesn't support an offset as `strpos()` does - this constitutes the
	 * lowest common denominator here.
	 *
	 * @link http://php.net/manual/en/function.strrpos.php
	 * @param string $haystack The string being checked.
	 * @param string $needle The string to find in the haystack.
	 * @param array $options Allows for selecting the adapter to use via the
	 *               `name` options. Will use the `'default'` adapter by default.
	 * @return integer Returns the numeric position of the last occurrence of
	 *                 the needle in the haystack string. If needle is not found,
	 *                 it returns `false`.
	 */
	public static function strrpos($haystack, $needle, array $options = array()) {
		$defaults = array('name' => 'default');
		$options += $defaults;
		return static::adapter($options['name'])->strrpos($haystack, $needle);
	}

	/**
	 * Returns the portion of string specified by the start and length parameters.
	 * Multibyte enabled version of `substr()`.
	 *
	 * @link http://php.net/manual/en/function.substr.php
	 * @param string $string The string to extract the substring from.
	 * @param integer $start Position of first character in string (offset).
	 * @param integer $length Maximum numbers of characters to use from string.
	 * @param array $options Allows for selecting the adapter to use via the
	 *               `name` options. Will use the `'default'` adapter by default.
	 * @return string The substring extracted from given string.
	 */
	public static function substr($string, $start, $length = null, array $options = array()) {
		$defaults = array('name' => 'default');
		$options += $defaults;
		return static::adapter($options['name'])->substr($string, $start, $length);
	}
}

?>