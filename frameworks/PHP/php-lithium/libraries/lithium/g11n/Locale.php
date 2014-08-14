<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\g11n;

use BadMethodCallException;
use InvalidArgumentException;
use lithium\action\Request as ActionRequest;
use lithium\console\Request as ConsoleRequest;

/**
 * The `Locale` class provides methods to deal with locale identifiers.  The locale
 * (here: _locale identifier_) is used to distinguish among different sets of common
 * preferences.
 *
 * In order to avoid unnecessary overhead all methods throughout the framework accepting
 * a locale require it to be well-formed according to the structure laid out below. For
 * assuring the correct format use `Locale::canonicalize()` once on the locale.
 *
 * However the methods within this class will also work with not-so-well-formed locales.
 * They accept both underscores and hyphens as separators between and don't care about the
 * case of the individual tags.
 *
 * The identifier used by Lithium is based in its structure upon Unicode's
 * language identifier and is compliant to BCP 47.
 *
 * `language[_Script][_TERRITORY][_VARIANT]`
 *  - `language` The spoken language, here represented by an ISO 639-1 code,
 *    where not available ISO 639-3 and ISO 639-5 codes are allowed too) tag.
 *    The tag should  be lower-cased and is required.
 *  - `Script` The tag should have it's first character capitalized, all others
 *    lower-cased. The tag is optional.
 *  - `TERRITORY` A geographical area, here represented by an ISO 3166-1 code.
 *     Should be all upper-cased and is optional.
 *  - `VARIANT` Should be all upper-cased and is optional.
 *
 * @link http://www.unicode.org/reports/tr35/tr35-12.html#Identifiers
 * @link http://www.rfc-editor.org/rfc/bcp/bcp47.txt
 * @link http://www.iana.org/assignments/language-subtag-registry
 */
class Locale extends \lithium\core\StaticObject {

	/**
	 * Properties for locale tags.
	 *
	 * @var array
	 */
	protected static $_tags = array(
		'language' => array('formatter' => 'strtolower'),
		'script' => array('formatter' => array('strtolower', 'ucfirst')),
		'territory' => array('formatter' => 'strtoupper'),
		'variant' => array('formatter' => 'strtoupper')
	);

	/**
	 * Magic method enabling `language`, `script`, `territory` and `variant`
	 * methods to parse and retrieve individual tags from a locale.
	 *
	 * {{{
	 *     Locale::language('en_US'); // returns 'en'
	 *     Locale::territory('en_US'); // returns 'US'
	 * }}}
	 *
	 * @see lithium\g11n\Locale::$_tags
	 * @see lithium\g11n\Locale::decompose()
	 * @param string $method
	 * @param array $params
	 * @return mixed
	 */
	public static function __callStatic($method, $params = array()) {
		$tags = static::invokeMethod('decompose', $params);

		if (!isset(static::$_tags[$method])) {
			throw new BadMethodCallException("Invalid locale tag `{$method}`.");
		}
		return isset($tags[$method]) ? $tags[$method] : null;
	}

	/**
	 * Custom check to determine if our given magic methods can be responded to.
	 *
	 * @param  string  $method     Method name.
	 * @param  bool    $internal   Interal call or not.
	 * @return bool
	 */
	public static function respondsTo($method, $internal = false) {
		return isset(static::$_tags[$method]) || parent::respondsTo($method, $internal);
	}

	/**
	 * Composes a locale from locale tags.  This is the pendant to `Locale::decompose()`.
	 *
	 * @param array $tags An array as obtained from `Locale::decompose()`.
	 * @return string A locale with tags separated by underscores or `null`
	 *         if none of the passed tags could be used to compose a locale.
	 */
	public static function compose($tags) {
		$result = array();

		foreach (static::$_tags as $name => $tag) {
			if (isset($tags[$name])) {
				$result[] = $tags[$name];
			}
		}
		if ($result) {
			return implode('_', $result);
		}
	}

	/**
	 * Parses a locale into locale tags.  This is the pendant to `Locale::compose()``.
	 *
	 * @param string $locale A locale in an arbitrary form (i.e. `'en_US'` or `'EN-US'`).
	 * @return array Parsed language, script, territory and variant tags.
	 * @throws InvalidArgumentException
	 */
	public static function decompose($locale) {
		$regex  = '(?P<language>[a-z]{2,3})';
		$regex .= '(?:[_-](?P<script>[a-z]{4}))?';
		$regex .= '(?:[_-](?P<territory>[a-z]{2}))?';
		$regex .= '(?:[_-](?P<variant>[a-z]{5,}))?';

		if (!preg_match("/^{$regex}$/i", $locale, $matches)) {
			throw new InvalidArgumentException("Locale `{$locale}` could not be parsed.");
		}
		return array_filter(array_intersect_key($matches, static::$_tags));
	}

	/**
	 * Returns a locale in its canonical form with tags formatted properly.
	 *
	 * @param string $locale A locale in an arbitrary form (i.e. `'ZH-HANS-HK_REVISED'`).
	 * @return string A locale in it's canonical form (i.e. `'zh_Hans_HK_REVISED'`).
	 */
	public static function canonicalize($locale) {
		$tags = static::decompose($locale);

		foreach ($tags as $name => &$tag) {
			foreach ((array) static::$_tags[$name]['formatter'] as $formatter) {
				$tag = $formatter($tag);
			}
		}
		return static::compose($tags);
	}

	/**
	 * Cascades a locale.
	 *
	 * Usage:
	 * {{{
	 * Locale::cascade('en_US');
	 * // returns array('en_US', 'en', 'root')
	 *
	 * Locale::cascade('zh_Hans_HK_REVISED');
	 * // returns array('zh_Hans_HK_REVISED', 'zh_Hans_HK', 'zh_Hans', 'zh', 'root')
	 * }}}
	 *
	 * @link http://www.unicode.org/reports/tr35/tr35-13.html#Locale_Inheritance
	 * @param string $locale A locale in an arbitrary form (i.e. `'en_US'` or `'EN-US'`).
	 * @return array Indexed array of locales (starting with the most specific one).
	 */
	public static function cascade($locale) {
		$locales[] = $locale;

		if ($locale === 'root') {
			return $locales;
		}
		$tags = static::decompose($locale);

		while (count($tags) > 1) {
			array_pop($tags);
			$locales[] = static::compose($tags);
		}
		$locales[] = 'root';
		return $locales;
	}

	/**
	 * Searches an array of locales for the best match to a locale. The locale
	 * is iteratively simplified until either it matches one of the locales
	 * in the list or the locale can't be further simplified.
	 *
	 * This method partially implements the lookup matching scheme as described
	 * in RFC 4647, section 3.4 and thus does not strictly conform to the
	 * specification.
	 *
	 * Differences to specification:
	 * - No support for wildcards in the to-be-matched locales.
	 * - No support for locales with private subtags.
	 * - No support for a default return value.
	 * - Passed locales are required to be in canonical form (i.e. `'ja_JP'`).
	 *
	 * @link http://www.ietf.org/rfc/rfc4647.txt
	 * @param array $locales Locales to match against `$locale`.
	 * @param string $locale A locale in it's canonical form (i.e. `'zh_Hans_HK_REVISED'`).
	 * @return string The matched locale.
	 */
	public static function lookup($locales, $locale) {
		$tags = static::decompose($locale);
		$count = count($tags);
		while ($count > 0) {
			if (($key = array_search(static::compose($tags), $locales)) !== false) {
				return $locales[$key];
			} elseif ($count === 1) {
				foreach ($locales as $currentLocale) {
					if (strpos($currentLocale, current($tags) . '_') === 0) {
						return $currentLocale;
					}
				}
			}
			if (($key = array_search(static::compose($tags), $locales)) !== false) {
				return $locales[$key];
			}
			array_pop($tags);
			$count = count($tags);
		}
	}

	/**
	 * Determines the preferred locale from a request or array. Optionally negotiates
	 * the preferred locale with available locales.
	 *
	 * @see lithium\g11n\Locale::_preferredAction()
	 * @see lithium\g11n\Locale::_preferredConsole()
	 * @see lithium\g11n\Locale::lookup()
	 * @param object|array $request An action or console request object or an array of locales.
	 * @param array $available A list of locales to negotiate the preferred locale with.
	 * @return string The preferred locale in it's canonical form (i.e. `'fr_CA'`).
	 * @todo Rewrite this to remove hard-coded class names.
	 */
	public static function preferred($request, $available = null) {
		if (is_array($request)) {
			$result = $request;
		} elseif ($request instanceof ActionRequest) {
			$result = static::_preferredAction($request);
		} elseif ($request instanceof ConsoleRequest) {
			$result = static::_preferredConsole($request);
		} else {
			return null;
		}
		if (!$available) {
			return array_shift($result);
		}
		foreach ((array) $result as $locale) {
			if ($match = static::lookup($available, $locale)) {
				return $match;
			}
		}
	}

	/**
	 * Detects preferred locales from an action request by looking at the
	 * `'Accept-Language'` header as described by RFC 2616, section 14.4.
	 *
	 * @link http://www.ietf.org/rfc/rfc2616.txt
	 * @param object $request An instance of `lithium\action\Request`.
	 * @return array Preferred locales in their canonical form (i.e. `'fr_CA'`).
	 */
	protected static function _preferredAction($request) {
		$result = array();
		$regex  = "/^\s*(?P<locale>\w\w(?:[-]\w\w)?)(?:;q=(?P<quality>(0|1|0\.\d+)))?\s*$/";

		foreach (explode(',', $request->env('HTTP_ACCEPT_LANGUAGE')) as $part) {
			if (preg_match($regex, $part, $matches)) {
				$locale = static::canonicalize($matches['locale']);
				$quality = isset($matches['quality']) ? $matches['quality'] : 1;
				$result[$quality][] = $locale;
			}
		}

		krsort($result);
		$return = array();

		foreach ($result as $locales) {
			$return = array_merge($return, array_values($locales));
		}
		return $return;
	}

	/**
	 * Detects preferred locales from a console request by looking at certain
	 * environment variables. The environment variables may be present or not
	 * depending on your system. If multiple variables are present the following
	 * hierarchy is used: `'LANGUAGE'`,  `'LC_ALL'`, `'LANG'`.
	 *
	 * The locales of the `'LC_ALL'` and the `'LANG'` are formatted according
	 * to the posix standard: `language(_territory)(.encoding)(@modifier)`.
	 * Locales having such a format are automatically canonicalized and transformed
	 * into the `Locale` class' format.
	 *
	 * @link http://www.linux.com/archive/feature/53781
	 * @param object $request An instance of `lithium\console\Request`.
	 * @return array Preferred locales in their canonical form (i.e. `'fr_CA'`).
	 */
	protected static function _preferredConsole($request) {
		$regex = '(?P<locale>[\w\_]+)(\.|@|$)+';
		$result = array();

		if ($value = $request->env('LANGUAGE')) {
			return explode(':', $value);
		}
		foreach (array('LC_ALL', 'LANG') as $variable)  {
			$value = $request->env($variable);

			if (!$value || $value === 'C' || $value === 'POSIX') {
				continue;
			}
			if (preg_match("/{$regex}/", $value, $matches)) {
				return (array) $matches['locale'];
			}
		}
		return $result;
	}
}

?>