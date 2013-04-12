<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2012, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\util;

use lithium\util\Set;
use InvalidArgumentException;

/**
 * The `Validator` class provides static access to commonly used data validation logic. These common
 * routines cover HTML form input data such as phone and credit card numbers, dates and postal
 * codes, but also include general checks for regular expressions and booleans and numericality.
 *
 * General data checking is done by using `Validator` statically. Rules can be specified as a
 * parameter to the `rule()` method or accessed directly via the `is[RuleName]()` method name
 * convention:
 *
 * {{{
 * use lithium\util\Validator;
 *
 * // The following are equivalent:
 * Validator::rule('email', 'foo@example.com');  // true
 * Validator::isEmail('foo-at-example.com');     // false
 * }}}
 *
 * Data can also be validated against multiple rules, each having their own associated error
 * message. The rule structure is array-based and hierarchical based on rule names and
 * messages. Responses match the keys present in the `$data` parameter of `check()` up with an array
 * of rules which they violate.
 *
 * {{{ embed:lithium\tests\cases\util\ValidatorTest::testCheckMultipleHasFirstError(1-15) }}}
 *
 * See the `check()` method for more information an multi-value datasets. Custom validation rules
 * can also be added to `Validator` at runtime. These can either take the form of regular expression
 * strings or functions supplied to the `add()` method.
 *
 * ### Rules
 *
 * The `Validator` class includes a series of commonly-used rules by default, any of which may be
 * used in calls to `rule()` or `check()`, or called directly as a method. Additionally, many rules
 * have a variety of different _formats_ in which they may be specified. The following is the list
 * of the built-in rules, but keep in mind that none of them are hard-coded. Any rule may be
 * overridden by adding a new rule of the same name using the `add()` method.
 *
 * - `notEmpty`: Checks that a string contains at least one non-whitespace character.
 *
 * - `alphaNumeric`: Checks that a string contains only integer or letters.
 *
 * - `lengthBetween`: Checks that a string length is within a specified range. Spaces are included
 *   in the character count. The available options are `'min'` and `'max'`, which designate the
 *   minimum and maximum length of the string.
 *
 * - `blank`: Checks that a field is left blank **OR** only whitespace characters are present in its
 *   value. Whitespace characters include spaces, tabs, carriage returns and newlines.
 *
 * - `creditCard`: Checks that a value is a valid credit card number. This rule is divided into a
 *   series of formats: `'amex'`, `'bankcard'`, `'diners'`, `'disc'`, `'electron'`, `'enroute'`,
 *   `'jcb'`, `'maestro'`, `'mc'`, `'solo'`, `'switch'`, `'visa'`, `'voyager'`, `'fast'`. If no
 *   format value is specified, the value defaults to `'any'`, which will validate the value if
 *   _any_ of the available formats match. You can also use the `'fast'` format, which does a
 *   high-speed, low-fidelity check to ensure that the value looks like a real credit card number.
 *   This rule includes one option, `'deep'`, which (if set to `true`) validates the value using the
 *   [Luhn algorithm](http://en.wikipedia.org/wiki/Luhn_algorithm) if the format validation is
 *   successful. See the `luhn` validator below for more details.
 *
 * - `date`: Checks that a value is a valid date that complies with one or more formats. Also
 *   validates leap years. Possible formats are `'dmy'` (27-12-2010 or 27-12-10 separators can be a
 *   space, period, dash, forward slash), `'mdy'` (12-27-2010 or 12-27-10 separators can be a space,
 *   period, dash, forward slash), `'ymd'` (2010-12-27 or 10-12-27 separators can be a space,
 *   period, dash, forward slash), `'dMy'` (27 December 2010 or 27 Dec 2010), `'Mdy'` (December 27,
 *   2010 or Dec 27, 2010 comma is optional), `'My'` (December 2010 or Dec 2010) or `'my'` (12/2010
 *   separators can be a space, period, dash, forward slash).
 *
 * - `time`: Checks that a value is a valid time. Validates time as 24hr (HH:MM) or am/pm
 *   ([ H]H:MM[a|p]m). Does not allow / validate seconds.
 *
 * - `boolean`: Checks that the value is or looks like a boolean value. The following types of
 *   values are interpreted as boolean and will pass the check.
 *   - boolean (`true`, `false`, `'true'`, `'false'`)
 *   - boolean number (`1`, `0`, `'1'`, `'0'`)
 *   - boolean text string (`'on'`, `'off'`, `'yes'`, `'no'`)
 *
 * - `decimal`: Checks that a value is a valid decimal. Takes one option, `'precision'`, which is
 *   an optional integer value defining the level of precision the decimal number must match.
 *
 * - `email`: Checks that a value is (probably) a valid email address. The subject of validating
 *   an actual email address is complicated and problematic. A regular expression that correctly
 *   validates addresses against [RFC 5322](http://tools.ietf.org/html/rfc5322) would be several
 *   pages long, with the drawback of being unable to keep up as new top-level domains are added.
 *   Instead, this validator uses PHP's internal input filtering API to check the format, and
 *   provides an option, `'deep'` ( _boolean_) which, if set to `true`, will validate that the email
 *   address' domain contains a valid MX record. Keep in mind, this is just one of the many ways to
 *   validate an email address in the overall context of an application. For other ideas or
 *   examples, [ask Sean](http://seancoates.com/).
 *
 * - `ip`: Validates a string as a valid IPv4 or IPv6 address.
 *
 * - `money`: Checks that a value is a valid monetary amount. This rule has two formats, `'right'`
 *   and `'left'`, which indicates which side the monetary symbol (i.e. $) appears on.
 *
 * - `numeric`: Checks that a value is numeric.
 *
 * - `phone`: Check that a value is a valid phone number, non-locale-specific phone number.
 *
 * - `postalCode`: Checks that a given value is a valid US postal code.
 *
 * - `inRange`: Checks that a numeric value is within a specified range. This value has two options,
 *    `'upper'` and `'lower'`, which specify the boundary of the value.
 *
 * - `url`: Checks that a value is a valid URL according to
 *   [RFC 2395](http://www.faqs.org/rfcs/rfc2396.html). Uses PHP's filter API, and accepts any
 *   options accepted for
 *   [the validation URL filter](http://www.php.net/manual/en/filter.filters.validate.php).
 *
 * - `luhn`: Checks that a value is a valid credit card number according to the
 *   [Luhn algorithm](http://en.wikipedia.org/wiki/Luhn_algorithm). (See also: the `creditCard`
 *   validator).
 *
 * - `inList`: Checks that a value is in a pre-defined list of values. This validator accepts one
 *   option, `'list'`, which is an array containing acceptable values.
 *
 * - `regex`: Checks that a value appears to be a valid regular expression, possibly
 *   containing PCRE-compatible options flags.
 *
 * - `uuid`: Checks that a value is a valid UUID.
 *
 * ### UTF-8 encoded input strings
 *
 * All rules operating on strings have been created with the possibility of
 * UTF-8 encoded input in mind. A default PHP binary and an enabled Lithium
 * g11n bootstrap will make these rules work correctly in any case. Should you
 * ever experience odd behavior following paragraph with implementation
 * details might help you to track to the cause.
 *
 * The rules `alphaNumeric` and `money` rely on additional functionality of
 * PCRE to validate UTF-8 encoded strings. As no PCRE feature detection is
 * done, having this feature enabled in PCRE isn't optional. Please ensure
 * you've got PCRE compiled with UTF-8 support.
 */
class Validator extends \lithium\core\StaticObject {

	/**
	 * An array of validation rules.  May contain a single regular expression, an array of regular
	 * expressions (where the array keys define various possible 'formats' of the same rule), or a
	 * closure which accepts a value to be validated, and an array of options, and returns a
	 * boolean value, indicating whether the validation succeeded or failed.
	 *
	 * @var array
	 * @see lithium\util\Validator::add()
	 * @see lithium\util\Validator::rule()
	 */
	protected static $_rules = array();

	/**
	 * Default options used when defining a new validator rule. Each key contains method-specific
	 * options that should always be applied, or options that should be applied to all rules in the
	 * `'defaults'` key.
	 *
	 * @see lithium\util\Validator::add()
	 * @see lithium\util\Validator::rule()
	 * @var array
	 */
	protected static $_options = array(
		'defaults' => array('contains' => true)
	);

	/**
	 * Initializes the list of default validation rules.
	 *
	 * @return void
	 */
	public static function __init() {
		$alnum = '[A-Fa-f0-9]';
		$class = get_called_class();
		static::$_methodFilters[$class] = array();

		static::$_rules = array(
			'alphaNumeric' => '/^[\p{Ll}\p{Lm}\p{Lo}\p{Lt}\p{Lu}\p{Nd}]+$/mu',
			'blank'        => '/[^\\s]/',
			'creditCard'   => array(
				'amex'     => '/^3[4|7]\\d{13}$/',
				'bankcard' => '/^56(10\\d\\d|022[1-5])\\d{10}$/',
				'diners'   => '/^(?:3(0[0-5]|[68]\\d)\\d{11})|(?:5[1-5]\\d{14})$/',
				'disc'     => '/^(?:6011|650\\d)\\d{12}$/',
				'electron' => '/^(?:417500|4917\\d{2}|4913\\d{2})\\d{10}$/',
				'enroute'  => '/^2(?:014|149)\\d{11}$/',
				'jcb'      => '/^(3\\d{4}|2100|1800)\\d{11}$/',
				'maestro'  => '/^(?:5020|6\\d{3})\\d{12}$/',
				'mc'       => '/^5[1-5]\\d{14}$/',
				'solo'     => '/^(6334[5-9][0-9]|6767[0-9]{2})\\d{10}(\\d{2,3})?$/',
				'switch'   => '/^(?:49(03(0[2-9]|3[5-9])|11(0[1-2]|7[4-9]|8[1-2])|36[0-9]{2})' .
				              '\\d{10}(\\d{2,3})?)|(?:564182\\d{10}(\\d{2,3})?)|(6(3(33[0-4]' .
				              '[0-9])|759[0-9]{2})\\d{10}(\\d{2,3})?)$/',
				'visa'     => '/^4\\d{12}(\\d{3})?$/',
				'voyager'  => '/^8699[0-9]{11}$/',
				'fast'     => '/^(?:4[0-9]{12}(?:[0-9]{3})?|5[1-5][0-9]{14}|6011[0-9]{12}|3' .
				              '(?:0[0-5]|[68][0-9])[0-9]{11}|3[47][0-9]{13})$/'
			),
			'date'         => array(
				'dmy'      => '%^(?:(?:31(\\/|-|\\.|\\x20)(?:0?[13578]|1[02]))\\1|(?:(?:29|30)' .
				              '(\\/|-|\\.|\\x20)(?:0?[1,3-9]|1[0-2])\\2))(?:(?:1[6-9]|[2-9]\\d)?' .
				              '\\d{2})$|^(?:29(\\/|-|\\.|\\x20)0?2\\3(?:(?:(?:1[6-9]|[2-9]\\d)?' .
				              '(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])' .
				              '00))))$|^(?:0?[1-9]|1\\d|2[0-8])(\\/|-|\\.|\\x20)(?:(?:0?[1-9])|' .
				              '(?:1[0-2]))\\4(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$%',
				'mdy'      => '%^(?:(?:(?:0?[13578]|1[02])(\\/|-|\\.|\\x20)31)\\1|(?:(?:0?[13-9]|' .
				              '1[0-2])(\\/|-|\\.|\\x20)(?:29|30)\\2))(?:(?:1[6-9]|[2-9]\\d)?\\d' .
				              '{2})$|^(?:0?2(\\/|-|\\.|\\x20)29\\3(?:(?:(?:1[6-9]|[2-9]\\d)?' .
				              '(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])' .
				              '00))))$|^(?:(?:0?[1-9])|(?:1[0-2]))(\\/|-|\\.|\\x20)(?:0?[1-9]|1' .
				              '\\d|2[0-8])\\4(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$%',
				'ymd'      => '%^(?:(?:(?:(?:(?:1[6-9]|[2-9]\\d)?(?:0[48]|[2468][048]|[13579]' .
				              '[26])|(?:(?:16|[2468][048]|[3579][26])00)))(\\/|-|\\.|\\x20)' .
				              '(?:0?2\\1(?:29)))|(?:(?:(?:1[6-9]|[2-9]\\d)?\\d{2})(\\/|-|\\.|' .
				              '\\x20)(?:(?:(?:0?[13578]|1[02])\\2(?:31))|(?:(?:0?[1,3-9]|1[0-2])' .
				              '\\2(29|30))|(?:(?:0?[1-9])|(?:1[0-2]))\\2(?:0?[1-9]|1\\d|2[0-8]' .
				              '))))$%',
				'dMy'      => '/^((31(?!\\ (Feb(ruary)?|Apr(il)?|June?|(Sep(?=\\b|t)t?|Nov)' .
				              '(ember)?)))|((30|29)(?!\\ Feb(ruary)?))|(29(?=\\ Feb(ruary)?\\ ' .
				              '(((1[6-9]|[2-9]\\d)(0[48]|[2468][048]|[13579][26])|((16|[2468]' .
				              '[048]|[3579][26])00)))))|(0?[1-9])|1\\d|2[0-8])\\ (Jan(uary)?|' .
				              'Feb(ruary)?|Ma(r(ch)?|y)|Apr(il)?|Ju((ly?)|(ne?))|Aug(ust)?|' .
				              'Oct(ober)?|(Sep(?=\\b|t)t?|Nov|Dec)(ember)?)\\ ((1[6-9]|[2-9]' .
				              '\\d)\\d{2})$/',
				'Mdy'      => '/^(?:(((Jan(uary)?|Ma(r(ch)?|y)|Jul(y)?|Aug(ust)?|Oct(ober)?' .
				              '|Dec(ember)?)\\ 31)|((Jan(uary)?|Ma(r(ch)?|y)|Apr(il)?|Ju((ly?)' .
				              '|(ne?))|Aug(ust)?|Oct(ober)?|(Sept|Nov|Dec)(ember)?)\\ (0?[1-9]' .
				              '|([12]\\d)|30))|(Feb(ruary)?\\ (0?[1-9]|1\\d|2[0-8]|(29(?=,?\\ ' .
				              '((1[6-9]|[2-9]\\d)(0[48]|[2468][048]|[13579][26])|((16|[2468]' .
				              '[048]|[3579][26])00)))))))\\,?\\ ((1[6-9]|[2-9]\\d)\\d{2}))$/',
				'My'       => '%^(Jan(uary)?|Feb(ruary)?|Ma(r(ch)?|y)|Apr(il)?|Ju((ly?)|(ne?))|' .
				              'Aug(ust)?|Oct(ober)?|(Sep(?=\\b|t)t?|Nov|Dec)(ember)?)[ /]((1[6-9]' .
				              '|[2-9]\\d)\\d{2})$%',
				'my'       => '%^(((0[123456789]|10|11|12)([- /.])(([1][9][0-9][0-9])|([2][0-9]' .
				              '[0-9][0-9]))))$%'
			),
			'ip' => function($value, $format = null, array $options = array()) {
				$options += array('flags' => array());
				return (boolean) filter_var($value, FILTER_VALIDATE_IP, $options);
			},
			'money'        => array(
				'right'    => '/^(?!0,?\d)(?:\d{1,3}(?:([, .])\d{3})?(?:\1\d{3})*|(?:\d+))' .
				              '((?!\1)[,.]\d{2})?(?<!\x{00a2})\p{Sc}?$/u',
				'left'     => '/^(?!\x{00a2})\p{Sc}?(?!0,?\d)(?:\d{1,3}(?:([, .])\d{3})?' .
				              '(?:\1\d{3})*|(?:\d+))((?!\1)[,.]\d{2})?$/u'
			),
			'notEmpty'     => '/[^\s]+/m',
			'phone'        => '/^\+?[0-9\(\)\-]{10,20}$/',
			'postalCode'   => '/(^|\A\b)[A-Z0-9\s\-]{5,}($|\b\z)/i',
			'regex'        => '/^(?:([^[:alpha:]\\\\{<\[\(])(.+)(?:\1))|(?:{(.+)})|(?:<(.+)>)|' .
			                  '(?:\[(.+)\])|(?:\((.+)\))[gimsxu]*$/',
			'time'         => '%^((0?[1-9]|1[012])(:[0-5]\d){0,2}([AP]M|[ap]m))$|^([01]\d|2[0-3])' .
			                  '(:[0-5]\d){0,2}$%',
			'boolean'      => function($value) {
				$bool = is_bool($value);
				$filter = filter_var($value, FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE);
				return ($bool || $filter !== null || empty($value));
			},
			'decimal' => function($value, $format = null, array $options = array()) {
				if (isset($options['precision'])) {
					$precision = strlen($value) - strrpos($value, '.') - 1;

					if ($precision !== (int) $options['precision']) {
						return false;
					}
				}
				return (filter_var($value, FILTER_VALIDATE_FLOAT, FILTER_NULL_ON_FAILURE) !== null);
			},
			'inList' => function($value, $format, $options) {
				$options += array('list' => array());
				$strict = is_bool($value) || $value === '';
				return in_array($value, $options['list'], $strict);
			},
			'lengthBetween' => function($value, $format, $options) {
				$length = strlen($value);
				$options += array('min' => 1, 'max' => 255);
				return ($length >= $options['min'] && $length <= $options['max']);
			},
			'luhn' => function($value) {
				if (empty($value) || !is_string($value)) {
					return false;
				}
				$sum = 0;
				$length = strlen($value);

				for ($position = 1 - ($length % 2); $position < $length; $position += 2) {
					$sum += $value[$position];
				}
				for ($position = ($length % 2); $position < $length; $position += 2) {
					$number = $value[$position] * 2;
					$sum += ($number < 10) ? $number : $number - 9;
				}
				return ($sum % 10 === 0);
			},
			'numeric' => function($value) {
				return is_numeric($value);
			},
			'inRange' => function($value, $format, $options) {
				$defaults = array('upper' => null, 'lower' => null);
				$options += $defaults;

				if (!is_numeric($value)) {
					return false;
				}
				switch (true) {
					case (!is_null($options['upper']) && !is_null($options['lower'])):
						return ($value >= $options['lower'] && $value <= $options['upper']);
					case (!is_null($options['upper'])):
						return ($value <= $options['upper']);
					case (!is_null($options['lower'])):
						return ($value >= $options['lower']);
				}
				return is_finite($value);
			},
			'uuid' => "/^{$alnum}{8}-{$alnum}{4}-{$alnum}{4}-{$alnum}{4}-{$alnum}{12}$/",
			'email' => function($value) {
				return filter_var($value, FILTER_VALIDATE_EMAIL);
			},
			'url' => function($value, $format = null, array $options = array()) {
				$options += array('flags' => array());
				return (boolean) filter_var($value, FILTER_VALIDATE_URL, $options);
			}
		);

		$isEmpty = function($self, $params, $chain) {
			extract($params);
			return (empty($value) && $value !== '0') ? false : $chain->next($self, $params, $chain);
		};

		static::$_methodFilters[$class]['alphaNumeric'] = array($isEmpty);
		static::$_methodFilters[$class]['notEmpty'] = array($isEmpty);

		static::$_methodFilters[$class]['creditCard'] = array(function($self, $params, $chain) {
			extract($params);
			$options += array('deep' => false);

			if (strlen($value = str_replace(array('-', ' '), '', $value)) < 13) {
				return false;
			}
			if (!$chain->next($self, compact('value') + $params, $chain)) {
				return false;
			}
			return $options['deep'] ? Validator::isLuhn($value) : true;
		});

		static::$_methodFilters[$class]['email'] = array(function($self, $params, $chain) {
			extract($params);
			$defaults = array('deep' => false);
			$options += $defaults;

			if (!$chain->next($self, $params, $chain)) {
				return false;
			}
			if (!$options['deep']) {
				return true;
			}
			list($prefix, $host) = explode('@', $params['value']);

			if (getmxrr($host, $mxhosts)) {
				return is_array($mxhosts);
			}
			return false;
		});
	}

	/**
	 * Maps method calls to validation rule names.  For example, a validation rule that would
	 * normally be called as `Validator::rule('email', 'foo@bar.com')` can also be called as
	 * `Validator::isEmail('foo@bar.com')`.
	 *
	 * @param string $method The name of the method called, i.e. `'isEmail'` or `'isCreditCard'`.
	 * @param array $args
	 * @return boolean
	 */
	public static function __callStatic($method, $args = array()) {
		if (!isset($args[0])) {
			return false;
		}
		$args = array_filter($args) + array(0 => $args[0], 1 => 'any', 2 => array());
		$rule = preg_replace("/^is([A-Z][A-Za-z0-9]+)$/", '$1', $method);
		$rule[0] = strtolower($rule[0]);
		return static::rule($rule, $args[0], $args[1], $args[2]);
	}

	/**
	 * Custom check to determine if our given magic methods can be responded to.
	 *
	 * @param  string  $method     Method name.
	 * @param  bool    $internal   Interal call or not.
	 * @return bool
	 */
	public static function respondsTo($method, $internal = false) {
		$rule = preg_replace("/^is([A-Z][A-Za-z0-9]+)$/", '$1', $method);
		$rule[0] = strtolower($rule[0]);
		return isset(static::$_rules[$rule]) || parent::respondsTo($method, $internal);
	}

	/**
	 * Checks a set of values against a specified rules list. This method may be used to validate
	 * any arbitrary array of data against a set of validation rules.
	 *
	 * @param array $values An array of key/value pairs, where the values are to be checked.
	 * @param array $rules An array of rules to check the values in `$values` against. Each key in
	 *              `$rules` should match a key contained in `$values`, and each value should be a
	 *              validation rule in one of the allowable formats. For example, if you are
	 *              validating a data set containing a `'credit_card'` key, possible values for
	 *              `$rules` would be as follows:
	 *              - `array('credit_card' => 'You must include a credit card number')`: This is the
	 *                simplest form of validation rule, in which the value is simply a message to
	 *                display if the rule fails. Using this format, all other validation settings
	 *                inherit from the defaults, including the validation rule itself, which only
	 *                checks to see that the corresponding key in `$values` is present and contains
	 *                a value that is not empty. _Please note when globalizing validation messages:_
	 *                When specifying messages, it may be preferable to use a code string (i.e.
	 *                `'ERR_NO_TITLE'`) instead of the full text of the validation error. These code
	 *                strings may then be translated by the appropriate tools in the templating
	 *                layer.
	 *              - `array('credit_card' => array('creditCard', 'message' => 'Invalid CC #'))`:
	 *                In the second format, the validation rule (in this case `creditCard`) and
	 *                associated configuration are specified as an array, where the rule to use is
	 *                the first value in the array (no key), and additional settings are specified
	 *                as other keys in the array. Please see the list below for more information on
	 *                allowed keys.
	 *              - The final format allows you to apply multiple validation rules to a single
	 *                value, and it is specified as follows:
	 *
	 * `array('credit_card' => array(
	 * 	array('notEmpty', 'message' => 'You must include credit card number'),
	 * 	array('creditCard', 'message' => 'Your credit card number must be valid')
	 * ));`
	 * @param array $options Validator-specific options.
	 *
	 * Each rule defined as an array can contain any of the following settings (in addition to the
	 * first value, which represents the rule to be used):
	 *  - `'message'` _string_: The error message to be returned if the validation rule fails. See
	 *    the note above regarding globalization of error messages.
	 *  - `'required`' _boolean_: Represents whether the value is required to be present in
	 *    `$values`. If `'required'` is set to `false`, the validation rule will be skipped if the
	 *     corresponding key is not present. Defaults to `true`.
	 *  - `'skipEmpty'` _boolean_: Similar to `'required'`, this setting (if `true`) will cause the
	 *    validation rule to be skipped if the corresponding value is empty (an empty string or
	 *    `null`). Defaults to `false`.
	 *  - `'format'` _string_: If the validation rule has multiple format definitions (see the
	 *    `add()` or `__init()` methods), the name of the format to be used can be specified here.
	 *    Additionally, two special values can be used: either `'any'`, which means that all formats
	 *    will be checked and the rule will pass if any format passes, or `'all'`, which requires
	 *    all formats to pass in order for the rule check to succeed.
	 * @return array Returns an array containing all validation failures for data in `$values`,
	 *         where each key matches a key in `$values`, and each value is an array of that
	 *         element's validation errors.
	 * @filter
	 */
	public static function check(array $values, array $rules, array $options = array()) {
		$defaults = array(
			'notEmpty',
			'message' => null,
			'required' => true,
			'skipEmpty' => false,
			'format' => 'any',
			'on' => null,
			'last' => false
		);

		$options += $defaults;
		$params = compact('values', 'rules', 'options');

		return static::_filter(__FUNCTION__, $params, function($self, $params) {
			$values = $params['values'];
			$rules = $params['rules'];
			$options = $params['options'];

			$errors = array();
			$events = (array) (isset($options['events']) ? $options['events'] : null);
			$values = Set::flatten($values);

			foreach ($rules as $field => $rules) {
				$rules = is_string($rules) ? array('message' => $rules) : $rules;
				$rules = is_array(current($rules)) ? $rules : array($rules);
				$errors[$field] = array();
				$options['field'] = $field;

				foreach ($rules as $key => $rule) {
					$rule += $options + compact('values');
					list($name) = $rule;

					if ($events && $rule['on'] && !array_intersect($events, (array) $rule['on'])) {
						continue;
					}
					if (!array_key_exists($field, $values)) {
						if ($rule['required']) {
							$errors[$field][] = $rule['message'] ?: $key;
						}
						if ($rule['last']) {
							break;
						}
						continue;
					}
					if (empty($values[$field]) && $rule['skipEmpty']) {
						continue;
					}

					if (!$self::rule($name, $values[$field], $rule['format'], $rule + $options)) {
						$errors[$field][] = $rule['message'] ?: $key;

						if ($rule['last']) {
							break;
						}
					}
				}
			}
			return array_filter($errors);
		});
	}

	/**
	 * Adds to or replaces built-in validation rules specified in `Validator::$_rules`.  Any new
	 * validation rules created are automatically callable as validation methods.
	 *
	 * For example:
	 * {{{
	 * Validator::add('zeroToNine', '/^[0-9]$/');
	 * $isValid = Validator::isZeroToNine("5"); // true
	 * $isValid = Validator::isZeroToNine("20"); // false
	 * }}}
	 *
	 * Alternatively, the first parameter may be an array of rules expressed as key/value pairs,
	 * as in the following:
	 * {{{
	 * Validator::add(array(
	 * 	'zeroToNine' => '/^[0-9]$/',
	 * 	'tenToNineteen' => '/^1[0-9]$/',
	 * ));
	 * }}}
	 *
	 * In addition to regular expressions, validation rules can also be defined as full anonymous
	 * functions:
	 * {{{
	 * use app\models\Account;
	 *
	 * Validator::add('accountActive', function($value) {
	 * 	$value = is_int($value) ? Account::find($value) : $value;
	 * 	return (boolean) $value->is_active;
	 * });
	 *
	 * $testAccount = Account::create(array('is_active' => false));
	 * Validator::isAccountActive($testAccount); // returns false
	 * }}}
	 *
	 * These functions can take up to 3 parameters:
	 * 	- `$value` _mixed_: This is the actual value to be validated (as in the above example).
	 * 	- `$format` _string_: Often, validation rules come in multiple "formats", for example:
	 * 	  postal codes, which vary by country or region. Defining multiple formats allows you to
	 * 	  retian flexibility in how you validate data. In cases where a user's country of origin
	 * 	  is known, the appropriate validation rule may be selected. In cases where it is not
	 * 	  known, the value of `$format` may be `'any'`, which should pass if any format matches.
	 * 	  In cases where validation rule formats are not mutually exclusive, the value may be
	 * 	  `'all'`, in which case all must match.
	 * 	- `$options` _array_: This parameter allows a validation rule to implement custom
	 * 	  options.
	 *
	 * @see lithium\util\Validator::$_rules
	 * @param mixed $name The name of the validation rule (string), or an array of key/value pairs
	 *              of names and rules.
	 * @param string $rule If $name is a string, this should be a string regular expression, or a
	 *               closure that returns a boolean indicating success. Should be left blank if
	 *               `$name` is an array.
	 * @param array $options The default options for validating this rule. An option which applies
	 *              to all regular expression rules is `'contains'` which, if set to true, allows
	 *              validated values to simply _contain_ a match to a rule, rather than exactly
	 *              matching it in whole.
	 * @return void
	 */
	public static function add($name, $rule = null, array $options = array()) {
		if (!is_array($name)) {
			$name = array($name => $rule);
		}
		static::$_rules = Set::merge(static::$_rules, $name);

		if (!empty($options)) {
			$options = array_combine(array_keys($name), array_fill(0, count($name), $options));
			static::$_options = Set::merge(static::$_options, $options);
		}
	}

	/**
	 * Checks a single value against a single validation rule in one or more formats.
	 *
	 * @param string $rule
	 * @param mixed $value
	 * @param string $format
	 * @param array $options
	 * @return boolean Returns `true` or `false` indicating whether the validation rule check
	 *         succeeded or failed.
	 * @filter
	 */
	public static function rule($rule, $value, $format = 'any', array $options = array()) {
		if (!isset(static::$_rules[$rule])) {
			throw new InvalidArgumentException("Rule `{$rule}` is not a validation rule.");
		}
		$defaults = isset(static::$_options[$rule]) ? static::$_options[$rule] : array();
		$options = (array) $options + $defaults + static::$_options['defaults'];

		$ruleCheck = static::$_rules[$rule];
		$ruleCheck = is_array($ruleCheck) ? $ruleCheck : array($ruleCheck);

		if (!$options['contains'] && !empty($ruleCheck)) {
			foreach ($ruleCheck as $key => $item) {
				$ruleCheck[$key] = is_string($item) ? "/^{$item}$/" : $item;
			}
		}

		$params = compact('value', 'format', 'options');
		return static::_filter($rule, $params, static::_checkFormats($ruleCheck));
	}

	/**
	 * Returns a list of available validation rules, or the configuration details of a single rule.
	 *
	 * @param string $name Optional name of a rule to get the details of. If not specified, an array
	 *               of all available rule names is returned. Otherwise, returns the details of a
	 *               single rule. This can be a regular expression string, a closure object, or an
	 *               array of available rule formats made up of string regular expressions,
	 *               closures, or both.
	 * @return mixed Returns either an single array of rule names, or the details of a single rule.
	 */
	public static function rules($name = null) {
		if (!$name) {
			return array_keys(static::$_rules);
		}
		return isset(static::$_rules[$name]) ? static::$_rules[$name] : null;
	}

	/**
	 * Perform validation checks against a value using an array of all possible formats for a rule,
	 * and an array specifying which formats within the rule to use.
	 *
	 * @param array $rules All available rules.
	 * @return closure Function returning boolean `true` if validation succeeded, `false` otherwise.
	 */
	protected static function _checkFormats($rules) {
		return function($self, $params, $chain) use ($rules) {
			$value = $params['value'];
			$format = $params['format'];
			$options = $params['options'];

			$defaults = array('all' => true);
			$options += $defaults;

			$formats = (array) $format;
			$options['all'] = ($format === 'any');

			foreach ($rules as $index => $check) {
				if (!$options['all'] && !(in_array($index, $formats) || isset($formats[$index]))) {
					continue;
				}

				$regexPassed = (is_string($check) && preg_match($check, $value));
				$closurePassed = (is_object($check) && $check($value, $format, $options));

				if (!$options['all'] && ($regexPassed || $closurePassed)) {
					return true;
				}
				if ($options['all'] && (!$regexPassed && !$closurePassed)) {
					return false;
				}
			}
			return $options['all'];
		};
	}
}

?>