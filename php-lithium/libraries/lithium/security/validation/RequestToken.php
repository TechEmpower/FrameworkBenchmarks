<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\security\validation;

use lithium\security\Password;
use lithium\util\String;
use lithium\util\Set;

/**
 * The `RequestToken` class creates cryptographically-secure tokens and keys that can be used to
 * validate the authenticity of client requests.
 *
 * `RequestToken` will persist the token for the life of
 * the client session, and generate per-request keys that will match against that token.
 *
 * Using these token/key pairs in forms and other non-idempotent requests will help you secure your
 * application against cross-site request forgeries, or CSRF attacks.
 *
 * ### Example
 *
 * {{{
 * // views/comments/add.html.php:
 * // ...
 * <?=$this->form->create($object); ?>
 * 	<?=$this->security->requestToken(); ?>
 * 	// Other fields...
 * <?=$this->form->end(); ?>
 * }}}
 *
 * {{{
 * // controllers/CommentsController.php:
 * public function add() {
 * 	if ($this->request->data && !RequestToken::check($this->request)) {
 * 		// Key didn't match the CSRF token. Regenerate the session token and
 * 		// prompt the user to retry the form submission.
 * 		RequestToken::get(array('regenerate' => true));
 * 		return;
 * 	}
 * 	// Handle a normal request...
 * }
 * }}}
 * @link http://shiflett.org/articles/cross-site-request-forgeries Cross-Site Request Forgeries
 * @see lithium\template\helper\Security::requestToken()
 */
class RequestToken {

	/**
	 * Class dependencies.
	 *
	 * @var array
	 */
	protected static $_classes = array(
		'session' => 'lithium\storage\Session'
	);

	/**
	 * Used to get or reconfigure dependencies with custom classes.
	 *
	 * @param array $config When assigning new configuration, should be an array containing a
	 *              `'classes'` key.
	 * @return array If `$config` is empty, returns an array with a `'classes'` key containing class
	 *         dependencies. Otherwise returns `null`.
	 */
	public static function config(array $config = array()) {
		if (!$config) {
			return array('classes' => static::$_classes);
		}

		foreach ($config as $key => $val) {
			$key = "_{$key}";

			if (isset(static::${$key})) {
				static::${$key} = $val + static::${$key};
			}
		}
	}

	/**
	 * Generates (or regenerates) a cryptographically-secure token to be used for the life of the
	 * client session, and stores the token using the `Session` class.
	 *
	 * @see lithium\util\String::hash()
	 * @param array $options An array of options to be used when generating or storing the token:
	 *              - `'regenerate'` _boolean_: If `true`, will force the regeneration of a the
	 *                token, even if one is already available in the session. Defaults to `false`.
	 *              - `'sessionKey'` _string_: The key used for session storage and retrieval.
	 *                Defaults to `'security.token'`.
	 *              - `'salt'` _string_: If the token is being generated (or regenerated), sets a
	 *                custom salt value to be used by `String::hash()`.
	 *              - `'type'` _string_: The hashing algorithm used by `String::hash()` when
	 *                generating the token. Defaults to `'sha512'`.
	 * @return string Returns a cryptographically-secure client session token.
	 */
	public static function get(array $options = array()) {
		$defaults = array(
			'regenerate' => false,
			'sessionKey' => 'security.token',
			'salt' => null,
			'type' => 'sha512'
		);
		$options += $defaults;
		$session = static::$_classes['session'];

		if ($options['regenerate'] || !($token = $session::read($options['sessionKey']))) {
			$token = String::hash(uniqid(microtime(true)), $options);
			$session::write($options['sessionKey'], $token);
		}
		return $token;
	}

	/**
	 * Generates a single-use key to be embedded in a form or used with another non-idempotent
	 * request (a request that changes the state of the server or application), that will match
	 * against a client session token using the `check()` method.
	 *
	 * @see lithium\security\validation\RequestToken::check()
	 * @param array $options An array of options to be passed to `RequestToken::get()`.
	 * @return string Returns a hashed key string for use with `RequestToken::check()`.
	 */
	public static function key(array $options = array()) {
		return Password::hash(static::get($options));
	}

	/**
	 * Checks a single-use hash key against the session token that generated it, using
	 * a cryptographically-secure verification method. Accepts either the request key as a string,
	 * or a `Request` object with a `$data` property containing a `['security']['token']` key.
	 *
	 * For example, the following two controller code samples are equivalent:
	 *
	 * {{{
	 * $key = $this->request->data['security']['token'];
	 *
	 * if (!RequestToken::check($key)) {
	 * 	// Handle invalid request...
	 * }
	 * }}}
	 *
	 * {{{
	 * if (!RequestToken::check($this->request)) {
	 * 	// Handle invalid request...
	 * }
	 * }}}
	 *
	 * @param mixed $key Either the actual key as a string, or a `Request` object containing the
	 *              key.
	 * @param array $options The options to use when matching the key to the token:
	 *              - `'sessionKey'` _string_: The key used when reading the token from the session.
	 * @return boolean Returns `true` if the hash key is a cryptographic match to the stored
	 *         session token. Returns `false` on failure, which indicates a forged request attempt.
	 */
	public static function check($key, array $options = array()) {
		$defaults = array('sessionKey' => 'security.token');
		$options += $defaults;
		$session = static::$_classes['session'];

		if (is_object($key) && isset($key->data)) {
			$result = Set::extract($key->data, '/security/token');
			$key = $result ? $result[0] : null;
		}
		return Password::check($session::read($options['sessionKey']), (string) $key);
	}
}

?>