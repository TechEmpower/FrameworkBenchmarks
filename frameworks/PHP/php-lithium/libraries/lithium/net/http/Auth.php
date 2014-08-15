<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\net\http;

/**
 * The `Auth` class handles HTTP Authentication encoding and decode. Typically, this class is not
 * used directly, but is a utility of `action\Request`  and `security\auth\adapter\Http`
 */
class Auth extends \lithium\core\StaticObject {

	/**
	 * The NC value needed for digest authentication
	 *
	 */
	public static $nc = '00000001';

	/**
	 * Returns the proper header string. Accepts the data from the `encode` method.
	 *
	 * @param array $data
	 * @return string
	 */
	public static function header($data) {
		if (empty($data['response'])) {
			return null;
		}
		if (!empty($data['opaque'])) {
			$defaults =  array(
				'realm' => 'app', 'method' => 'GET', 'uri' => '/',
				'username' => null, 'qop' => 'auth',
				'nonce' => null, 'opaque' => null,
				'cnonce' => md5(time()),  'nc' => static::$nc
			);
			$data += $defaults;
			$auth = "username=\"{$data['username']}\", response=\"{$data['response']}\", ";
			$auth .= "uri=\"{$data['uri']}\", realm=\"{$data['realm']}\", ";
			$auth .= "qop=\"{$data['qop']}\", nc={$data['nc']}, cnonce=\"{$data['cnonce']}\", ";
			$auth .= "nonce=\"{$data['nonce']}\", opaque=\"{$data['opaque']}\"";
			return "Digest " . $auth;
		}
		return "Basic " . $data['response'];
	}

	/**
	 * Encoded the data with username and password to create the proper response. Returns an array
	 * containing the username and encoded response.
	 *
	 * @param string $username Username to authenticate with
	 * @param string $password Password to authenticate with
	 * @param array $data Params needed to hash the response
	 * @return array
	 */
	public static function encode($username, $password, $data = array()) {
		if (isset($data['nonce'])) {
			$defaults = array(
				'realm' => 'app', 'method' => 'GET', 'uri' => '/', 'qop' => null,
				'cnonce' => md5(time()), 'nc' => static::$nc
			);
			$data = array_filter($data) + $defaults;
			$part1 = md5("{$username}:{$data['realm']}:{$password}");
			$part2 = "{$data['nonce']}:{$data['nc']}:{$data['cnonce']}:{$data['qop']}";
			$part3 = md5($data['method'] . ':' . $data['uri']);
			$response = md5("{$part1}:{$part2}:{$part3}");
			return compact('username', 'response') + $data;
		}
		$response = base64_encode("{$username}:{$password}");
		return compact('username', 'response');
	}

	/**
	 * Takes the header string and parses out the params needed for a digest authentication.
	 *
	 * @param string $header
	 * @return array
	 */
	public static function decode($header) {
		$data = array(
			'realm' => null, 'username' => null, 'uri' => null,
			'nonce' => null, 'opaque' => null, 'qop' => null,
			'cnonce' => null, 'nc' => null,
			'response' => null
		);
		$keys = implode('|', array_keys($data));
		$regex = '@(' . $keys . ')=(?:([\'"])([^\2]+?)\2|([^\s,]+))@';
		preg_match_all($regex, $header, $matches, PREG_SET_ORDER);

		foreach ($matches as $m) {
			if (!isset($m[3]) && !isset($m[4])) {
				continue;
			}
			$data[$m[1]] = $m[3] ? $m[3] : $m[4];
		}
		return $data;
	}
}

?>