<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\security\validation;

use lithium\action\Request;
use lithium\security\Password;
use lithium\security\validation\RequestToken;

class RequestTokenTest extends \lithium\test\Unit {

	protected static $_storage = array();

	public function setUp() {
		self::$_storage = array();
		RequestToken::config(array('classes' => array('session' => __CLASS__)));
	}

	public function tearDown() {
		RequestToken::config(array('classes' => array('session' => 'lithium\storage\Session')));
	}

	public static function read($key) {
		return isset(static::$_storage[$key]) ? static::$_storage[$key] : null;
	}

	public static function write($key, $val) {
		return static::$_storage[$key] = $val;
	}

	/**
	 * Tests that class dependencies can be reconfigured.
	 */
	public function testConfiguration() {
		$expected = array('classes' => array('session' => __CLASS__));
		$this->assertEqual($expected, RequestToken::config());

		$new = array('classes' => array('session' => 'lithium\storage\Session'));
		RequestToken::config($new);
		$this->assertEqual($new, RequestToken::config());
	}

	/**
	 * Tests proper generation of secure tokens.
	 */
	public function testTokenGeneration() {
		$token = RequestToken::get();
		$this->assertPattern('/^[a-f0-9]{128}$/', $token);
		$this->assertEqual(array('security.token' => $token), self::$_storage);

		$newToken = RequestToken::get();
		$this->assertEqual($token, $newToken);

		$reallyNewToken = RequestToken::get(array('regenerate' => true));
		$this->assertPattern('/^[a-f0-9]{128}$/', $reallyNewToken);
		$this->assertNotEqual($token, $reallyNewToken);
		$this->assertEqual(array('security.token' => $reallyNewToken), self::$_storage);
	}

	/**
	 * Tests that a random sequence of keys and tokens properly match one another.
	 */
	public function testKeyMatching() {
		for ($i = 0; $i < 4; $i++) {
			$token = RequestToken::get(array('regenerate' => true));

			for ($j = 0; $j < 4; $j++) {
				$key = Password::hash($token);
				$this->assertTrue(RequestToken::check($key));
			}
		}
	}

	/**
	 * Tests extracting a key from a `Request` object and matching it against a token.
	 */
	public function testTokenFromRequestObject() {
		$request = new Request(array('data' => array(
			'security' => array('token' => RequestToken::key())
		)));
		$this->assertTrue(RequestToken::check($request));
	}
}

?>