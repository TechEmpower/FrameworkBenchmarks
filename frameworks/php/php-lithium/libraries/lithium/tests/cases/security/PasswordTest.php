<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\security;

use lithium\security\Password;

class PasswordTest extends \lithium\test\Unit {

	/**
	 * The password to be encrypted.
	 */
	protected $_password = 'lith1um';

	/**
	 * Tests the `Password::hash()` method with both generated and
	 * custom salts.
	 */
	public function testHash() {
		$this->skipIf(!CRYPT_BLOWFISH, 'Blowfish is not supported.');

		$salt = '$2a$07$l1th1um1saw3some12345678$';
		$expected = '$2a$07$l1th1um1saw3some12345uDt5Wuw5uzI5lCIn3HM1QkB7cJLou4Hy';
		$result = Password::hash($this->_password, $salt);
		$this->assertEqual($expected, $result);

		$result = Password::hash($this->_password);
		$this->assertNotEqual($expected, $result);
	}

	/**
	 * Tests the `Password::check()` method to make sure that it returns
	 * either true or false, depending on the input.
	 */
	public function testCheck() {
		$this->skipIf(!CRYPT_BLOWFISH, 'Blowfish is not supported.');

		$salt = '$2a$07$l1th1um1saw3some12345678$';
		$hash = Password::hash($this->_password, $salt);
		$this->assertTrue(Password::check($this->_password, $hash));

		$hash = Password::hash($this->_password);
		$this->assertTrue(Password::check($this->_password, $hash));

		$wrong = 'wr0ng';
		$this->assertFalse(Password::check($wrong, $hash));
	}

	/**
	 * Tests salting passwords with the Blowfish algorithm.
	 *
	 * It also contains tests to prove that password longer than 72 characters
	 * are translated into the same hash.
	 */
	public function testSaltBlowfish() {
		$this->skipIf(!CRYPT_BLOWFISH, 'Blowfish is not supported.');

		$saltPattern = "{^\\$2a\\$06\\$[0-9A-Za-z./]{22}$}";
		$hashPattern = "{^\\$2a\\$06\\$[0-9A-Za-z./]{53}$}";

		$log2 = 6;
		$salt = Password::salt('bf', $log2);
		$this->assertPattern($saltPattern, $salt);
		$this->assertNotEqual($salt, Password::salt('bf', $log2));

		$hash = Password::hash($this->_password, $salt);
		$hash2 = Password::hash($this->_password, Password::salt('bf', $log2));
		$this->assertPattern($hashPattern, $hash);
		$this->assertNotEqual($hash, $hash2);

		$maxLength = 72;
		$salt = Password::salt('bf');
		$password = str_repeat('a', $maxLength);
		$expected = Password::hash($password, $salt);
		$result = Password::hash($password . 'a',  $salt);
		$this->assertIdentical($expected, $result);
	}

	/**
	 * Tests salting passwords with the Extended-DES algorithm.
	 */
	public function testSaltXDES() {
		$this->skipIf(!CRYPT_EXT_DES, 'Extended-DES is not supported.');

		$saltPattern = "{^_[0-9A-Za-z./]{8}$}";
		$hashPattern = "{^_[0-9A-Za-z./]{19}$}";

		$log2 = 18;
		$salt = Password::salt('xdes', $log2);
		$this->assertPattern($saltPattern, $salt);
		$this->assertNotEqual($salt, Password::salt('xdes', $log2));

		$hash = Password::hash($this->_password, $salt);
		$hash2 = Password::hash($this->_password, Password::salt('xdes', $log2));
		$this->assertPattern($hashPattern, $hash);
		$this->assertNotEqual($hash, $hash2);
	}

	/**
	 * Tests salting passwords with the MD5 algorithm.
	 */
	public function testSaltMD5() {
		$this->skipIf(!CRYPT_MD5, 'MD5 is not supported.');

		$saltPattern = "{^\\$1\\$[0-9A-Za-z./]{8}$}";
		$hashPattern = "{^\\$1\\$[0-9A-Za-z./]{8}\\$[0-9A-Za-z./]{22}$}";

		$salt = Password::salt('md5', null);
		$this->assertPattern($saltPattern, $salt);
		$this->assertNotEqual($salt, Password::salt('md5', null));

		$hash = Password::hash($this->_password, $salt);
		$hash2 = Password::hash($this->_password, Password::salt('md5', null));
		$this->assertPattern($hashPattern, $hash);
		$this->assertNotEqual($hash, $hash2);
	}
}

?>