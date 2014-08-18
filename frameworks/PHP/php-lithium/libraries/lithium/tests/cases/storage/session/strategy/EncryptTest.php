<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\storage\session\strategy;

use lithium\storage\session\strategy\Encrypt;
use lithium\tests\mocks\storage\session\strategy\MockCookieSession;

class EncryptTest extends \lithium\test\Unit {

	public $secret = 'foobar';

	/**
	 * Skip the test if the mcrypt extension is unavailable.
	 */
	public function skip() {
		$this->skipIf(!Encrypt::enabled(), 'The Mcrypt extension is not installed or enabled.');
	}

	public function setUp() {
		$this->mock = 'lithium\tests\mocks\storage\session\strategy\MockCookieSession';
		MockCookieSession::reset();
	}

	public function testConstructException() {
		$this->expectException('/Encrypt strategy requires a secret key./');
		$encrypt = new Encrypt();
	}

	public function testEnabled() {
		$this->assertTrue(Encrypt::enabled());
	}

	public function testConstruct() {
		$encrypt = new Encrypt(array('secret' => $this->secret));
		$this->assertTrue($encrypt instanceof Encrypt);
	}

	public function testWrite() {
		$encrypt = new Encrypt(array('secret' => $this->secret));

		$key = 'fookey';
		$value = 'barvalue';

		$result = $encrypt->write($value, array('class' => $this->mock, 'key' => $key));
		$cookie = MockCookieSession::data();

		$this->assertTrue($result);
		$this->assertTrue($cookie['__encrypted']);
		$this->assertTrue(is_string($cookie['__encrypted']));
		$this->assertNotEqual($cookie['__encrypted'], $value);
	}

	public function testRead() {
		$encrypt = new Encrypt(array('secret' => $this->secret));

		$key = 'fookey';
		$value = 'barvalue';

		$result = $encrypt->write($value, array('class' => $this->mock, 'key' => $key));
		$this->assertTrue($result);

		$cookie = MockCookieSession::data();
		$result = $encrypt->read($key, array('class' => $this->mock, 'key' => $key));

		$this->assertEqual($value, $result);
		$this->assertNotEqual($cookie['__encrypted'], $result);
	}

	public function testDelete() {
		$encrypt = new Encrypt(array('secret' => $this->secret));

		$key = 'fookey';
		$value = 'barvalue';

		$result = $encrypt->write($value, array('class' => $this->mock, 'key' => $key));
		$this->assertTrue($result);

		$cookie = MockCookieSession::data();
		$result = $encrypt->read($key, array('class' => $this->mock, 'key' => $key));

		$this->assertEqual($value, $result);

		$result = $encrypt->delete($key, array('class' => $this->mock, 'key' => $key));

		$cookie = MockCookieSession::data();
		$this->assertTrue(empty($cookie['__encrypted']));

		$result = $encrypt->read($key, array('class' => $this->mock));
		$this->assertFalse($result);
	}
}

?>