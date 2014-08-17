<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\storage\session\strategy;

use lithium\storage\session\strategy\Hmac;
use lithium\tests\mocks\storage\session\strategy\MockCookieSession;


class HmacTest extends \lithium\test\Unit {

	public function setUp() {
		$this->secret = 'foobar';
		$this->Hmac = new Hmac(array('secret' => $this->secret));
		$this->mock = 'lithium\tests\mocks\storage\session\strategy\MockCookieSession';
		MockCookieSession::reset();
	}

	public function testConstructException() {
		$this->expectException('/HMAC strategy requires a secret key./');
		$hmac = new Hmac();
	}

	public function testConstruct() {
		$secret = 'foo';
		$hmac = new Hmac(compact('secret'));
		$this->assertTrue($hmac instanceof Hmac);
	}

	public function testWrite() {
		$value = 'value';
		$key = 'new_key';
		$oldData = MockCookieSession::data();
		$class = $this->mock;

		$result = $this->Hmac->write($value, compact('key', 'class'));
		$this->assertEqual($value, $result);

		$signature = hash_hmac('sha1', serialize(array($key => $value) + $oldData), $this->secret);
		$signedData = MockCookieSession::data();
		$this->assertEqual($signedData, $oldData + array('__signature' => $signature));
	}

	public function testReadWithValidSignature() {
		$class = $this->mock;
		$currentData = MockCookieSession::data();
		$signature = hash_hmac('sha1', serialize($currentData), $this->secret);
		$result = MockCookieSession::write('__signature', $signature);
		$this->assertEqual($signature, $result);

		$value = 'data_read';
		$result = $this->Hmac->read($value, compact('class'));
		$this->assertEqual($value, $result);
	}

	public function testReadWithNoSignature() {
		$class = $this->mock;
		$value = 'data_read';
		$this->expectException('/HMAC signature not found./');
		$result = $this->Hmac->read($value, compact('class'));
	}

	public function testReadWithInvalidSignature() {
		$class = $this->mock;
		$currentData = MockCookieSession::data();
		$signature = 'some_invalid_signature';
		$result = MockCookieSession::write('__signature', $signature);
		$this->assertEqual($signature, $result);

		$value = 'data_read_that_wont_match_signature';
		$this->expectException('/Possible data tampering: HMAC signature does not match data./');
		$result = $this->Hmac->read($value, compact('class'));
	}

	public function testDelete() {
		$key = 'one';
		$class = $this->mock;
		$oldData = MockCookieSession::data();
		$currentSignature = hash_hmac('sha1', serialize($oldData), $this->secret);
		$result = MockCookieSession::write('__signature', $currentSignature);

		$newData = $oldData;
		unset($newData[$key]);

		$expectedSignature = hash_hmac('sha1', serialize($newData), $this->secret);
		$result = $this->Hmac->delete('foo', compact('class', 'key'));

		$this->assertEqual('foo', $result);
		$signature = MockCookieSession::read('__signature');
		$this->assertEqual($expectedSignature, $signature);
	}
}

?>