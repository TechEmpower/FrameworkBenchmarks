<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2009, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\security;

use lithium\security\Auth;
use lithium\storage\Session;

class AuthTest extends \lithium\test\Unit {

	protected $_classes = array(
		'mockAuthAdapter' => 'lithium\tests\mocks\security\auth\adapter\MockAuthAdapter'
	);

	public function setUp() {
		Session::config(array(
			'test' => array('adapter' => 'Memory')
		));

		Auth::config(array(
			'test' => array(
				'adapter' => $this->_classes['mockAuthAdapter']
			)
		));
	}

	public function testBasicAuthCheck() {
		$this->assertFalse(Auth::check('test'));
		$user = array('user' => 'bob');

		$result = Auth::check('test', $user, array('success' => true));
		$this->assertEqual($user, $result);

		$result = Session::read('test');
		$this->assertEqual($user, $result);

		$result = Auth::check('test');
		$this->assertEqual($user, $result);
	}

	public function testAuthLogout() {
		$user = array('user' => 'bob');

		$result = Auth::check('test', $user, array('success' => true));
		$this->assertEqual($user, $result);

		$result = Auth::check('test');
		$this->assertEqual($user, $result);

		Auth::clear('test');
		$this->assertFalse(Auth::check('test'));
	}

	public function testManualSessionInitialization() {
		$this->assertFalse(Auth::check('test'));
		$user = array('id' => 13, 'user' => 'bob');

		$this->assertTrue(Auth::set('test', $user));

		$result = Auth::check('test');
		$this->assertEqual($user, $result);
	}

	public function testManualSessionFail() {
		$this->assertFalse(Auth::check('test'));
		$user = array('id' => 13, 'user' => 'bob');

		$this->assertFalse(Auth::set('test', $user, array('fail' => true)));
		$this->assertFalse(Auth::check('test'));
	}

	public function testNoConfigurations() {
		Auth::reset();
		$this->assertIdentical(array(), Auth::config());
		$this->expectException("Configuration `user` has not been defined.");
		Auth::check('user');
	}

	public function testAuthPersist() {
		Auth::reset();

		Auth::config(array(
			'test' => array(
				'adapter' => $this->_classes['mockAuthAdapter'],
			)
		));

		$config = Auth::config();
		$this->assertTrue(isset($config['test']['session']['persist']));
		$this->assertTrue(empty($config['test']['session']['persist']));

		$user = array('username' => 'foo', 'password' => 'bar');
		$result = Auth::check('test', $user, array('success' => true));
		$this->assertTrue(isset($result['username']));
		$this->assertFalse(isset($result['password']));

		Auth::reset();

		Auth::config(array(
			'test' => array(
				'adapter' => $this->_classes['mockAuthAdapter'],
				'session' => array(
					'persist' => array('username', 'email')
				)
			)
		));

		$user = array(
			'username' => 'foobar',
			'password' => 'not!important',
			'email' => 'foo@bar.com',
			'insuranceNumer' => 1234567
		);

		$expected = array(
			'username' => 'foobar',
			'email' => 'foo@bar.com'
		);

		$result = Auth::check('test', $user, array('success' => true, 'checkSession' => false));
		$this->assertEqual($expected, $result);
		$this->assertEqual($expected, Session::read('test'));

		Auth::reset();

		Auth::config(array(
			'test' => array(
				'adapter' => $this->_classes['mockAuthAdapter'],
			)
		));

		$user = array(
			'id' => '123',
			'username' => 'foobar',
			'password' => 'not!important',
			'email' => 'foo@bar.com',
			'insuranceNumer' => 1234567
		);

		$expected = 123;

		$result = Auth::check('test', $user, array('keyOnly' => true, 'checkSession' => false));
		$this->assertEqual($expected, $result);
		$this->assertEqual($expected, Session::read('test'));
	}
}

?>