<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\template\view\adapter;

use lithium\template\view\adapter\Simple;
use lithium\tests\mocks\util\MockStringObject;

class SimpleTest extends \lithium\test\Unit {

	public $subject = null;

	public function setUp() {
		$this->subject = new Simple();
	}

	public function testBasicRender() {
		$result = $this->subject->template('layout', array('layout' => '{:content}'));
		$expected = '{:content}';
		$this->assertEqual($expected, $result);

		$message = new MockStringObject();
		$message->message = 'Lithium is about to rock you.';

		$result = $this->subject->render('Hello {:name}! {:message}', compact('message') + array(
			'name' => 'World'
		));
		$expected = 'Hello World! Lithium is about to rock you.';
		$this->assertEqual($expected, $result);
	}
}

?>