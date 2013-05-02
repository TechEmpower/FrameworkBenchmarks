<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\g11n\catalog\adapter;

use Exception;
use lithium\core\Libraries;
use lithium\g11n\catalog\adapter\Php;

class PhpTest extends \lithium\test\Unit {

	public $adapter;

	protected $_path;

	public function skip() {
		$this->_path = $path = Libraries::get(true, 'resources') . '/tmp/tests';
		$this->skipIf(!is_writable($path), "Path `{$path}` is not writable.");
	}

	public function setUp() {
		$this->adapter = new Php(array('path' => $this->_path));
	}

	public function tearDown() {
		$this->_cleanUp();
	}

	public function testPathMustExist() {
		try {
			new Php(array('path' => $this->_path));
			$result = true;
		} catch (Exception $e) {
			$result = false;
		}
		$this->assert($result);

		try {
			new Php(array('path' => "{$this->_path}/i_do_not_exist"));
			$result = false;
		} catch (Exception $e) {
			$result = true;
		}
		$this->assert($result);
	}

	public function testRead() {
		mkdir("{$this->_path}/fr/message", 0755, true);

		$data = <<<EOD
<?php
return array(
	'politics' => 'politique',
	'house' => array('maison', 'maisons')
);
?>
EOD;
		file_put_contents("{$this->_path}/fr/message/default.php", $data);

		$result = $this->adapter->read('message', 'fr', null);
		$expected = array(
			'politics' => array(
				'id' => 'politics',
				'ids' => array(),
				'translated' => 'politique',
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
			),
			'house' => array(
				'id' => 'house',
				'ids' => array(),
				'translated' => array('maison', 'maisons'),
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
		));
		$this->assertEqual($expected, $result);
	}

	public function testReadTemplate() {
		$data = <<<EOD
<?php
return array(
	'politics' => 'politique',
	'house' => array('maison', 'maisons')
);
?>
EOD;
		file_put_contents("{$this->_path}/message_default.php", $data);

		$result = $this->adapter->read('messageTemplate', 'root', null);
		$expected = array(
			'politics' => array(
				'id' => 'politics',
				'ids' => array(),
				'translated' => 'politique',
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
			),
			'house' => array(
				'id' => 'house',
				'ids' => array(),
				'translated' => array('maison', 'maisons'),
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
		));
		$this->assertEqual($expected, $result);
	}

	public function testReadWithScope() {
		mkdir("{$this->_path}/fr/message", 0755, true);

		$data = <<<EOD
<?php
return array(
	'politics' => 'politique'
);
?>
EOD;
		file_put_contents("{$this->_path}/fr/message/li3_docs.php", $data);

		$result = $this->adapter->read('message', 'fr', null);
		$this->assertFalse($result);

		$result = $this->adapter->read('message', 'fr', 'li3_docs');
		$expected = array(
			'politics' => array(
				'id' => 'politics',
				'ids' => array(),
				'translated' => 'politique',
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
		));
		$this->assertEqual($expected, $result);
	}

	public function testReadValidation() {
		mkdir("{$this->_path}/fr/validation", 0755, true);

		$data = <<<EOD
<?php
return array(
	'phone' => '/[0-9].*/i'
);
?>
EOD;
		file_put_contents("{$this->_path}/fr/validation/default.php", $data);

		$result = $this->adapter->read('validation', 'fr', null);
		$expected = array(
			'phone' => array(
				'id' => 'phone',
				'ids' => array(),
				'translated' => '/[0-9].*/i',
				'flags' => array(),
				'comments' => array(),
				'occurrences' => array()
		));
		$this->assertEqual($expected, $result);

	}

	public function testReadWithAnonymousFunction() {
		mkdir("{$this->_path}/fr/message", 0755, true);

		$data = <<<EOD
<?php
return array(
	'plural' => function() { return 123; },
	'politics' => 'politique',
);
?>
EOD;
		file_put_contents("{$this->_path}/fr/message/default.php", $data);

		$result = $this->adapter->read('message', 'fr', null);
		$expected = array(
			'id' => 'politics',
			'ids' => array(),
			'translated' => 'politique',
			'flags' => array(),
			'comments' => array(),
			'occurrences' => array()
		);
		$this->assertEqual($expected, $result['politics']);

		$this->assertTrue(is_callable($result['plural']['translated']));

		$expected = 123;
		$result = $result['plural']['translated']();
		$this->assertEqual($expected, $result);
	}
}

?>