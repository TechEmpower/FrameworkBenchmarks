<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\g11n;

use lithium\g11n\Catalog;
use lithium\g11n\catalog\adapter\Memory;

class CatalogTest extends \lithium\test\Unit {

	protected $_backup = array();

	public function setUp() {
		$this->_backup['catalogConfig'] = Catalog::config();
		Catalog::reset();
		Catalog::config(array(
			'runtime' => array('adapter' => new Memory())
		));
	}

	public function tearDown() {
		Catalog::reset();
		Catalog::config($this->_backup['catalogConfig']);
	}

	/**
	 * Tests for values returned by `read()`.
	 *
	 * @return void
	 */
	public function testRead() {
		$result = Catalog::read('runtime', 'validation.ssn', 'de_DE');
		$this->assertNull($result);
	}

	/**
	 * Tests for values returned by `write()`.
	 *
	 * @return void
	 */
	public function testWrite() {
		$data = array(
			'DKK' => 'Dänische Krone'
		);
		$result = Catalog::write('runtime', 'currency', 'de_DE', $data);
		$this->assertTrue($result);
	}

	/**
	 * Tests writing and reading for single and multiple items.
	 *
	 * @return void
	 */
	public function testWriteRead() {
		$data = '/postalCode en_US/';
		Catalog::write('runtime', 'validation.postalCode', 'en_US', $data);
		$result = Catalog::read('runtime', 'validation.postalCode', 'en_US');
		$this->assertEqual($data, $result);

		$this->tearDown();
		$this->setUp();

		$data = array(
			'GRD' => 'Griechische Drachme',
			'DKK' => 'Dänische Krone'
		);
		Catalog::write('runtime', 'currency', 'de', $data);
		$result = Catalog::read('runtime', 'currency', 'de');
		$this->assertEqual($data, $result);
	}

	/**
	 * Tests writing and reading with data merged between cascaded locales.
	 *
	 * Only complete items are merged in, (atomic) merging between items
	 * should not occur. Categories fall back to results for more generic locales.
	 *
	 * @return void
	 */
	public function testWriteReadMergeLocales() {
		$data = '/postalCode en/';
		Catalog::write('runtime', 'validation.postalCode', 'en', $data);
		$result = Catalog::read('runtime', 'validation.postalCode', 'en_US');
		$expected = '/postalCode en/';
		$this->assertEqual($expected, $result);

		$this->tearDown();
		$this->setUp();

		$data = '/postalCode en_US/';
		Catalog::write('runtime', 'validation.postalCode', 'en_US', $data);
		$data = '/postalCode en/';
		Catalog::write('runtime', 'validation.postalCode', 'en', $data);
		$result = Catalog::read('runtime', 'validation.postalCode', 'en_US');
		$expected = '/postalCode en_US/';
		$this->assertEqual($expected, $result);

		$this->tearDown();
		$this->setUp();

		$data = array('a' => true, 'b' => true, 'c' => true);
		Catalog::write('runtime', 'language', 'en', $data);
		$result = Catalog::read('runtime', 'language', 'en_US');
		$expected = array('a' => true, 'b' => true, 'c' => true);
		$this->assertEqual($expected, $result);

		$this->tearDown();
		$this->setUp();

		$data = array(
			'DKK' => 'Dänische Krone'
		);
		Catalog::write('runtime', 'currency', 'de', $data);
		$data = array(
			'GRD' => 'Griechische Drachme'
		);
		Catalog::write('runtime', 'currency', 'de_CH', $data);
		$result = Catalog::read('runtime', 'currency', 'de_CH');
		$expected = array(
			'GRD' => 'Griechische Drachme',
			'DKK' => 'Dänische Krone'
		);
		$this->assertEqual($expected, $result);

		$this->tearDown();
		$this->setUp();

		$data = array(
			'GRD' => 'de Griechische Drachme',
			'DKK' => 'de Dänische Krone'
		);
		Catalog::write('runtime', 'currency', 'de', $data);
		$data = array(
			'GRD' => 'de_CH Griechische Drachme'
		);
		Catalog::write('runtime', 'currency', 'de_CH', $data);
		$result = Catalog::read('runtime', 'currency', 'de_CH');
		$expected = array(
			'GRD' => 'de_CH Griechische Drachme',
			'DKK' => 'de Dänische Krone'
		);
		$this->assertEqual($expected, $result);
	}

	/**
	 * Tests that a scope is honored if one is used.
	 *
	 * @return void
	 */
	public function testWriteReadWithScope() {
		$data = '/postalCode en_US scope0/';
		Catalog::write('runtime', 'validation.postalCode', 'en_US', $data, array(
			'scope' => 'scope0'
		));
		$data = '/postalCode en_US scope1/';
		Catalog::write('runtime', 'validation.postalCode', 'en_US', $data, array(
			'scope' => 'scope1'
		));

		$result = Catalog::read('runtime', 'validation.postalCode', 'en_US');
		$this->assertNull($result);

		$result = Catalog::read('runtime', 'validation.postalCode', 'en_US', array(
			'scope' => 'scope0'
		));
		$expected = '/postalCode en_US scope0/';
		$this->assertEqual($expected, $result);

		$result = Catalog::read('runtime', 'validation.postalCode', 'en_US', array(
			'scope' => 'scope1'
		));
		$expected = '/postalCode en_US scope1/';
		$this->assertEqual($expected, $result);

		$data = '/postalCode en_US/';
		Catalog::write('runtime', 'validation.postalCode', 'en_US', $data);

		$result = Catalog::read('runtime', 'validation.postalCode', 'en_US');
		$expected = '/postalCode en_US/';
		$this->assertEqual($expected, $result);
	}

	/**
	 * Tests reading from all configured stores with fallbacks.
	 *
	 * @return void
	 */
	public function testWriteReadMergeAllConfigurations() {
		Catalog::reset();
		Catalog::config(array(
			'runtime0' => array('adapter' => new Memory()),
			'runtime1' => array('adapter' => new Memory())
		));

		$data = '/postalCode en0/';
		Catalog::write('runtime0', 'validation.postalCode', 'en', $data);
		$data = '/postalCode en_US1/';
		Catalog::write('runtime1', 'validation.postalCode', 'en_US', $data);
		$data = '/postalCode en1/';
		Catalog::write('runtime1', 'validation.postalCode', 'en', $data);
		$result = Catalog::read(true, 'validation.postalCode', 'en_US');
		$expected = '/postalCode en_US1/';
		$this->assertEqual($expected, $result);

		Catalog::reset();
		Catalog::config(array(
			'runtime0' => array('adapter' => new Memory()),
			'runtime1' => array('adapter' => new Memory())
		));

		$data = array(
			'GRD' => 'de0 Griechische Drachme',
			'DKK' => 'de0 Dänische Krone'
		);
		Catalog::write('runtime0', 'currency', 'de', $data);
		$data = array(
			'GRD' => 'de1 Griechische Drachme'
		);
		Catalog::write('runtime1', 'currency', 'de', $data);
		$data = array(
			'GRD' => 'de_CH1 Griechische Drachme'
		);
		Catalog::write('runtime1', 'currency', 'de_CH', $data);
		$result = Catalog::read(true, 'currency', 'de_CH');
		$expected = array(
			'GRD' => 'de_CH1 Griechische Drachme',
			'DKK' => 'de0 Dänische Krone'
		);
		$this->assertEqual($expected, $result);
	}

	/**
	 * Tests reading from selected multiple configured stores.
	 *
	 * @return void
	 */
	public function testReadMergeSelectedConfigurations() {
		Catalog::reset();
		Catalog::config(array(
			'runtime0' => array('adapter' => new Memory()),
			'runtime1' => array('adapter' => new Memory()),
			'runtime2' => array('adapter' => new Memory())
		));

		$data = '/postalCode en0/';
		Catalog::write('runtime0', 'validation.postalCode', 'en', $data);
		$data = '/postalCode en1/';
		Catalog::write('runtime1', 'validation.postalCode', 'en', $data);
		$data = '/postalCode en2/';
		Catalog::write('runtime2', 'validation.postalCode', 'en', $data);
		$data = '/ssn en2/';
		Catalog::write('runtime2', 'validation.ssn', 'en', $data);

		$result = Catalog::read('runtime0', 'validation.postalCode', 'en');
		$expected = '/postalCode en0/';
		$this->assertEqual($expected, $result);

		$result = Catalog::read('runtime2', 'validation.postalCode', 'en');
		$expected = '/postalCode en2/';
		$this->assertEqual($expected, $result);

		$result = Catalog::read('runtime2', 'validation.postalCode', 'en');
		$expected = '/postalCode en2/';
		$this->assertEqual($expected, $result);

		$result = Catalog::read(array('runtime0', 'runtime2'), 'validation', 'en');
		$expected = array(
			'postalCode' => '/postalCode en0/',
			'ssn' => '/ssn en2/'
		);
		$this->assertEqual($expected, $result);

		$resultA = Catalog::read(array('runtime0', 'runtime2'), 'validation', 'en');
		$resultB = Catalog::read(true, 'validation', 'en');
		$this->assertEqual($resultA, $resultB);
	}

	/**
	 * Tests writing, then reading different types of values.
	 *
	 * @return void
	 */
	public function testDataTypeSupport() {
		$data = function($n) { return $n === 1 ? 0 : 1; };
		Catalog::write('runtime', 'message.pluralRule', 'en', $data);
		$result = Catalog::read('runtime', 'message.pluralRule', 'en');
		$this->assertEqual($data, $result);

		$data = array('fish', 'fishes');
		Catalog::write('runtime', 'message.fish', 'en', $data);
		$result = Catalog::read('runtime', 'message.fish', 'en');
		$this->assertEqual($data, $result);
	}

	/**
	 * Tests if the output is normalized and doesn't depend on the input format.
	 *
	 * @return void
	 */
	public function testInputFormatNormalization() {
		$data = array('house' => 'Haus');
		Catalog::write('runtime', 'message', 'de', $data);
		$result = Catalog::read('runtime', 'message', 'de', array('lossy' => false));
		$expected = array('house' => array(
			'id' => 'house',
			'ids' => array(),
			'translated' => 'Haus',
			'flags' => array(),
			'comments' => array(),
			'occurrences' => array()
		));
		$this->assertEqual($expected, $result);

		$data = array('house' => array(
			'id' => 'house',
			'ids' => array(),
			'translated' => 'Haus',
			'flags' => array(),
			'comments' => array(),
			'occurrences' => array()
		));
		Catalog::write('runtime', 'message', 'de', $data);
		$result = Catalog::read('runtime', 'message', 'de', array('lossy' => false));
		$expected = array('house' => array(
			'id' => 'house',
			'ids' => array(),
			'translated' => 'Haus',
			'flags' => array(),
			'comments' => array(),
			'occurrences' => array()
		));
		$this->assertEqual($expected, $result);
	}

	public function testOutputLossyFormat() {
		$data = array('house' => array(
			'id' => 'house',
			'ids' => array('singular' => 'house'),
			'translated' => 'Haus',
			'flags' => array(),
			'comments' => array(),
			'occurrences' => array()
		));
		Catalog::write('runtime', 'message', 'de', $data);
		$result = Catalog::read('runtime', 'message', 'de');
		$expected = array('house' => 'Haus');
		$this->assertEqual($expected, $result);
	}

	public function testOutputLosslessFormat() {
		$data = array('house' => array(
			'id' => 'house',
			'ids' => array('singular' => 'house'),
			'translated' => 'Haus',
			'flags' => array(),
			'comments' => array(),
			'occurrences' => array()
		));
		Catalog::write('runtime', 'message', 'de', $data);
		$result = Catalog::read('runtime', 'message', 'de', array('lossy' => false));
		$expected = array('house' => array(
			'id' => 'house',
			'ids' => array('singular' => 'house'),
			'translated' => 'Haus',
			'flags' => array(),
			'comments' => array(),
			'occurrences' => array()
		));
		$this->assertEqual($expected, $result);
	}

	public function testInvalidWrite() {
		Catalog::reset();
		$data = array('house' => array('id' => 'house'));
		$this->expectException("Configuration `runtime` has not been defined.");
		$this->assertFalse(Catalog::write('runtime', 'message', 'de', $data));
	}
}

?>