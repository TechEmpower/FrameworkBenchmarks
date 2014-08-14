<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\integration\g11n;

use lithium\g11n\Catalog;
use lithium\g11n\catalog\adapter\Memory;
use lithium\util\Inflector;

class CatalogInflectorTest extends \lithium\test\Integration {

	protected $_backup = array();

	public function setUp() {
		$this->_backup['catalogConfig'] = Catalog::config();
		Catalog::reset();
		Catalog::config(array(
			'runtime' => array('adapter' => new Memory())
		));
	}

	public function tearDown() {
		Inflector::reset();
		Catalog::reset();
		Catalog::config($this->_backup['catalogConfig']);
	}

	public function testTransliteration() {
		$data = array(
			'transliteration' => array(
				'\$' => 'dollar',
				'&' => 'and'
			)
		);
		Catalog::write('runtime', 'inflection', 'en', $data);

		Inflector::rules(
			'transliteration', Catalog::read('runtime', 'inflection.transliteration', 'en')
		);

		$result = Inflector::slug('this & that');
		$expected = 'this-and-that';
		$this->assertEqual($expected, $result);

		$data = array(
			'transliteration' => array(
				't' => 'd',
				'&' => 'und'
			)
		);
		Catalog::write('runtime', 'inflection', 'de', $data);

		Inflector::rules(
			'transliteration', Catalog::read('runtime', 'inflection.transliteration', 'de')
		);

		$result = Inflector::slug('this & that');
		$expected = 'dhis-und-dhad';
		$this->assertEqual($expected, $result);
	}
}

?>