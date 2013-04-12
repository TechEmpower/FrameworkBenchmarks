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
use lithium\util\Validator;

class CatalogValidatorTest extends \lithium\test\Integration {

	protected $_backup = array();

	public function setUp() {
		$this->_backup['catalogConfig'] = Catalog::config();
		Catalog::reset();
		Catalog::config(array(
			'runtime' => array('adapter' => new Memory())
		));
		Validator::__init();
	}

	public function tearDown() {
		Catalog::reset();
		Catalog::config($this->_backup['catalogConfig']);
	}

	public function testFlat() {
		$data = '/postalCode en_US/';
		Catalog::write('runtime', 'validation.postalCode', 'en_US', $data);

		Validator::add('postalCode', Catalog::read('runtime', 'validation.postalCode', 'en_US'));

		$result = Validator::isPostalCode('postalCode en_US');
		$this->assertTrue($result);
	}

	public function testMultipleRules() {
		$data = array(
			'postalCode' => '/postalCode en_US/',
			'phone' => '/phone en_US/'
		);
		Catalog::write('runtime', 'validation', 'en_US', $data);

		Validator::add(Catalog::read('runtime', 'validation', 'en_US'));

		$result = Validator::isPostalCode('postalCode en_US');
		$this->assertTrue($result);

		$result = Validator::isPhone('phone en_US');
		$this->assertTrue($result);
	}

	public function testMultipleLocales() {
		$data = '/phone en_US/';
		Catalog::write('runtime', 'validation.phone', 'en_US', $data);
		$data = '/phone en_GB/';
		Catalog::write('runtime', 'validation.phone', 'en_GB', $data);

		Validator::add('phone', array(
			'en_US' => Catalog::read('runtime', 'validation.phone', 'en_US'),
			'en_GB' => Catalog::read('runtime', 'validation.phone', 'en_GB')
		));

		$result = Validator::isPhone('phone en_US', 'en_US');
		$this->assertTrue($result);

		$result = Validator::isPhone('phone en_GB', 'en_GB');
		$this->assertTrue($result);
	}
}

?>