<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\g11n;

use lithium\core\Environment;
use lithium\g11n\Message;
use lithium\g11n\Catalog;
use lithium\g11n\catalog\adapter\Memory;

class MessageTest extends \lithium\test\Unit {

	protected $_backup = array();

	public function setUp() {
		$this->_backup['catalogConfig'] = Catalog::config();
		Catalog::reset();
		Catalog::config(array(
			'runtime' => array('adapter' => new Memory())
		));
		$data = function($n) { return $n === 1 ? 0 : 1; };
		Catalog::write('runtime', 'message.pluralRule', 'root', $data);

		$this->_backup['environment'] = Environment::get('test');
		Environment::set('test', array('locale' => 'en'));
		Environment::set('test');
		Message::cache(false);
	}

	public function tearDown() {
		Catalog::reset();
		Catalog::config($this->_backup['catalogConfig']);

		Environment::set('test', $this->_backup['environment']);
	}

	public function testTranslateBasic() {
		$data = array('catalog' => 'Katalog');
		Catalog::write('runtime', 'message', 'de', $data);

		$expected = 'Katalog';
		$result = Message::translate('catalog', array('locale' => 'de'));
		$this->assertEqual($expected, $result);
	}

	public function testTranslatePlural() {
		$data = array(
			'house' => array('Haus', 'Häuser')
		);
		Catalog::write('runtime', 'message', 'de', $data);

		$expected = 'Haus';
		$result = Message::translate('house', array('locale' => 'de'));
		$this->assertEqual($expected, $result);

		$expected = 'Häuser';
		$result = Message::translate('house', array('locale' => 'de', 'count' => 5));
		$this->assertEqual($expected, $result);
	}

	public function testTranslateNonIntegerCounts() {
		$data = array(
			'house' => array('Haus', 'Häuser')
		);
		Catalog::write('runtime', 'message', 'de', $data);

		$expected = 'Häuser';
		$result = Message::translate('house', array('locale' => 'de', 'count' => 2.31));
		$this->assertEqual($expected, $result);

		$expected = 'Häuser';
		$result = Message::translate('house', array('locale' => 'de', 'count' => 1.1));
		$this->assertEqual($expected, $result);

		$expected = 'Häuser';
		$result = Message::translate('house', array('locale' => 'de', 'count' => 0.1));
		$this->assertEqual($expected, $result);

		$expected = 'Haus';
		$result = Message::translate('house', array('locale' => 'de', 'count' => true));
		$this->assertEqual($expected, $result);

		$expected = 'Häuser';
		$result = Message::translate('house', array('locale' => 'de', 'count' => false));
		$this->assertEqual($expected, $result);

		$expected = 'Häuser';
		$result = Message::translate('house', array('locale' => 'de', 'count' => '2'));
		$this->assertEqual($expected, $result);

		$expected = 'Häuser';
		$result = Message::translate('house', array('locale' => 'de', 'count' => '0'));
		$this->assertEqual($expected, $result);
	}

	public function testTranslateNegativeIntegerCounts() {
		$data = array(
			'house' => array('Haus', 'Häuser')
		);
		Catalog::write('runtime', 'message', 'de', $data);

		$expected = 'Haus';
		$result = Message::translate('house', array('locale' => 'de', 'count' => -1));
		$this->assertEqual($expected, $result);

		$expected = 'Häuser';
		$result = Message::translate('house', array('locale' => 'de', 'count' => -2));
		$this->assertEqual($expected, $result);

		$expected = 'Häuser';
		$result = Message::translate('house', array('locale' => 'de', 'count' => -5));
		$this->assertEqual($expected, $result);
	}

	public function testTranslateFail() {
		$result = Message::translate('catalog', array('locale' => 'de'));
		$this->assertNull($result);

		Catalog::reset();
		Catalog::config(array(
			'runtime' => array('adapter' => new Memory())
		));

		$data = array(
			'catalog' => array('Katalog', 'Kataloge')
		);
		Catalog::write('runtime', 'message', 'de', $data);

		$result = Message::translate('catalog', array('locale' => 'de'));
		$this->assertNull($result);

		$data = 'not a valid pluralization function';
		Catalog::write('runtime', 'message.pluralRule', 'root', $data);

		$result = Message::translate('catalog', array('locale' => 'de'));
		$this->assertNull($result);
	}

	public function testTranslateScope() {
		$data = array(
			'catalog' => 'Katalog'
		);
		Catalog::write('runtime', 'message', 'de', $data, array('scope' => 'test'));

		$data = function($n) { return $n === 1 ? 0 : 1; };
		Catalog::write('runtime', 'message.pluralRule', 'root', $data, array(
			'scope' => 'test'
		));

		$result = Message::translate('catalog', array('locale' => 'de'));
		$this->assertNull($result);

		$expected = 'Katalog';
		$result = Message::translate('catalog', array('locale' => 'de', 'scope' => 'test'));
		$this->assertEqual($expected, $result);
	}

	public function testTranslateDefault() {
		$result = Message::translate('Here I am', array('locale' => 'de'));
		$this->assertNull($result);

		$result = Message::translate('Here I am', array(
			'locale' => 'de', 'default' => 'Here I am'
		));
		$expected = 'Here I am';
		$this->assertEqual($expected, $result);
	}

	public function testTranslatePlaceholders() {
		$data = array(
			'green' => 'grün',
			'No. {:id}' => 'Nr. {:id}',
			'The fish is {:color}.' => 'Der Fisch ist {:color}.',
			'{:count} bike' => array('{:count} Fahrrad', '{:count} Fahrräder')
		);
		Catalog::write('runtime', 'message', 'de', $data);

		$expected = 'Der Fisch ist grün.';
		$result = Message::translate('The fish is {:color}.', array(
			'locale' => 'de',
			'color' => Message::translate('green', array('locale' => 'de'))
		));
		$this->assertEqual($expected, $result);

		$expected = '1 Fahrrad';
		$result = Message::translate('{:count} bike', array('locale' => 'de', 'count' => 1));
		$this->assertEqual($expected, $result);

		$expected = '7 Fahrräder';
		$result = Message::translate('{:count} bike', array('locale' => 'de', 'count' => 7));
		$this->assertEqual($expected, $result);

		$expected = 'Nr. 8';
		$result = Message::translate('No. {:id}', array('locale' => 'de', 'id' => 8));
		$this->assertEqual($expected, $result);
	}

	public function testTranslateLocales() {
		$data = array(
			'catalog' => 'Katalog'
		);
		Catalog::write('runtime', 'message', 'de', $data);
		$data = array(
			'catalog' => 'catalogue'
		);
		Catalog::write('runtime', 'message', 'fr', $data);

		$expected = 'Katalog';
		$result = Message::translate('catalog', array('locale' => 'de'));
		$this->assertEqual($expected, $result);

		$expected = 'catalogue';
		$result = Message::translate('catalog', array('locale' => 'fr'));
		$this->assertEqual($expected, $result);
	}

	public function testTranslateNoop() {
		$data = array(
			'catalog' => 'Katalog'
		);
		Catalog::write('runtime', 'message', 'de', $data);

		$result = Message::translate('catalog', array('locale' => 'de', 'noop' => true));
		$this->assertNull($result);
	}

	public function testAliasesBasic() {
		$data = array(
			'house' => array('Haus', 'Häuser')
		);
		Catalog::write('runtime', 'message', 'de', $data);

		$filters = Message::aliases();
		$t = $filters['t'];
		$tn = $filters['tn'];

		$expected = 'Haus';
		$result = $t('house', array('locale' => 'de'));
		$this->assertEqual($expected, $result);

		$expected = 'Haus';
		$result = $tn('house', 'houses', 1, array('locale' => 'de'));
		$this->assertEqual($expected, $result);

		$expected = 'Häuser';
		$result = $tn('house', 'houses', 3, array('locale' => 'de'));
		$this->assertEqual($expected, $result);
	}

	public function testAliasesSymmetry() {
		$data = array('house' => array('Haus', 'Häuser'));
		Catalog::write('runtime', 'message', 'de', $data);

		$filters = Message::aliases();
		$t = $filters['t'];
		$tn = $filters['tn'];

		$expected = Message::translate('house', array('locale' => 'de'));
		$result = $t('house', array('locale' => 'de'));
		$this->assertEqual($expected, $result);

		$expected = Message::translate('house', array('locale' => 'de', 'count' => 1));
		$result = $tn('house', 'houses', 1, array('locale' => 'de'));
		$this->assertEqual($expected, $result);

		$expected = Message::translate('house', array('locale' => 'de', 'count' => 3));
		$result = $tn('house', 'houses', 3, array('locale' => 'de'));
		$this->assertEqual($expected, $result);
	}

	public function testAliasesAsymmetry() {
		$filters = Message::aliases();
		$t = $filters['t'];
		$tn = $filters['tn'];

		$expected = Message::translate('house', array('locale' => 'de'));
		$result = $t('house', array('locale' => 'de'));
		$this->assertNotEqual($expected, $result);

		$expected = Message::translate('house', array('locale' => 'de', 'count' => 3));
		$result = $tn('house', 'houses', array('locale' => 'de'));
		$this->assertNotEqual($expected, $result);
	}

	public function testCaching() {
		$data = array('catalog' => 'Katalog');
		Catalog::write('runtime', 'message', 'de', $data, array('scope' => 'foo'));

		$this->assertFalse(Message::cache());

		$result = Message::translate('catalog', array('locale' => 'de', 'scope' => 'foo'));
		$this->assertEqual('Katalog', $result);

		$cache = Message::cache();
		$this->assertEqual('Katalog', $cache['foo']['de']['catalog']);

		Message::cache(false);
		$this->assertFalse(Message::cache());

		Message::cache(array('foo' => array('de' => array('catalog' => '<Katalog>'))));
		$result = Message::translate('catalog', array('locale' => 'de', 'scope' => 'foo'));
		$this->assertEqual('<Katalog>', $result);

		$options = array('locale' => 'de', 'scope' => 'foo', 'count' => 2);
		$this->assertEqual('<Katalog>', Message::translate('catalog', $options));

		Message::cache(false);
		Message::cache(array('foo' => array('de' => array('catalog' => array('<Katalog>')))));
		$this->assertNull(Message::translate('catalog', $options));
	}
}

?>