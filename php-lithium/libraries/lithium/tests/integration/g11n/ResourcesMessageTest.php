<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\integration\g11n;

use lithium\g11n\Catalog;

/**
 * Test for integration of g11n resources. Numbers of rules refer to those documented in
 * the document on pluralization at Mozilla.
 *
 * @link https://developer.mozilla.org/en/Localization_and_Plurals
 */
class ResourcesMessageTest extends \lithium\test\Integration {

	protected $_backup = array();

	public function setUp() {
		$this->_backup['catalogConfig'] = Catalog::config();
		Catalog::reset();
		Catalog::config(array(
			'lithium' => array(
				'adapter' => 'Php',
				'path' => LITHIUM_LIBRARY_PATH . '/lithium/g11n/resources/php'
			)
		));
	}

	public function tearDown() {
		Catalog::reset();
		Catalog::config($this->_backup['catalogConfig']);
	}

	/**
	 * Tests the plural rule #1 which applies to the following languages
	 * grouped by family and sorted alphabetically.
	 *
	 * Germanic family:
	 * - English (en)
	 * - German (de)
	 *
	 * @return void
	 */
	public function testPlurals1() {
		$locales = array(
			'en', 'de'
		);
		foreach ($locales as $locale) {
			$expected = 2;
			$result = Catalog::read(true, 'message.pluralForms', $locale);
			$this->assertEqual($expected, $result, "Locale: `{$locale}`\n{:message}");

			$rule = Catalog::read(true, 'message.pluralRule', $locale);

			$expected  = '10111111111111111111111111111111111111111111111111';
			$expected .= '11111111111111111111111111111111111111111111111111';
			$expected .= '11111111111111111111111111111111111111111111111111';
			$expected .= '11111111111111111111111111111111111111111111111111';
			$result = '';

			for ($n = 0; $n < 200; $n++) {
				$result .= $rule($n);
			}
			$this->assertIdentical($expected, $result, "Locale: `{$locale}`\n{:message}");
		}
	}

	/**
	 * Tests the plural rule #2 which applies to the following languages
	 * grouped by family and sorted alphabetically.
	 *
	 * Romanic family:
	 * - French (fr)
	 *
	 * @return void
	 */
	public function testPlurals2() {
		$locales = array(
			'fr'
		);
		foreach ($locales as $locale) {
			$expected = 2;
			$result = Catalog::read(true, 'message.pluralForms', $locale);
			$this->assertEqual($expected, $result, "Locale: `{$locale}`\n{:message}");

			$rule = Catalog::read(true, 'message.pluralRule', $locale);

			$expected  = '00111111111111111111111111111111111111111111111111';
			$expected .= '11111111111111111111111111111111111111111111111111';
			$expected .= '11111111111111111111111111111111111111111111111111';
			$expected .= '11111111111111111111111111111111111111111111111111';
			$result = '';

			for ($n = 0; $n < 200; $n++) {
				$result .= $rule($n);
			}
			$this->assertIdentical($expected, $result, "Locale: `{$locale}`\n{:message}");
		}
	}
}

?>