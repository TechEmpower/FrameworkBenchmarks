<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\util;

use lithium\util\Validator;

class ValidatorTest extends \lithium\test\Unit {

	public function setUp() {
		Validator::__init();
	}

	/**
	 * Tests static method call routing to enable patterns defined in Validator::$_rules to be
	 * called as methods.
	 */
	public function testCustomMethodDispatching() {
		$this->assertTrue(Validator::isRegex('/^abc$/'));
		$this->assertTrue(Validator::isPhone('800-999-5555'));

		$this->assertTrue(Validator::isUrl('http://example.com'));
	}

	public function testFieldOption() {
		Validator::add('isInArray', function($data, $params, $options) {
			$existing = array(
				'number' => array('one', 'two', 'three'),
				'name' => array('bob', 'bill')
			);

			$isSet = isset($existing[$options['field']]);
			$inArray = in_array($data,$existing[$options['field']]);
			return isset($options['field']) && $isSet && $inArray;
		});

		$fieldValidationRules = array(
			'number' => array('rule' => array('isInArray')),
			'name' => array('rule' => array('isInArray'))
		);

		$result = Validator::check(
			array('number' => 'one', 'name' => 'bob'),
			$fieldValidationRules
		);
		$this->assertTrue(empty($result));

		$result = Validator::check(
			array('number' => 'four', 'name' => 'bob'),
			$fieldValidationRules
		);
		$this->assertFalse(empty($result));

		$result = Validator::check(
			array('number' => 'one', 'name' => 'rex'),
			$fieldValidationRules
		);
		$this->assertFalse(empty($result));
	}

	/**
	 * Tests that new methods can be called on Validator by adding rules using Validator::add().
	 */
	public function testAddCustomRegexMethods() {
		$this->assertNull(Validator::rules('foo'));

		Validator::add('foo', '/^foo$/');
		$this->assertTrue(Validator::isFoo('foo'));
		$this->assertFalse(Validator::isFoo('bar'));
		$this->assertTrue(in_array('foo', Validator::rules()));
		$this->assertEqual('/^foo$/', Validator::rules('foo'));

		$this->expectException("Rule `bar` is not a validation rule.");
		$this->assertNull(Validator::isBar('foo'));
	}

	/**
	 * Tests that the rules state is reset when calling `Validator::__init()`.
	 */
	public function testStateReset() {
		$this->assertNull(Validator::rules('foo'));

		Validator::add('foo', '/foo/');
		$this->assertEqual('/foo/', Validator::rules('foo'));

		Validator::__init();
		$this->assertNull(Validator::rules('foo'));
	}

	/**
	 * Tests that valid and invalid UUIDs are properly detected.
	 */
	public function testUuid() {
		$this->assertTrue(Validator::isUuid('1c0a5830-6025-11de-8a39-0800200c9a66'));
		$this->assertTrue(Validator::isUuid('1c0a5831-6025-11de-8a39-0800200c9a66'));
		$this->assertTrue(Validator::isUuid('1c0a5832-6025-11de-8a39-0800200c9a66'));
		$this->assertFalse(Validator::isUuid('zc0a5832-6025-11de-8a39-0800200c9a66'));
		$this->assertFalse(Validator::isUuid('1-1c0a5832-6025-11de-8a39-0800200c9a66'));
	}

	public function testCustomWithFormat() {
		$rFormat = null;
		$function = function(&$value, $format = null, array $options = array()) use (&$rFormat) {
			$rFormat = $format;
			if ($format === 'string') {
				return true;
			}
		};
		Validator::add('test', $function);
		$validations = array(
			'inputName' => array( array( 'test', 'message' => 'foobar', 'format' => 'string' ) )
		);
		$values = array(
			'inputName' => 'blah'
		);
		$this->assertFalse((boolean) Validator::check($values, $validations));
		$this->assertEqual($rFormat, 'string');
	}

	/**
	 * Tests that new formats can be added to existing regex methods using Validator::add().
	 */
	public function testAddCustomRegexFormats() {
		$this->assertTrue(Validator::isPhone('1234567890'));
		$this->assertTrue(Validator::isPhone('+1234567890'));

		$this->assertFalse(Validator::isPhone('0800-LITHIUM'));
		Validator::add(array('phone' => array('foo' => '/^0800-[A-Z]+$/')));

		$this->assertTrue(Validator::isPhone('0800-LITHIUM'));
		$this->assertTrue(Validator::isPhone('0800-LITHIUM', 'foo'));
		$this->assertTrue(Validator::isPhone('0800-LITHIUM', 'any'));
	}

	/**
	 * Tests that setting the `'contain'` rule option to false correctly requires a string to be
	 * an exact match of the regex, with no additional characters outside.
	 */
	public function testRegexContainment() {
		$this->assertTrue(Validator::isIp('127.0.0.1', null, array('contains' => false)));

		Validator::add('foo', '/foo/', array('contains' => true));
		$this->assertTrue(Validator::isFoo('foobar'));

		Validator::add('foo', 'foo', array('contains' => false));
		$this->assertFalse(Validator::isFoo('foobar'));
		$this->assertTrue(Validator::isFoo('foo'));
	}

	/**
	 * Tests the regular expression validation for various regex delimiters
	 *
	 * @link http://www.php.net/manual/en/regexp.reference.delimiters.php Regex Delimiters
	 */
	public function testIsRegex() {
		$this->assertTrue(Validator::isRegex('/^123$/'));
		$this->assertTrue(Validator::isRegex('/^abc$/'));
		$this->assertTrue(Validator::isRegex('/^abc123$/'));
		$this->assertTrue(Validator::isRegex('@^abc$@'));
		$this->assertTrue(Validator::isRegex('#^abc$#'));
		$this->assertFalse(Validator::isRegex('d^abc$d'));

		$this->assertTrue(Validator::isRegex('(^abc$)'));
		$this->assertTrue(Validator::isRegex('{^abc$}'));
		$this->assertTrue(Validator::isRegex('[^abc$]'));
		$this->assertTrue(Validator::isRegex('<^abc$>'));
		$this->assertTrue(Validator::isRegex(')^abc$)'));
		$this->assertTrue(Validator::isRegex('}^abc$}'));
		$this->assertTrue(Validator::isRegex(']^abc$]'));
		$this->assertTrue(Validator::isRegex('>^abc$>'));

		$this->assertFalse(Validator::isRegex('\\^abc$\\'));
		$this->assertFalse(Validator::isRegex('(^abc$('));
		$this->assertFalse(Validator::isRegex('{^abc${'));
		$this->assertFalse(Validator::isRegex('[^abc$['));
		$this->assertFalse(Validator::isRegex('<^abc$<'));
	}

	public function testPrefilterMethodAccess() {
		$this->assertTrue(Validator::isNotEmpty('0'));
		$this->assertFalse(Validator::isNotEmpty(''));
		$this->assertFalse(Validator::isNotEmpty(null));
	}

	/**
	 * Tests that the 'notEmpty' rule validates correct values
	 */
	public function testNotEmptyRule() {
		$this->assertTrue(Validator::isNotEmpty('abcdefg'));
		$this->assertTrue(Validator::isNotEmpty('fasdf '));
		$this->assertTrue(Validator::isNotEmpty('fooo' . chr(243) . 'blabla'));
		$this->assertTrue(Validator::isNotEmpty('abçďĕʑʘπй'));
		$this->assertTrue(Validator::isNotEmpty('José'));
		$this->assertTrue(Validator::isNotEmpty('é'));
		$this->assertTrue(Validator::isNotEmpty('π'));
		$this->assertFalse(Validator::isNotEmpty("\t "));
		$this->assertFalse(Validator::isNotEmpty(""));
	}

	/**
	 * Tests the the 'alphaNumeric' rule validates correct values.
	 */
	public function testAlphaNumeric() {
		$this->assertTrue(Validator::isAlphaNumeric('frferrf'));
		$this->assertTrue(Validator::isAlphaNumeric('12234'));
		$this->assertTrue(Validator::isAlphaNumeric('1w2e2r3t4y'));
		$this->assertTrue(Validator::isAlphaNumeric('0'));
		$this->assertTrue(Validator::isAlphaNumeric('abçďĕʑʘπй'));
		$this->assertTrue(Validator::isAlphaNumeric('ˇˆๆゞ'));
		$this->assertTrue(Validator::isAlphaNumeric('אกあアꀀ豈'));
		$this->assertTrue(Validator::isAlphaNumeric('ǅᾈᾨ'));
		$this->assertTrue(Validator::isAlphaNumeric('ÆΔΩЖÇ'));
		$this->assertTrue(Validator::isAlphaNumeric('日本語でも'));
		$this->assertTrue(Validator::isAlphaNumeric('をありがとうございました'));

		$this->assertFalse(Validator::isAlphaNumeric('12 234'));
		$this->assertFalse(Validator::isAlphaNumeric('dfd 234'));
		$this->assertFalse(Validator::isAlphaNumeric('こんにちは！'));
		$this->assertFalse(Validator::isAlphaNumeric("\n"));
		$this->assertFalse(Validator::isAlphaNumeric("\t"));
		$this->assertFalse(Validator::isAlphaNumeric("\r"));
		$this->assertFalse(Validator::isAlphaNumeric(' '));
		$this->assertFalse(Validator::isAlphaNumeric(''));
	}

	/**
	 * Tests the the 'lengthBetween' rule validates correct values.
	 */
	public function testIsLengthBetweenRule() {
		$this->assertTrue(Validator::isLengthBetween('abcde', null, array('min' => 1, 'max' => 7)));
		$this->assertTrue(Validator::isLengthBetween('', null, array('min' => 0, 'max' => 7)));
		$this->assertFalse(Validator::isLengthBetween('abcd', null, array('min' => 1, 'max' => 3)));
	}

	public function testIsNumericRule() {
		$this->assertTrue(Validator::isNumeric(0));
		$this->assertTrue(Validator::isNumeric('0'));
		$this->assertTrue(Validator::isNumeric('-0'));
		$this->assertFalse(Validator::isNumeric('-'));
	}

	public function testBooleanValidation() {
		$this->assertTrue(Validator::isBoolean(true));
		$this->assertTrue(Validator::isBoolean(false));
		$this->assertTrue(Validator::isBoolean('true'));
		$this->assertTrue(Validator::isBoolean('false'));
		$this->assertTrue(Validator::isBoolean(0));
		$this->assertTrue(Validator::isBoolean(1));
		$this->assertTrue(Validator::isBoolean('0'));
		$this->assertTrue(Validator::isBoolean('1'));
		$this->assertTrue(Validator::isBoolean('on'));
		$this->assertTrue(Validator::isBoolean('off'));
		$this->assertTrue(Validator::isBoolean('yes'));
		$this->assertTrue(Validator::isBoolean('no'));
		$this->assertTrue(Validator::isBoolean(''));

		$this->assertFalse(Validator::isBoolean('11'));
		$this->assertFalse(Validator::isBoolean('-1'));
		$this->assertFalse(Validator::isBoolean(-1));
		$this->assertFalse(Validator::isBoolean(11));
		$this->assertFalse(Validator::isBoolean(null));
		$this->assertFalse(Validator::isBoolean('test'));
	}

	/**
	 * Test basic decimal number validation.
	 */
	public function testDecimal() {
		$this->assertTrue(Validator::isDecimal('0.0'));
		$this->assertTrue(Validator::isDecimal('0.000'));
		$this->assertTrue(Validator::isDecimal('1.1'));
		$this->assertTrue(Validator::isDecimal('11.11'));
		$this->assertTrue(Validator::isDecimal('+0'));
		$this->assertTrue(Validator::isDecimal('-0'));
		$this->assertTrue(Validator::isDecimal('+1234.54321'));
		$this->assertTrue(Validator::isDecimal('-1234.54321'));
		$this->assertTrue(Validator::isDecimal('1234.54321'));
		$this->assertTrue(Validator::isDecimal('+0123.45e6'));
		$this->assertTrue(Validator::isDecimal('-0123.45e6'));
		$this->assertTrue(Validator::isDecimal('0123.45e6'));
		$this->assertTrue(Validator::isDecimal('1234'));
		$this->assertTrue(Validator::isDecimal('-1234'));
		$this->assertTrue(Validator::isDecimal('+1234'));
		$this->assertFalse(Validator::isDecimal('string'));
	}

	/**
	 * Test decimal validation with precision specified.
	 */
	public function testDecimalWithPlaces() {
		$this->assertTrue(Validator::isDecimal('.27', null, array('precision' => '2')));
		$this->assertTrue(Validator::isDecimal(.27, null, array('precision' => 2)));
		$this->assertTrue(Validator::isDecimal(-.27, null, array('precision' => 2)));
		$this->assertTrue(Validator::isDecimal(+.27, null, array('precision' => 2)));
		$this->assertTrue(Validator::isDecimal('.277', null, array('precision' => '3')));
		$this->assertTrue(Validator::isDecimal(.277, null, array('precision' => 3)));
		$this->assertTrue(Validator::isDecimal(-.277, null, array('precision' => 3)));
		$this->assertTrue(Validator::isDecimal(+.277, null, array('precision' => 3)));
		$this->assertTrue(Validator::isDecimal('1234.5678', null, array('precision' => '4')));
		$this->assertTrue(Validator::isDecimal(1234.5678, null, array('precision' => 4)));
		$this->assertTrue(Validator::isDecimal(-1234.5678, null, array('precision' => 4)));
		$this->assertTrue(Validator::isDecimal(+1234.5678, null, array('precision' => 4)));
		$this->assertFalse(Validator::isDecimal('1234.5678', null, array('precision' => '3')));
		$this->assertFalse(Validator::isDecimal(1234.5678, null, array('precision' => 3)));
		$this->assertFalse(Validator::isDecimal(-1234.5678, null, array('precision' => 3)));
		$this->assertFalse(Validator::isDecimal(+1234.5678, null, array('precision' => 3)));
	}

	public function testEmailValidation() {
		$this->assertTrue(Validator::isEmail('abc.efg@domain.com'));
		$this->assertTrue(Validator::isEmail('efg@domain.com'));
		$this->assertTrue(Validator::isEmail('abc-efg@domain.com'));
		$this->assertTrue(Validator::isEmail('abc_efg@domain.com'));
		$this->assertTrue(Validator::isEmail('raw@test.ra.ru'));
		$this->assertTrue(Validator::isEmail('abc-efg@domain-hyphened.com'));
		$this->assertTrue(Validator::isEmail("p.o'malley@domain.com"));
		$this->assertTrue(Validator::isEmail('abc+efg@domain.com'));
		$this->assertTrue(Validator::isEmail('abc&efg@domain.com'));
		$this->assertTrue(Validator::isEmail('abc.efg@12345.com'));
		$this->assertTrue(Validator::isEmail('abc.efg@12345.co.jp'));
		$this->assertTrue(Validator::isEmail('abc@g.cn'));
		$this->assertTrue(Validator::isEmail('abc@x.com'));
		$this->assertTrue(Validator::isEmail('henrik@sbcglobal.net'));
		$this->assertTrue(Validator::isEmail('sani@sbcglobal.net'));

		/**
		 * All ICANN TLDs
		 */
		$this->assertTrue(Validator::isEmail('abc@example.aero'));
		$this->assertTrue(Validator::isEmail('abc@example.asia'));
		$this->assertTrue(Validator::isEmail('abc@example.biz'));
		$this->assertTrue(Validator::isEmail('abc@example.cat'));
		$this->assertTrue(Validator::isEmail('abc@example.com'));
		$this->assertTrue(Validator::isEmail('abc@example.coop'));
		$this->assertTrue(Validator::isEmail('abc@example.edu'));
		$this->assertTrue(Validator::isEmail('abc@example.gov'));
		$this->assertTrue(Validator::isEmail('abc@example.info'));
		$this->assertTrue(Validator::isEmail('abc@example.int'));
		$this->assertTrue(Validator::isEmail('abc@example.jobs'));
		$this->assertTrue(Validator::isEmail('abc@example.mil'));
		$this->assertTrue(Validator::isEmail('abc@example.mobi'));
		$this->assertTrue(Validator::isEmail('abc@example.museum'));
		$this->assertTrue(Validator::isEmail('abc@example.name'));
		$this->assertTrue(Validator::isEmail('abc@example.net'));
		$this->assertTrue(Validator::isEmail('abc@example.org'));
		$this->assertTrue(Validator::isEmail('abc@example.pro'));
		$this->assertTrue(Validator::isEmail('abc@example.tel'));
		$this->assertTrue(Validator::isEmail('abc@example.travel'));
		$this->assertTrue(Validator::isEmail('someone@st.t-com.hr'));

		/**
		 * Strange, but valid addresses
		 */
		$this->assertTrue(Validator::isEmail('_somename@example.com'));
		$this->assertTrue(Validator::isEmail('abc@example.c'));
		$this->assertTrue(Validator::isEmail('abc@example.com.a'));
		$this->assertTrue(Validator::isEmail('abc@example.toolong'));

		/**
		 * Addresses which are invalid, but not caught until PHP 5.3.3.
		 */
		$this->assertFalse(
			Validator::isEmail('abc@example'),
			'Invalid email address passed validation. Please update to PHP 5.3.3 ' .
			'or higher to correct this.'
		);
		$this->assertFalse(
			Validator::isEmail('abc.@example.com'),
			'Invalid email address passed validation. Please update to PHP 5.3.3 ' .
			'or higher to correct this.'
		);

		/**
		 * Invalid addresses
		 */
		$this->assertFalse(Validator::isEmail('abc@example.com.'));
		$this->assertFalse(Validator::isEmail('abc@example..com'));
		$this->assertFalse(Validator::isEmail('abc;@example.com'));
		$this->assertFalse(Validator::isEmail('abc@example.com;'));
		$this->assertFalse(Validator::isEmail('abc@efg@example.com'));
		$this->assertFalse(Validator::isEmail('abc@@example.com'));
		$this->assertFalse(Validator::isEmail('abc efg@example.com'));
		$this->assertFalse(Validator::isEmail('abc,efg@example.com'));
		$this->assertFalse(Validator::isEmail('abc@sub,example.com'));
		$this->assertFalse(Validator::isEmail("abc@sub'example.com"));
		$this->assertFalse(Validator::isEmail('abc@sub/example.com'));
		$this->assertFalse(Validator::isEmail('abc@yahoo!.com'));
		$this->assertFalse(Validator::isEmail("Nyrée.surname@example.com"));
		$this->assertFalse(Validator::isEmail('abc@example_underscored.com'));
		$this->assertFalse(Validator::isEmail('raw@test.ra.ru....com'));
	}

	/**
	 * Tests email address validation, with additional hostname lookup
	 */
	public function testEmailDomainCheck() {
		$message = "No internet connection established.";
		$this->skipIf(!$this->_hasNetwork(), $message);

		$this->assertTrue(Validator::isEmail('abc.efg@rad-dev.org', null, array('deep' => true)));
		$this->assertFalse(Validator::isEmail('abc.efg@invalidfoo.com', null, array(
			'deep' => true
		)));
		$this->assertFalse(Validator::isEmail('abc@example.abcd', null, array('deep' => true)));
	}

	/**
	 * Tests 'inList' validation.
	 */
	public function testInList() {
		$this->assertTrue(Validator::isInList('one', null, array('list' => array('one', 'two'))));
		$this->assertTrue(Validator::isInList('two', null, array('list' => array('one', 'two'))));
		$this->assertFalse(Validator::isInList('3', null, array('list' => array('one', 'two'))));

		$this->assertFalse(Validator::isInList('', null, array('list' => array('0', '1'))));
		$this->assertFalse(Validator::isInList(null, null, array('list' => array('0', '1'))));
		$this->assertFalse(Validator::isInList(false, null, array('list' => array('0', '1'))));
		$this->assertFalse(Validator::isInList(true, null, array('list' => array('0', '1'))));

		$this->assertFalse(Validator::isInList('', null, array('list' => array(0, 1))));
		$this->assertFalse(Validator::isInList(null, null, array('list' => array(0, 1))));
		$this->assertFalse(Validator::isInList(false, null, array('list' => array(0, 1))));
		$this->assertFalse(Validator::isInList(true, null, array('list' => array(0, 1))));
		$this->assertTrue(Validator::isInList(0, null, array('list' => array(0, 1))));
		$this->assertTrue(Validator::isInList(1, null, array('list' => array(0, 1))));
		$this->assertFalse(Validator::isInList(2, null, array('list' => array(0, 1))));

		$this->assertTrue(Validator::isInList(0, null, array('list' => array('0', '1'))));
		$this->assertTrue(Validator::isInList('1', null, array('list' => array('0', '1'))));

		$this->assertTrue(Validator::isInList(1, null, array('list' => array('0', '1'))));
		$this->assertTrue(Validator::isInList('1', null, array('list' => array('0', '1'))));

		$this->assertFalse(Validator::isInList(2, null, array('list' => array('0', '1'))));
		$this->assertFalse(Validator::isInList('2', null, array('list' => array('0', '1'))));
	}


	/**
	 * Tests credit card validation for numbers in various vendors' formats.
	 */
	public function testCreditCardValidation() {

		/**
		 * American Express
		 */
		$this->assertTrue(Validator::isCreditCard('370482756063980', 'amex'));
		$this->assertTrue(Validator::isCreditCard('3491-0643-3773-483', 'amex'));
		$this->assertTrue(Validator::isCreditCard('344671486204764', 'amex'));
		$this->assertTrue(Validator::isCreditCard('344042544509943', 'amex'));
		$this->assertTrue(Validator::isCreditCard('377147515754475', 'amex'));
		$this->assertTrue(Validator::isCreditCard('375239372816422', 'amex'));
		$this->assertTrue(Validator::isCreditCard('376294341957707', 'amex'));
		$this->assertTrue(Validator::isCreditCard('341779292230411', 'amex'));
		$this->assertTrue(Validator::isCreditCard('341646919853372', 'amex'));
		$this->assertTrue(Validator::isCreditCard('348498616319346', 'amex', array(
			'deep' => true
		)));
		$this->assertFalse(Validator::isCreditCard('5610376649499352', 'amex'));

		/**
		 * BankCard
		 */
		$this->assertTrue(Validator::isCreditCard('5610 7458 6741 3420', 'bankcard'));
		$this->assertTrue(Validator::isCreditCard('5610376649499352', 'bankcard'));
		$this->assertTrue(Validator::isCreditCard('5610091936000694', 'bankcard'));
		$this->assertTrue(Validator::isCreditCard('5602248780118788', 'bankcard'));
		$this->assertTrue(Validator::isCreditCard('5610631567676765', 'bankcard'));
		$this->assertTrue(Validator::isCreditCard('5602238211270795', 'bankcard'));
		$this->assertTrue(Validator::isCreditCard('5610173951215470', 'bankcard'));
		$this->assertTrue(Validator::isCreditCard('5610139705753702', 'bankcard'));
		$this->assertTrue(Validator::isCreditCard('5602226032150551', 'bankcard'));
		$this->assertTrue(Validator::isCreditCard('5602223993735777', 'bankcard'));
		$this->assertFalse(Validator::isCreditCard('30155483651028', 'bankcard'));

		/**
		 * Diners Club 14
		 */
		$this->assertTrue(Validator::isCreditCard('30155483651028', 'diners'));
		$this->assertTrue(Validator::isCreditCard('36371312803821', 'diners'));
		$this->assertTrue(Validator::isCreditCard('38801277489875', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30348560464296', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30349040317708', 'diners'));
		$this->assertTrue(Validator::isCreditCard('36567413559978', 'diners'));
		$this->assertTrue(Validator::isCreditCard('36051554732702', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30391842198191', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30172682197745', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30162056566641', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30085066927745', 'diners'));
		$this->assertTrue(Validator::isCreditCard('36519025221976', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30372679371044', 'diners'));
		$this->assertTrue(Validator::isCreditCard('38913939150124', 'diners'));
		$this->assertTrue(Validator::isCreditCard('36852899094637', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30138041971120', 'diners'));
		$this->assertTrue(Validator::isCreditCard('36184047836838', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30057460264462', 'diners'));
		$this->assertTrue(Validator::isCreditCard('38980165212050', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30356516881240', 'diners'));
		$this->assertTrue(Validator::isCreditCard('38744810033182', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30173638706621', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30158334709185', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30195413721186', 'diners'));
		$this->assertTrue(Validator::isCreditCard('38863347694793', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30275627009113', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30242860404971', 'diners'));
		$this->assertTrue(Validator::isCreditCard('30081877595151', 'diners'));
		$this->assertTrue(Validator::isCreditCard('38053196067461', 'diners'));
		$this->assertTrue(Validator::isCreditCard('36520379984870', 'diners'));

		/**
		 * 2004 MasterCard/Diners Club Alliance International 14
		 */
		$this->assertTrue(Validator::isCreditCard('36747701998969', 'diners'));
		$this->assertTrue(Validator::isCreditCard('36427861123159', 'diners'));
		$this->assertTrue(Validator::isCreditCard('36150537602386', 'diners'));
		$this->assertTrue(Validator::isCreditCard('36582388820610', 'diners'));
		$this->assertTrue(Validator::isCreditCard('36729045250216', 'diners'));

		/**
		 * 2004 MasterCard/Diners Club Alliance US & Canada 16
		 */
		$this->assertTrue(Validator::isCreditCard('5597511346169950', 'diners'));
		$this->assertTrue(Validator::isCreditCard('5526443162217562', 'diners'));
		$this->assertTrue(Validator::isCreditCard('5577265786122391', 'diners'));
		$this->assertTrue(Validator::isCreditCard('5534061404676989', 'diners'));
		$this->assertTrue(Validator::isCreditCard('5545313588374502', 'diners'));
		$this->assertFalse(Validator::isCreditCard('6011802876467237', 'diners'));

		/**
		 * Discover
		 */
		$this->assertTrue(Validator::isCreditCard('6011802876467237', 'disc'));
		$this->assertTrue(Validator::isCreditCard('6506432777720955', 'disc'));
		$this->assertTrue(Validator::isCreditCard('6011126265283942', 'disc'));
		$this->assertTrue(Validator::isCreditCard('6502187151579252', 'disc'));
		$this->assertTrue(Validator::isCreditCard('6506600836002298', 'disc'));
		$this->assertTrue(Validator::isCreditCard('6504376463615189', 'disc'));
		$this->assertTrue(Validator::isCreditCard('6011440907005377', 'disc'));
		$this->assertTrue(Validator::isCreditCard('6509735979634270', 'disc'));
		$this->assertTrue(Validator::isCreditCard('6011422366775856', 'disc'));
		$this->assertTrue(Validator::isCreditCard('6500976374623323', 'disc'));
		$this->assertFalse(Validator::isCreditCard('201496944158937', 'disc'));

		/**
		 * enRoute
		 */
		$this->assertTrue(Validator::isCreditCard('201496944158937', 'enroute'));
		$this->assertTrue(Validator::isCreditCard('214945833739665', 'enroute'));
		$this->assertTrue(Validator::isCreditCard('214982692491187', 'enroute'));
		$this->assertTrue(Validator::isCreditCard('214901395949424', 'enroute'));
		$this->assertTrue(Validator::isCreditCard('201480676269187', 'enroute'));
		$this->assertTrue(Validator::isCreditCard('214911922887807', 'enroute'));
		$this->assertTrue(Validator::isCreditCard('201485025457250', 'enroute'));
		$this->assertTrue(Validator::isCreditCard('201402662758866', 'enroute'));
		$this->assertTrue(Validator::isCreditCard('214981579370225', 'enroute'));
		$this->assertTrue(Validator::isCreditCard('201447595859877', 'enroute'));
		$this->assertFalse(Validator::isCreditCard('210034762247893', 'enroute'));

		/**
		 * JCB 15 digit
		 */
		$this->assertTrue(Validator::isCreditCard('210034762247893', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('180078671678892', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('180010559353736', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('210095474464258', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('210006675562188', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('210063299662662', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('180032506857825', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('210057919192738', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('180031358949367', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('180033802147846', 'jcb'));

		/**
		 * JCB 16 digit
		 */
		$this->assertTrue(Validator::isCreditCard('3096806857839939', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3158699503187091', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3112549607186579', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3112332922425604', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3112001541159239', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3112162495317841', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3337562627732768', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3337107161330775', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3528053736003621', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3528915255020360', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3096786059660921', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3528264799292320', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3096469164130136', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3112127443822853', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3096849995802328', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3528090735127407', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3112101006819234', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3337444428040784', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3088043154151061', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3088295969414866', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3158748843158575', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3158709206148538', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3158365159575324', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3158671691305165', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3528523028771093', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3096057126267870', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3158514047166834', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3528274546125962', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3528890967705733', 'jcb'));
		$this->assertTrue(Validator::isCreditCard('3337198811307545', 'jcb'));
		$this->assertFalse(Validator::isCreditCard('5020147409985219', 'jcb'));

		/**
		 * Maestro (debit card)
		 */
		$this->assertTrue(Validator::isCreditCard('5020147409985219', 'maestro'));
		$this->assertTrue(Validator::isCreditCard('5020931809905616', 'maestro'));
		$this->assertTrue(Validator::isCreditCard('5020412965470224', 'maestro'));
		$this->assertTrue(Validator::isCreditCard('5020129740944022', 'maestro'));
		$this->assertTrue(Validator::isCreditCard('5020024696747943', 'maestro'));
		$this->assertTrue(Validator::isCreditCard('5020581514636509', 'maestro'));
		$this->assertTrue(Validator::isCreditCard('5020695008411987', 'maestro'));
		$this->assertTrue(Validator::isCreditCard('5020565359718977', 'maestro'));
		$this->assertTrue(Validator::isCreditCard('6339931536544062', 'maestro'));
		$this->assertTrue(Validator::isCreditCard('6465028615704406', 'maestro'));
		$this->assertFalse(Validator::isCreditCard('5580424361774366', 'maestro'));

		/**
		 * MasterCard
		 */
		$this->assertTrue(Validator::isCreditCard('5580424361774366', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5589563059318282', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5387558333690047', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5163919215247175', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5386742685055055', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5102303335960674', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5526543403964565', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5538725892618432', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5119543573129778', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5391174753915767', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5510994113980714', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5183720260418091', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5488082196086704', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5484645164161834', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5171254350337031', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5526987528136452', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5504148941409358', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5240793507243615', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5162114693017107', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5163104807404753', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5590136167248365', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5565816281038948', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5467639122779531', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5297350261550024', 'mc'));
		$this->assertTrue(Validator::isCreditCard('5162739131368058', 'mc'));
		$this->assertFalse(Validator::isCreditCard('6767432107064987', 'mc'));

		/**
		 * Solo 16
		 */
		$this->assertTrue(Validator::isCreditCard('6767432107064987', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6334667758225411', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6767037421954068', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6767823306394854', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6334768185398134', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6767286729498589', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6334972104431261', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6334843427400616', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6767493947881311', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6767194235798817', 'solo'));

		/**
		 * Solo 18
		 */
		$this->assertTrue(Validator::isCreditCard('676714834398858593', 'solo'));
		$this->assertTrue(Validator::isCreditCard('676751666435130857', 'solo'));
		$this->assertTrue(Validator::isCreditCard('676781908573924236', 'solo'));
		$this->assertTrue(Validator::isCreditCard('633488724644003240', 'solo'));
		$this->assertTrue(Validator::isCreditCard('676732252338067316', 'solo'));
		$this->assertTrue(Validator::isCreditCard('676747520084495821', 'solo'));
		$this->assertTrue(Validator::isCreditCard('633465488901381957', 'solo'));
		$this->assertTrue(Validator::isCreditCard('633487484858610484', 'solo'));
		$this->assertTrue(Validator::isCreditCard('633453764680740694', 'solo'));
		$this->assertTrue(Validator::isCreditCard('676768613295414451', 'solo'));

		/**
		 * Solo 19
		 */
		$this->assertTrue(Validator::isCreditCard('6767838565218340113', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6767760119829705181', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6767265917091593668', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6767938856947440111', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6767501945697390076', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6334902868716257379', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6334922127686425532', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6334933119080706440', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6334647959628261714', 'solo'));
		$this->assertTrue(Validator::isCreditCard('6334527312384101382', 'solo'));
		$this->assertFalse(Validator::isCreditCard('5641829171515733', 'solo'));

		/**
		 * Switch 16
		 */
		$this->assertTrue(Validator::isCreditCard('5641829171515733', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641824852820809', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759129648956909', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759626072268156', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641822698388957', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641827123105470', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641823755819553', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641821939587682', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936097148079186', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641829739125009', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641822860725507', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936717688865831', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759487613615441', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641821346840617', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641825793417126', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641821302759595', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759784969918837', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641824910667036', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759139909636173', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6333425070638022', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641823910382067', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936295218139423', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6333031811316199', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936912044763198', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936387053303824', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759535838760523', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6333427174594051', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641829037102700', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641826495463046', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6333480852979946', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641827761302876', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641825083505317', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759298096003991', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936119165483420', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936190990500993', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4903356467384927', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6333372765092554', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641821330950570', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759841558826118', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936164540922452', 'switch'));

		/**
		 * Switch 18
		 */
		$this->assertTrue(Validator::isCreditCard('493622764224625174', 'switch'));
		$this->assertTrue(Validator::isCreditCard('564182823396913535', 'switch'));
		$this->assertTrue(Validator::isCreditCard('675917308304801234', 'switch'));
		$this->assertTrue(Validator::isCreditCard('675919890024220298', 'switch'));
		$this->assertTrue(Validator::isCreditCard('633308376862556751', 'switch'));
		$this->assertTrue(Validator::isCreditCard('564182377633208779', 'switch'));
		$this->assertTrue(Validator::isCreditCard('564182870014926787', 'switch'));
		$this->assertTrue(Validator::isCreditCard('675979788553829819', 'switch'));
		$this->assertTrue(Validator::isCreditCard('493668394358130935', 'switch'));
		$this->assertTrue(Validator::isCreditCard('493637431790930965', 'switch'));
		$this->assertTrue(Validator::isCreditCard('633321438601941513', 'switch'));
		$this->assertTrue(Validator::isCreditCard('675913800898840986', 'switch'));
		$this->assertTrue(Validator::isCreditCard('564182592016841547', 'switch'));
		$this->assertTrue(Validator::isCreditCard('564182428380440899', 'switch'));
		$this->assertTrue(Validator::isCreditCard('493696376827623463', 'switch'));
		$this->assertTrue(Validator::isCreditCard('675977939286485757', 'switch'));
		$this->assertTrue(Validator::isCreditCard('490302699502091579', 'switch'));
		$this->assertTrue(Validator::isCreditCard('564182085013662230', 'switch'));
		$this->assertTrue(Validator::isCreditCard('493693054263310167', 'switch'));
		$this->assertTrue(Validator::isCreditCard('633321755966697525', 'switch'));
		$this->assertTrue(Validator::isCreditCard('675996851719732811', 'switch'));
		$this->assertTrue(Validator::isCreditCard('493699211208281028', 'switch'));
		$this->assertTrue(Validator::isCreditCard('493697817378356614', 'switch'));
		$this->assertTrue(Validator::isCreditCard('675968224161768150', 'switch'));
		$this->assertTrue(Validator::isCreditCard('493669416873337627', 'switch'));
		$this->assertTrue(Validator::isCreditCard('564182439172549714', 'switch'));
		$this->assertTrue(Validator::isCreditCard('675926914467673598', 'switch'));
		$this->assertTrue(Validator::isCreditCard('564182565231977809', 'switch'));
		$this->assertTrue(Validator::isCreditCard('675966282607849002', 'switch'));
		$this->assertTrue(Validator::isCreditCard('493691609704348548', 'switch'));
		$this->assertTrue(Validator::isCreditCard('675933118546065120', 'switch'));
		$this->assertTrue(Validator::isCreditCard('493631116677238592', 'switch'));
		$this->assertTrue(Validator::isCreditCard('675921142812825938', 'switch'));
		$this->assertTrue(Validator::isCreditCard('633338311815675113', 'switch'));
		$this->assertTrue(Validator::isCreditCard('633323539867338621', 'switch'));
		$this->assertTrue(Validator::isCreditCard('675964912740845663', 'switch'));
		$this->assertTrue(Validator::isCreditCard('633334008833727504', 'switch'));
		$this->assertTrue(Validator::isCreditCard('493631941273687169', 'switch'));
		$this->assertTrue(Validator::isCreditCard('564182971729706785', 'switch'));
		$this->assertTrue(Validator::isCreditCard('633303461188963496', 'switch'));

		/**
		 * Switch 19
		 */
		$this->assertTrue(Validator::isCreditCard('6759603460617628716', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936705825268647681', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641829846600479183', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759389846573792530', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936189558712637603', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641822217393868189', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4903075563780057152', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936510653566569547', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936503083627303364', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936777334398116272', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641823876900554860', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759619236903407276', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759011470269978117', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6333175833997062502', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759498728789080439', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4903020404168157841', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759354334874804313', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759900856420875115', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641827269346868860', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641828995047453870', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6333321884754806543', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6333108246283715901', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759572372800700102', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4903095096797974933', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6333354315797920215', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759163746089433755', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759871666634807647', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641827883728575248', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936527975051407847', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641823318396882141', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759123772311123708', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4903054736148271088', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936477526808883952', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936433964890967966', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6333245128906049344', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936321036970553134', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936111816358702773', 'switch'));
		$this->assertTrue(Validator::isCreditCard('4936196077254804290', 'switch'));
		$this->assertTrue(Validator::isCreditCard('6759558831206830183', 'switch'));
		$this->assertTrue(Validator::isCreditCard('5641827998830403137', 'switch'));
		$this->assertFalse(Validator::isCreditCard('4024007174754', 'switch'));

		/**
		 * Visa 13 digit
		 */
		$this->assertTrue(Validator::isCreditCard('4024007174754', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4104816460717', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4716229700437', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539305400213', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4728260558665', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4929100131792', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4024007117308', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539915491024', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539790901139', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4485284914909', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4782793022350', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4556899290685', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4024007134774', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4333412341316', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539534204543', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4485640373626', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4929911445746', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539292550806', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4716523014030', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4024007125152', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539758883311', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4024007103258', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4916933155767', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4024007159672', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4716935544871', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4929415177779', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4929748547896', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4929153468612', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539397132104', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4485293435540', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4485799412720', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4916744757686', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4556475655426', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539400441625', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4485437129173', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4716253605320', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539366156589', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4916498061392', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4716127163779', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4024007183078', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4041553279654', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4532380121960', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4485906062491', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539365115149', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4485146516702', 'visa'));

		/**
		 * Visa 16 digit
		 */
		$this->assertTrue(Validator::isCreditCard('4916375389940009', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4929167481032610', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4485029969061519', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4485573845281759', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4485669810383529', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4929615806560327', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4556807505609535', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4532611336232890', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4532201952422387', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4485073797976290', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4024007157580969', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4053740470212274', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4716265831525676', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4024007100222966', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539556148303244', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4532449879689709', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4916805467840986', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4532155644440233', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4467977802223781', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539224637000686', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4556629187064965', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4532970205932943', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4821470132041850', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4916214267894485', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4024007169073284', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4716783351296122', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4556480171913795', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4929678411034997', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4682061913519392', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4916495481746474', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4929007108460499', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539951357838586', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4716482691051558', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4916385069917516', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4929020289494641', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4532176245263774', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4556242273553949', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4481007485188614', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4716533372139623', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4929152038152632', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4539404037310550', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4532800925229140', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4916845885268360', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4394514669078434', 'visa'));
		$this->assertTrue(Validator::isCreditCard('4485611378115042', 'visa'));
		$this->assertFalse(Validator::isCreditCard('869940697287073', 'visa'));

		/**
		 * Visa Electron
		 */
		$this->assertTrue(Validator::isCreditCard('4175003346287100', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4913042516577228', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4917592325659381', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4917084924450511', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4917994610643999', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4175005933743585', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4175008373425044', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4913119763664154', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4913189017481812', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4913085104968622', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4175008803122021', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4913294453962489', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4175009797419290', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4175005028142917', 'electron'));
		$this->assertTrue(Validator::isCreditCard('4913940802385364', 'electron'));
		$this->assertFalse(Validator::isCreditCard('869940697287073', 'electron'));

		/**
		 * Voyager
		 */
		$this->assertTrue(Validator::isCreditCard('869940697287073', 'voyager'));
		$this->assertTrue(Validator::isCreditCard('869934523596112', 'voyager'));
		$this->assertTrue(Validator::isCreditCard('869958670174621', 'voyager'));
		$this->assertTrue(Validator::isCreditCard('869921250068209', 'voyager'));
		$this->assertTrue(Validator::isCreditCard('869972521242198', 'voyager'));
		$this->assertFalse(Validator::isCreditCard('370482756063980', 'voyager'));

		$this->assertTrue(Validator::isLuhn('869972521242198'));
		$this->assertFalse(Validator::isLuhn(false));
		$this->assertFalse(Validator::isLuhn(null));
		$this->assertFalse(Validator::isLuhn(''));
		$this->assertFalse(Validator::isLuhn(true));
	}

	public function testCheckHasErrors() {
		$rules = array('title' => array('please enter a title'));
		$result = Validator::check(array(), $rules);
		$this->assertFalse(empty($result));

		$expected = array('title' => array('please enter a title'));
		$this->assertEqual($expected, $result);
	}

	public function testCheckPasses() {
		$rules = array('title' => 'please enter a title');
		$data = array('title' => 'new title');
		$result = Validator::check($data, $rules);
		$this->assertTrue(empty($result));
	}

	public function testCheckSkipEmpty() {
		$rules = array(
			'email' => array('email', 'skipEmpty' => true, 'message' => 'email is not valid')
		);

		// empty string should pass
		$data = array('email' => '');
		$result = Validator::check($data, $rules);
		$this->assertTrue(empty($result));

		// null value should pass
		$data = array('email' => null);
		$result = Validator::check($data, $rules);
		$this->assertTrue(empty($result));

		// string with spaces should NOT pass
		$data = array('email' => ' ');
		$result = Validator::check($data, $rules);
		$this->assertFalse(empty($result));
	}

	public function testCheckMultipleHasErrors() {
		$rules = array(
			'title' => 'please enter a title',
			'email' => array(
				array('notEmpty', 'message' => 'email is empty'),
				array('email', 'message' => 'email is not valid')
			)
		);
		$result = Validator::check(array(), $rules);
		$this->assertFalse(empty($result));

		$expected = array(
			'title' => array('please enter a title'),
			'email' => array('email is empty', 'email is not valid')
		);
		$this->assertEqual($expected, $result);
	}

	public function testCheckWithLastRule() {
		$rules = array(
			'title' => array('please enter a title'),
			'email' => array(
				array('notEmpty', 'message' => 'email is empty', 'last' => true),
				array('email', 'message' => 'email is invalid')
			)
		);
		$result = Validator::check(array(), $rules);
		$this->assertFalse(empty($result));

		$expected = array(
			'title' => array('title is empty'),
			'email' => array('email is empty')
		);
		$this->assertEqual($expected, $result);
	}

	public function testCheckMultipleHasFirstError() {
		$rules = array(
			'title' => 'please enter a title',
			'email' => array(
				array('notEmpty', 'message' => 'email is empty'),
				array('email', 'message' => 'email is not valid')
			)
		);
		$data = array('email' => 'something');
		$result = Validator::check($data, $rules);

		// result:
		$errors = array(
			'title' => array('please enter a title'),
			'email' => array('email is not valid')
		);
		$this->assertFalse(empty($result));
		$this->assertEqual($errors, $result);
	}

	public function testCheckMultipleHasOneError() {
		$rules = array(
			'title' => 'please enter a title',
			'email' => array(
				array('notEmpty', 'message' => 'email is empty'),
				array('email', 'message' => 'email is not valid')
			)
		);
		$data = array('title' => 'new title', 'email' => 'something');
		$result = Validator::check($data, $rules);
		$this->assertFalse(empty($result));

		$expected = array('email' => array('email is not valid'));
		$this->assertEqual($expected, $result);
	}

	public function testCheckMultiplePasses() {
		$rules = array(
			'title' => 'please enter a title',
			'email' => array(
				array('notEmpty', 'message' => 'email is empty'),
				array('email', 'message' => 'email is not valid')
			)
		);
		$data = array('title' => 'new title', 'email' => 'something@test.com');
		$result = Validator::check($data, $rules);
		$this->assertTrue(empty($result));

		$expected = array();
		$this->assertEqual($expected, $result);
	}

	public function testIsInRange() {
		$lower = 1;
		$upper = 10;

		$value = 0;
		$result = Validator::isInRange($value, null, compact('lower', 'upper'));
		$this->assertFalse($result);

		$value = 1;
		$result = Validator::isInRange($value, null, compact('lower', 'upper'));
		$this->assertTrue($result);

		$value = 5;
		$result = Validator::isInRange($value, null, compact('lower', 'upper'));
		$this->assertTrue($result);

		$value = 10;
		$result = Validator::isInRange($value, null, compact('lower', 'upper'));
		$this->assertTrue($result);

		$value = 11;
		$result = Validator::isInRange($value, null, compact('lower', 'upper'));
		$this->assertFalse($result);

		$result = Validator::isInRange(-1, null, array('upper' => 1));
		$this->assertTrue($result);

		$result = Validator::isInRange(1, null, array('upper' => 1));
		$this->assertTrue($result);

		$result = Validator::isInRange(2, null, array('upper' => 1));
		$this->assertFalse($result);

		$result = Validator::isInRange(2, null, array('lower' => 1));
		$this->assertTrue($result);

		$result = Validator::isInRange(1, null, array('lower' => 1));
		$this->assertTrue($result);

		$result = Validator::isInRange(0, null, array('lower' => 1));
		$this->assertFalse($result);

		$this->assertTrue(Validator::isInRange(0));
	}

	public function testValidationWithContextData() {
		Validator::add('someModelRule', function($value, $format, $options) {
			return $value === 'Title' && $options['values']['body'] === 'Body';
		});

		$result = Validator::check(
			array('title' => 'Title', 'body' => 'Body'),
			array('title' => array('someModelRule'))
		);
		$this->assertIdentical(array(), $result);

		$result = Validator::check(
			array('title' => 'Title', 'body' => 'Not Body'),
			array('title' => array('someModelRule'))
		);
		$this->assertIdentical(array('title' => array(0)), $result);
	}

	/**
	 * Tests that event flags applied to rules only trigger when the corresponding event is passed
	 * in the `$options` parameter of `check()`.
	 */
	public function testEvents() {
		$rules = array('number' => array('numeric', 'message' => 'Badness!'));
		$expected = array('number' => array('Badness!'));

		$result = Validator::check(array('number' => 'o'), $rules);
		$this->assertEqual($expected, $result);

		$rules['number']['on'] = 'foo';
		$result = Validator::check(array('number' => 'o'), $rules, array('events' => 'foo'));
		$this->assertEqual($expected, $result);

		$result = Validator::check(array('number' => 'o'), $rules, array('events' => 'bar'));
		$this->assertEqual(array(), $result);

		$result = Validator::check(array('number' => 'o'), $rules, array(
			'events' => array('foo', 'bar')
		));
		$this->assertEqual($expected, $result);

		$result = Validator::check(array('number' => 'o'), $rules, array(
			'events' => array('bar', 'baz')
		));
		$this->assertEqual(array(), $result);

		unset($rules['number']['on']);
		$result = Validator::check(array('number' => 'o'), $rules, array('events' => 'foo'));
		$this->assertEqual($expected, $result);
	}

	/**
	 * Tests validating nested fields using dot-separated paths.
	 */
	public function testNestedFields() {
		$rules = array(
			'id' => array('numeric', 'message' => 'Bad ID'),
			'profile.name' => "Can't be empty",
			'profile.email' => array('email', 'message' => 'Must be a valid email')
		);
		$data = array('id' => 1, 'profile' => array('email' => 'foo'));
		$result = Validator::check($data, $rules);
		$expected = array(
			'profile.name' => array("Can't be empty"),
			'profile.email' => array('Must be a valid email')
		);
		$this->assertEqual($expected, $result);

		$data = array('id' => '.', 'profile' => array('email' => 'foo@bar.com', 'name' => 'Bob'));
		$result = Validator::check($data, $rules);
		$this->assertEqual(array('id' => array('Bad ID')), $result);
	}

	public function testRespondsToParentCall() {
		$this->assertTrue(Validator::respondsTo('applyFilter'));
		$this->assertFalse(Validator::respondsTo('fooBarBaz'));
	}

	public function testRespondsToMagic() {
		$this->assertTrue(Validator::respondsTo('isAlphaNumeric'));
		$this->assertTrue(Validator::respondsTo('isCreditCard'));
		$this->assertFalse(Validator::respondsTo('isFoobar'));
	}

}

?>