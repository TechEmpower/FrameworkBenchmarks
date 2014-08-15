<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Tests the Valid class
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.valid
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @author     BRMatt <matthew@sigswitch.com>
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_ValidTest extends Unittest_TestCase
{

	/**
	 * Provides test data for test_alpha()
	 * @return array
	 */
	public function provider_alpha()
	{
		return array(
			array('asdavafaiwnoabwiubafpowf', TRUE),
			array('!aidhfawiodb', FALSE),
			array('51535oniubawdawd78', FALSE),
			array('!"£$(G$W£(HFW£F(HQ)"n', FALSE),
			// UTF-8 tests
			array('あいうえお', TRUE, TRUE),
			array('¥', FALSE, TRUE),
			// Empty test
			array('', FALSE, FALSE),
			array(NULL, FALSE, FALSE),
			array(FALSE, FALSE, FALSE),
		);
	}

	/**
	 * Tests Valid::alpha()
	 *
	 * Checks whether a string consists of alphabetical characters only.
	 *
	 * @test
	 * @dataProvider provider_alpha
	 * @param string  $string
	 * @param boolean $expected
	 */
	public function test_alpha($string, $expected, $utf8 = FALSE)
	{
		$this->assertSame(
			$expected,
			Valid::alpha($string, $utf8)
		);
	}

	/*
	 * Provides test data for test_alpha_numeric
	 */
	public function provide_alpha_numeric()
	{
		return array(
			array('abcd1234',  TRUE),
			array('abcd',      TRUE),
			array('1234',      TRUE),
			array('abc123&^/-', FALSE),
			// UTF-8 tests
			array('あいうえお', TRUE, TRUE),
			array('零一二三四五', TRUE, TRUE),
			array('あい四五£^£^', FALSE, TRUE),
			// Empty test
			array('', FALSE, FALSE),
			array(NULL, FALSE, FALSE),
			array(FALSE, FALSE, FALSE),
		);
	}

	/**
	 * Tests Valid::alpha_numeric()
	 *
	 * Checks whether a string consists of alphabetical characters and numbers only.
	 *
	 * @test
	 * @dataProvider provide_alpha_numeric
	 * @param string  $input     The string to test
	 * @param boolean $expected  Is $input valid
	 */
	public function test_alpha_numeric($input, $expected, $utf8 = FALSE)
	{
		$this->assertSame(
			$expected,
			Valid::alpha_numeric($input, $utf8)
		);
	}

	/**
	 * Provides test data for test_alpha_dash
	 */
	public function provider_alpha_dash()
	{
		return array(
			array('abcdef',     TRUE),
			array('12345',      TRUE),
			array('abcd1234',   TRUE),
			array('abcd1234-',  TRUE),
			array('abc123&^/-', FALSE),
			// Empty test
			array('', FALSE),
			array(NULL, FALSE),
			array(FALSE, FALSE),
		);
	}

	/**
	 * Tests Valid::alpha_dash()
	 *
	 * Checks whether a string consists of alphabetical characters, numbers, underscores and dashes only.
	 *
	 * @test
	 * @dataProvider provider_alpha_dash
	 * @param string  $input          The string to test
	 * @param boolean $contains_utf8  Does the string contain utf8 specific characters
	 * @param boolean $expected       Is $input valid?
	 */
	public function test_alpha_dash($input, $expected, $contains_utf8 = FALSE)
	{
		if ( ! $contains_utf8)
		{
			$this->assertSame(
				$expected,
				Valid::alpha_dash($input)
			);
		}

		$this->assertSame(
			$expected,
			Valid::alpha_dash($input, TRUE)
		);
	}

	/**
	 * DataProvider for the valid::date() test
	 */
	public function provider_date()
	{
		return array(
			array('now',TRUE),
			array('10 September 2010',TRUE),
			array('+1 day',TRUE),
			array('+1 week',TRUE),
			array('+1 week 2 days 4 hours 2 seconds',TRUE),
			array('next Thursday',TRUE),
			array('last Monday',TRUE),

			array('blarg',FALSE),
			array('in the year 2000',FALSE),
			array('324824',FALSE),
			// Empty test
			array('', FALSE),
			array(NULL, FALSE),
			array(FALSE, FALSE),
		);
	}

	/**
	 * Tests Valid::date()
	 *
	 * @test
	 * @dataProvider provider_date
	 * @param string  $date  The date to validate
	 * @param integer $expected
	 */
	public function test_date($date, $expected)
	{
		$this->assertSame(
			$expected,
			Valid::date($date, $expected)
		);
	}

	/**
	 * DataProvider for the valid::decimal() test
	 */
	public function provider_decimal()
	{
		return array(
			// Empty test
			array('', 2, NULL, FALSE),
			array(NULL, 2, NULL, FALSE),
			array(FALSE, 2, NULL, FALSE),
			array('45.1664', 3, NULL, FALSE),
			array('45.1664', 4, NULL, TRUE),
			array('45.1664', 4, 2, TRUE),
			array('-45.1664', 4, NULL, TRUE),
			array('+45.1664', 4, NULL, TRUE),
			array('-45.1664', 3, NULL, FALSE),
		);
	}

	/**
	 * Tests Valid::decimal()
	 *
	 * @test
	 * @dataProvider provider_decimal
	 * @param string  $decimal  The decimal to validate
	 * @param integer $places   The number of places to check to
	 * @param integer $digits   The number of digits preceding the point to check
	 * @param boolean $expected Whether $decimal conforms to $places AND $digits
	 */
	public function test_decimal($decimal, $places, $digits, $expected)
	{
		$this->assertSame(
			$expected,
			Valid::decimal($decimal, $places, $digits),
			'Decimal: "'.$decimal.'" to '.$places.' places and '.$digits.' digits (preceeding period)'
		);
	}

	/**
	 * Provides test data for test_digit
	 * @return array
	 */
	public function provider_digit()
	{
		return array(
			array('12345',    TRUE),
			array('10.5',     FALSE),
			array('abcde',    FALSE),
			array('abcd1234', FALSE),
			array('-5',       FALSE),
			array(-5,         FALSE),
			// Empty test
			array('',         FALSE),
			array(NULL,       FALSE),
			array(FALSE,      FALSE),
		);
	}

	/**
	 * Tests Valid::digit()
	 *
	 * @test
	 * @dataProvider provider_digit
	 * @param mixed   $input     Input to validate
	 * @param boolean $expected  Is $input valid
	 */
	public function test_digit($input, $expected, $contains_utf8 = FALSE)
	{
		if ( ! $contains_utf8)
		{
			$this->assertSame(
				$expected,
				Valid::digit($input)
			);
		}

		$this->assertSame(
			$expected,
			Valid::digit($input, TRUE)
		);

	}

	/**
	 * DataProvider for the valid::color() test
	 */
	public function provider_color()
	{
		return array(
			array('#000000', TRUE),
			array('#GGGGGG', FALSE),
			array('#AbCdEf', TRUE),
			array('#000', TRUE),
			array('#abc', TRUE),
			array('#DEF', TRUE),
			array('000000', TRUE),
			array('GGGGGG', FALSE),
			array('AbCdEf', TRUE),
			array('000', TRUE),
			array('DEF', TRUE),
			// Empty test
			array('', FALSE),
			array(NULL, FALSE),
			array(FALSE, FALSE),
		);
	}

	/**
	 * Tests Valid::color()
	 *
	 * @test
	 * @dataProvider provider_color
	 * @param string  $color     The color to test
	 * @param boolean $expected  Is $color valid
	 */
	public function test_color($color, $expected)
	{
		$this->assertSame(
			$expected,
			Valid::color($color)
		);
	}

	/**
	 * Provides test data for test_credit_card()
	 */
	public function provider_credit_card()
	{
		return array(
			array('4222222222222',    'visa',       TRUE),
			array('4012888888881881', 'visa',       TRUE),
			array('4012888888881881', NULL,         TRUE),
			array('4012888888881881', array('mastercard', 'visa'), TRUE),
			array('4012888888881881', array('discover', 'mastercard'), FALSE),
			array('4012888888881881', 'mastercard', FALSE),
			array('5105105105105100', 'mastercard', TRUE),
			array('6011111111111117', 'discover',   TRUE),
			array('6011111111111117', 'visa',       FALSE),
			// Empty test
			array('', NULL, FALSE),
			array(NULL, NULL, FALSE),
			array(FALSE, NULL, FALSE),
		);
	}

	/**
	 * Tests Valid::credit_card()
	 *
	 * @test
	 * @covers Valid::credit_card
	 * @dataProvider  provider_credit_card()
	 * @param string  $number   Credit card number
	 * @param string  $type	    Credit card type
	 * @param boolean $expected
	 */
	public function test_credit_card($number, $type, $expected)
	{
		$this->assertSame(
			$expected,
			Valid::credit_card($number, $type)
		);
	}

	/**
	 * Provides test data for test_credit_card()
	 */
	public function provider_luhn()
	{
		return array(
			array('4222222222222', TRUE),
			array('4012888888881881', TRUE),
			array('5105105105105100', TRUE),
			array('6011111111111117', TRUE),
			array('60111111111111.7', FALSE),
			array('6011111111111117X', FALSE),
			array('6011111111111117 ', FALSE),
			array('WORD ', FALSE),
			// Empty test
			array('', FALSE),
			array(NULL, FALSE),
			array(FALSE, FALSE),
		);
	}

	/**
	 * Tests Valid::luhn()
	 *
	 * @test
	 * @covers Valid::luhn
	 * @dataProvider  provider_luhn()
	 * @param string  $number   Credit card number
	 * @param boolean $expected
	 */
	public function test_luhn($number, $expected)
	{
		$this->assertSame(
			$expected,
			Valid::luhn($number)
		);
	}

	/**
	 * Provides test data for test_email()
	 *
	 * @return array
	 */
	public function provider_email()
	{
		return array(
			array('foo', TRUE,  FALSE),
			array('foo', FALSE, FALSE),

			array('foo@bar', TRUE, TRUE),
			// RFC is less strict than the normal regex, presumably to allow
			//  admin@localhost, therefore we IGNORE IT!!!
			array('foo@bar', FALSE, FALSE),
			array('foo@bar.com', FALSE, TRUE),
			array('foo@barcom:80', FALSE, FALSE),
			array('foo@bar.sub.com', FALSE, TRUE),
			array('foo+asd@bar.sub.com', FALSE, TRUE),
			array('foo.asd@bar.sub.com', FALSE, TRUE),
			// RFC says 254 length max #4011
			array(Text::random(NULL, 200).'@'.Text::random(NULL, 50).'.com', FALSE, FALSE),
			// Empty test
			array('', TRUE, FALSE),
			array(NULL, TRUE, FALSE),
			array(FALSE, TRUE, FALSE),
		);
	}

	/**
	 * Tests Valid::email()
	 *
	 * Check an email address for correct format.
	 *
	 * @test
	 * @dataProvider provider_email
	 * @param string  $email   Address to check
	 * @param boolean $strict  Use strict settings
	 * @param boolean $correct Is $email address valid?
	 */
	public function test_email($email, $strict, $correct)
	{
		$this->assertSame(
			$correct,
			Valid::email($email, $strict)
		);
	}

	/**
	 * Returns test data for test_email_domain()
	 *
	 * @return array
	 */
	public function provider_email_domain()
	{
		return array(
			array('google.com', TRUE),
			// Don't anybody dare register this...
			array('DAWOMAWIDAIWNDAIWNHDAWIHDAIWHDAIWOHDAIOHDAIWHD.com', FALSE),
			// Empty test
			array('', FALSE),
			array(NULL, FALSE),
			array(FALSE, FALSE),
		);
	}

	/**
	 * Tests Valid::email_domain()
	 *
	 * Validate the domain of an email address by checking if the domain has a
	 * valid MX record.
	 *
	 * Test skips on windows
	 *
	 * @test
	 * @dataProvider provider_email_domain
	 * @param string  $email   Email domain to check
	 * @param boolean $correct Is it correct?
	 */
	public function test_email_domain($email, $correct)
	{
		if ( ! $this->hasInternet())
		{
			$this->markTestSkipped('An internet connection is required for this test');
		}

		if ( ! Kohana::$is_windows OR version_compare(PHP_VERSION, '5.3.0', '>='))
		{
			$this->assertSame(
				$correct,
				Valid::email_domain($email)
			);
		}
		else
		{
			$this->markTestSkipped('checkdnsrr() was not added on windows until PHP 5.3');
		}
	}

	/**
	 * Provides data for test_exact_length()
	 *
	 * @return array
	 */
	public function provider_exact_length()
	{
		return array(
			array('somestring', 10, TRUE),
			array('somestring', 11, FALSE),
			array('anotherstring', 13, TRUE),
			// Empty test
			array('', 10, FALSE),
			array(NULL, 10, FALSE),
			array(FALSE, 10, FALSE),
			// Test array of allowed lengths
			array('somestring', array(1, 3, 5, 7, 9, 10), TRUE),
			array('somestring', array(1, 3, 5, 7, 9), FALSE),
		);
	}

	/**
	 *
	 * Tests Valid::exact_length()
	 *
	 * Checks that a field is exactly the right length.
	 *
	 * @test
	 * @dataProvider provider_exact_length
	 * @param string  $string  The string to length check
	 * @param integer $length  The length of the string
	 * @param boolean $correct Is $length the actual length of the string?
	 * @return bool
	 */
	public function test_exact_length($string, $length, $correct)
	{
		return $this->assertSame(
			$correct,
			Valid::exact_length($string, $length),
			'Reported string length is not correct'
		);
	}

	/**
	 * Provides data for test_equals()
	 *
	 * @return array
	 */
	public function provider_equals()
	{
		return array(
			array('foo', 'foo', TRUE),
			array('1', '1', TRUE),
			array(1, '1', FALSE),
			array('011', 011, FALSE),
			// Empty test
			array('', 123, FALSE),
			array(NULL, 123, FALSE),
			array(FALSE, 123, FALSE),
		);
	}

	/**
	 * Tests Valid::equals()
	 *
	 * @test
	 * @dataProvider provider_equals
	 * @param   string   $string    value to check
	 * @param   integer  $required  required value
	 * @param   boolean  $correct   is $string the same as $required?
	 * @return  boolean
	 */
	public function test_equals($string, $required, $correct)
	{
		return $this->assertSame(
			$correct,
			Valid::equals($string, $required),
			'Values are not equal'
		);
	}

	/**
	 * DataProvider for the valid::ip() test
	 * @return array
	 */
	public function provider_ip()
	{
		return array(
			array('75.125.175.50',   FALSE, TRUE),
			// PHP 5.3.6 fixed a bug that allowed 127.0.0.1 as a public ip: http://bugs.php.net/53150
			array('127.0.0.1',       FALSE, version_compare(PHP_VERSION, '5.3.6', '<')),
			array('256.257.258.259', FALSE, FALSE),
			array('255.255.255.255', FALSE, FALSE),
			array('192.168.0.1',     FALSE, FALSE),
			array('192.168.0.1',     TRUE,  TRUE),
			// Empty test
			array('', TRUE, FALSE),
			array(NULL, TRUE, FALSE),
			array(FALSE, TRUE, FALSE),
		);
	}

	/**
	 * Tests Valid::ip()
	 *
	 * @test
	 * @dataProvider  provider_ip
	 * @param string  $input_ip
	 * @param boolean $allow_private
	 * @param boolean $expected_result
	 */
	public function test_ip($input_ip, $allow_private, $expected_result)
	{
		$this->assertEquals(
			$expected_result,
			Valid::ip($input_ip, $allow_private)
		);
	}

	/**
	 * Returns test data for test_max_length()
	 *
	 * @return array
	 */
	public function provider_max_length()
	{
		return array(
			// Border line
			array('some', 4, TRUE),
			// Exceeds
			array('KOHANARULLLES', 2, FALSE),
			// Under
			array('CakeSucks', 10, TRUE),
			// Empty test
			array('', -10, FALSE),
			array(NULL, -10, FALSE),
			array(FALSE, -10, FALSE),
		);
	}

	/**
	 * Tests Valid::max_length()
	 *
	 * Checks that a field is short enough.
	 *
	 * @test
	 * @dataProvider provider_max_length
	 * @param string  $string    String to test
	 * @param integer $maxlength Max length for this string
	 * @param boolean $correct   Is $string <= $maxlength
	 */
	public function test_max_length($string, $maxlength, $correct)
	{
		 $this->assertSame(
			$correct,
			Valid::max_length($string, $maxlength)
		);
	}

	/**
	 * Returns test data for test_min_length()
	 *
	 * @return array
	 */
	public function provider_min_length()
	{
		return array(
			array('This is obviously long enough', 10, TRUE),
			array('This is not', 101, FALSE),
			array('This is on the borderline', 25, TRUE),
			// Empty test
			array('', 10, FALSE),
			array(NULL, 10, FALSE),
			array(FALSE, 10, FALSE),
		);
	}

	/**
	 * Tests Valid::min_length()
	 *
	 * Checks that a field is long enough.
	 *
	 * @test
	 * @dataProvider provider_min_length
	 * @param string  $string     String to compare
	 * @param integer $minlength  The minimum allowed length
	 * @param boolean $correct    Is $string 's length >= $minlength
	 */
	public function test_min_length($string, $minlength, $correct)
	{
		$this->assertSame(
			$correct,
			Valid::min_length($string, $minlength)
		);
	}

	/**
	 * Returns test data for test_not_empty()
	 *
	 * @return array
	 */
	public function provider_not_empty()
	{
		// Create a blank arrayObject
		$ao = new ArrayObject;

		// arrayObject with value
		$ao1 = new ArrayObject;
		$ao1['test'] = 'value';

		return array(
			array(array(),      FALSE),
			array(NULL,         FALSE),
			array('',           FALSE),
			array($ao,          FALSE),
			array($ao1,         TRUE),
			array(array(NULL),  TRUE),
			array(0,            TRUE),
			array('0',          TRUE),
			array('Something',  TRUE),
		);
	}

	/**
	 * Tests Valid::not_empty()
	 *
	 * Checks if a field is not empty.
	 *
	 * @test
	 * @dataProvider provider_not_empty
	 * @param mixed   $value  Value to check
	 * @param boolean $empty  Is the value really empty?
	 */
	public function test_not_empty($value, $empty)
	{
		return $this->assertSame(
			$empty,
			Valid::not_empty($value)
		);
	}

	/**
	 * DataProvider for the Valid::numeric() test
	 */
	public function provider_numeric()
	{
		return array(
			array(12345,   TRUE),
			array(123.45,  TRUE),
			array('12345', TRUE),
			array('10.5',  TRUE),
			array('-10.5', TRUE),
			array('10.5a', FALSE),
			// @issue 3240
			array(.4,      TRUE),
			array(-.4,     TRUE),
			array(4.,      TRUE),
			array(-4.,     TRUE),
			array('.5',    TRUE),
			array('-.5',   TRUE),
			array('5.',    TRUE),
			array('-5.',   TRUE),
			array('.',     FALSE),
			array('1.2.3', FALSE),
			// Empty test
			array('', FALSE),
			array(NULL, FALSE),
			array(FALSE, FALSE),
		);
	}

	/**
	 * Tests Valid::numeric()
	 *
	 * @test
	 * @dataProvider provider_numeric
	 * @param string  $input     Input to test
	 * @param boolean $expected  Whether or not $input is numeric
	 */
	public function test_numeric($input, $expected)
	{
		$this->assertSame(
			$expected,
			Valid::numeric($input)
		);
	}

	/**
	 * Provides test data for test_phone()
	 * @return array
	 */
	public function provider_phone()
	{
		return array(
			array('0163634840',       NULL, TRUE),
			array('+27173634840',     NULL, TRUE),
			array('123578',           NULL, FALSE),
			// Some uk numbers
			array('01234456778',      NULL, TRUE),
			array('+0441234456778',   NULL, FALSE),
			// Google UK case you're interested
			array('+44 20-7031-3000', array(12), TRUE),
			// BT Corporate
			array('020 7356 5000',	  NULL, TRUE),
			// Empty test
			array('', NULL, FALSE),
			array(NULL, NULL, FALSE),
			array(FALSE, NULL, FALSE),
		);
	}

	/**
	 * Tests Valid::phone()
	 *
	 * @test
	 * @dataProvider  provider_phone
	 * @param string  $phone     Phone number to test
	 * @param boolean $expected  Is $phone valid
	 */
	public function test_phone($phone, $lengths, $expected)
	{
		$this->assertSame(
			$expected,
			Valid::phone($phone, $lengths)
		);
	}

	/**
	 * DataProvider for the valid::regex() test
	 */
	public function provider_regex()
	{
		return array(
			array('hello world', '/[a-zA-Z\s]++/', TRUE),
			array('123456789', '/[0-9]++/', TRUE),
			array('£$%£%', '/[abc]/', FALSE),
			array('Good evening',  '/hello/',  FALSE),
			// Empty test
			array('', '/hello/', FALSE),
			array(NULL, '/hello/', FALSE),
			array(FALSE, '/hello/', FALSE),
		);
	}

	/**
	 * Tests Valid::range()
	 *
	 * Tests if a number is within a range.
	 *
	 * @test
	 * @dataProvider provider_regex
	 * @param string $value Value to test against
	 * @param string $regex Valid pcre regular expression
	 * @param bool $expected Does the value match the expression?
	 */
	public function test_regex($value, $regex, $expected)
	{
		$this->AssertSame(
			$expected,
			Valid::regex($value, $regex)
		);
	}

	/**
	 * DataProvider for the valid::range() test
	 */
	public function provider_range()
	{
		return array(
			array(1,  0,  2, NULL, TRUE),
			array(-1, -5, 0, NULL, TRUE),
			array(-1, 0,  1, NULL, FALSE),
			array(1,  0,  0, NULL, FALSE),
			array(2147483647, 0, 200000000000000, NULL, TRUE),
			array(-2147483647, -2147483655, 2147483645, NULL, TRUE),
			// #4043
			array(2, 0, 10, 2, TRUE),
			array(3, 0, 10, 2, FALSE),
			// Empty test
			array('', 5, 10, NULL, FALSE),
			array(NULL, 5, 10, NULL, FALSE),
			array(FALSE, 5, 10, NULL, FALSE),
		);
	}

	/**
	 * Tests Valid::range()
	 *
	 * Tests if a number is within a range.
	 *
	 * @test
	 * @dataProvider provider_range
	 * @param integer $number    Number to test
	 * @param integer $min       Lower bound
	 * @param integer $max       Upper bound
	 * @param boolean $expected  Is Number within the bounds of $min && $max
	 */
	public function test_range($number, $min, $max, $step, $expected)
	{
		$this->AssertSame(
			$expected,
			Valid::range($number, $min, $max, $step)
		);
	}

	/**
	 * Provides test data for test_url()
	 *
	 * @return array
	 */
	public function provider_url()
	{
		$data = array(
			array('http://google.com', TRUE),
			array('http://google.com/', TRUE),
			array('http://google.com/?q=abc', TRUE),
			array('http://google.com/#hash', TRUE),
			array('http://localhost', TRUE),
			array('http://hello-world.pl', TRUE),
			array('http://hello--world.pl', TRUE),
			array('http://h.e.l.l.0.pl', TRUE),
			array('http://server.tld/get/info', TRUE),
			array('http://127.0.0.1', TRUE),
			array('http://127.0.0.1:80', TRUE),
			array('http://user@127.0.0.1', TRUE),
			array('http://user:pass@127.0.0.1', TRUE),
			array('ftp://my.server.com', TRUE),
			array('rss+xml://rss.example.com', TRUE),

			array('http://google.2com', FALSE),
			array('http://google.com?q=abc', FALSE),
			array('http://google.com#hash', FALSE),
			array('http://hello-.pl', FALSE),
			array('http://hel.-lo.world.pl', FALSE),
			array('http://ww£.google.com', FALSE),
			array('http://127.0.0.1234', FALSE),
			array('http://127.0.0.1.1', FALSE),
			array('http://user:@127.0.0.1', FALSE),
			array("http://finalnewline.com\n", FALSE),
			// Empty test
			array('', FALSE),
			array(NULL, FALSE),
			array(FALSE, FALSE),
		);

		$data[] = array('http://'.str_repeat('123456789.', 25).'com/', TRUE); // 253 chars
		$data[] = array('http://'.str_repeat('123456789.', 25).'info/', FALSE); // 254 chars

		return $data;
	}

	/**
	 * Tests Valid::url()
	 *
	 * @test
	 * @dataProvider provider_url
	 * @param string  $url       The url to test
	 * @param boolean $expected  Is it valid?
	 */
	public function test_url($url, $expected)
	{
		$this->assertSame(
			$expected,
			Valid::url($url)
		);
	}

	/**
	 * DataProvider for the valid::matches() test
	 */
	public function provider_matches()
	{
		return array(
			array(array('a' => 'hello', 'b' => 'hello'), 'a', 'b', TRUE),
			array(array('a' => 'hello', 'b' => 'hello '), 'a', 'b', FALSE),
			array(array('a' => '1', 'b' => 1), 'a', 'b', FALSE),
			// Empty test
			array(array('a' => '', 'b' => 'hello'), 'a', 'b', FALSE),
			array(array('a' => NULL, 'b' => 'hello'), 'a', 'b', FALSE),
			array(array('a' => FALSE, 'b' => 'hello'), 'a', 'b', FALSE),
		);
	}

	/**
	 * Tests Valid::matches()
	 *
	 * Tests if a field matches another from an array of data
	 *
	 * @test
	 * @dataProvider provider_matches
	 * @param array   $data      Array of fields
	 * @param integer $field     First field name
	 * @param integer $match     Field name that must match $field in $data
	 * @param boolean $expected  Do the two fields match?
	 */
	public function test_matches($data, $field, $match, $expected)
	{
		$this->AssertSame(
			$expected,
			Valid::matches($data, $field, $match)
		);
	}
}
