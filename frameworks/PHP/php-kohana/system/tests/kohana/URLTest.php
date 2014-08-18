<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Tests URL
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.url
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @author     BRMatt <matthew@sigswitch.com>
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_URLTest extends Unittest_TestCase
{
	/**
	 * Default values for the environment, see setEnvironment
	 * @var array
	 */
	// @codingStandardsIgnoreStart
	protected $environmentDefault =	array(
		'Kohana::$base_url'	=> '/kohana/',
		'Kohana::$index_file'=> 'index.php',
		'HTTP_HOST' => 'example.com',
		'_GET'		=> array(),
	);
	// @codingStandardsIgnoreEnd

	/**
	 * Provides test data for test_base()
	 *
	 * @return array
	 */
	public function provider_base()
	{
		return array(
			// $protocol, $index, $expected, $enviroment

			// Test with different combinations of parameters for max code coverage
			array(NULL,    FALSE, '/kohana/'),
			array('http',  FALSE, 'http://example.com/kohana/'),
			array(NULL,    TRUE,  '/kohana/index.php/'),
			array(NULL,    TRUE,  '/kohana/index.php/'),
			array('http',  TRUE,  'http://example.com/kohana/index.php/'),
			array('https', TRUE,  'https://example.com/kohana/index.php/'),
			array('ftp',   TRUE,  'ftp://example.com/kohana/index.php/'),

			// Test for automatic protocol detection, protocol = TRUE
			array(TRUE,    TRUE,  'cli://example.com/kohana/index.php/', array('HTTPS' => FALSE, 'Request::$initial' => Request::factory('/')->protocol('cli'))),

			// Change base url'
			array('https', FALSE, 'https://example.com/kohana/', array('Kohana::$base_url' => 'omglol://example.com/kohana/')),

			// Use port in base url, issue #3307
			array('http', FALSE, 'http://example.com:8080/', array('Kohana::$base_url' => 'example.com:8080/')),

			// Use protocol from base url if none specified
			array(NULL,  FALSE, 'http://www.example.com/', array('Kohana::$base_url' => 'http://www.example.com/')),

			// Use HTTP_HOST before SERVER_NAME
			array('http', FALSE, 'http://example.com/kohana/', array('HTTP_HOST' => 'example.com', 'SERVER_NAME' => 'example.org')),

			// Use SERVER_NAME if HTTP_HOST DNX
			array('http',  FALSE, 'http://example.org/kohana/', array('HTTP_HOST' => NULL, 'SERVER_NAME' => 'example.org')),
		);
	}

	/**
	 * Tests URL::base()
	 *
	 * @test
	 * @dataProvider provider_base
	 * @param boolean $protocol    Parameter for Url::base()
	 * @param boolean $index       Parameter for Url::base()
	 * @param string  $expected    Expected url
	 * @param array   $enviroment  Array of enviroment vars to change @see Kohana_URLTest::setEnvironment()
	 */
	public function test_base($protocol, $index, $expected, array $enviroment = array())
	{
		$this->setEnvironment($enviroment);

		$this->assertSame(
			$expected,
			URL::base($protocol, $index)
		);
	}

	/**
	 * Provides test data for test_site()
	 *
	 * @return array
	 */
	public function provider_site()
	{
		return array(
			array('', NULL,		'/kohana/index.php/'),
			array('', 'http',			'http://example.com/kohana/index.php/'),

			array('my/site', NULL, '/kohana/index.php/my/site'),
			array('my/site', 'http',  'http://example.com/kohana/index.php/my/site'),

			// @ticket #3110
			array('my/site/page:5', NULL, '/kohana/index.php/my/site/page:5'),
			array('my/site/page:5', 'http', 'http://example.com/kohana/index.php/my/site/page:5'),

			array('my/site?var=asd&kohana=awesome', NULL,  '/kohana/index.php/my/site?var=asd&kohana=awesome'),
			array('my/site?var=asd&kohana=awesome', 'http',  'http://example.com/kohana/index.php/my/site?var=asd&kohana=awesome'),

			array('?kohana=awesome&life=good', NULL, '/kohana/index.php/?kohana=awesome&life=good'),
			array('?kohana=awesome&life=good', 'http', 'http://example.com/kohana/index.php/?kohana=awesome&life=good'),

			array('?kohana=awesome&life=good#fact', NULL, '/kohana/index.php/?kohana=awesome&life=good#fact'),
			array('?kohana=awesome&life=good#fact', 'http', 'http://example.com/kohana/index.php/?kohana=awesome&life=good#fact'),

			array('some/long/route/goes/here?kohana=awesome&life=good#fact', NULL, '/kohana/index.php/some/long/route/goes/here?kohana=awesome&life=good#fact'),
			array('some/long/route/goes/here?kohana=awesome&life=good#fact', 'http', 'http://example.com/kohana/index.php/some/long/route/goes/here?kohana=awesome&life=good#fact'),

			array('/route/goes/here?kohana=awesome&life=good#fact', 'https', 'https://example.com/kohana/index.php/route/goes/here?kohana=awesome&life=good#fact'),
			array('/route/goes/here?kohana=awesome&life=good#fact', 'ftp', 'ftp://example.com/kohana/index.php/route/goes/here?kohana=awesome&life=good#fact'),
		);
	}

	/**
	 * Tests URL::site()
	 *
	 * @test
	 * @dataProvider provider_site
	 * @param string          $uri         URI to use
	 * @param boolean|string  $protocol    Protocol to use
	 * @param string          $expected    Expected result
	 * @param array           $enviroment  Array of enviroment vars to set
	 */
	public function test_site($uri, $protocol, $expected, array $enviroment = array())
	{
		$this->setEnvironment($enviroment);

		$this->assertSame(
			$expected,
			URL::site($uri, $protocol)
		);
	}

	/**
	 * Provides test data for test_site_url_encode_uri()
	 * See issue #2680
	 *
	 * @return array
	 */
	public function provider_site_url_encode_uri()
	{
		$provider = array(
			array('test', 'encode'),
			array('test', 'éñçø∂ë∂'),
			array('†éß†', 'encode'),
			array('†éß†', 'éñçø∂ë∂', 'µåñ¥'),
		);

		foreach ($provider as $i => $params)
		{
			// Every non-ASCII character except for forward slash should be encoded...
			$expected = implode('/', array_map('rawurlencode', $params));

			// ... from a URI that is not encoded
			$uri = implode('/', $params);

			$provider[$i] = array("/kohana/index.php/{$expected}", $uri);
		}

		return $provider;
	}

	/**
	 * Tests URL::site for proper URL encoding when working with non-ASCII characters.
	 *
	 * @test
	 * @dataProvider provider_site_url_encode_uri
	 */
	public function test_site_url_encode_uri($expected, $uri)
	{
		$this->assertSame($expected, URL::site($uri, FALSE));
	}

	/**
	 * Provides test data for test_title()
	 * @return array
	 */
	public function provider_title()
	{
		return array(
			// Tests that..
			// Title is converted to lowercase
			array('we-shall-not-be-moved', 'WE SHALL NOT BE MOVED', '-'),
			// Excessive white space is removed and replaced with 1 char
			array('thissssss-is-it', 'THISSSSSS         IS       IT  ', '-'),
			// separator is either - (dash) or _ (underscore) & others are converted to underscores
			array('some-title', 'some title', '-'),
			array('some_title', 'some title', '_'),
			array('some!title', 'some title', '!'),
			array('some:title', 'some title', ':'),
			// Numbers are preserved
			array('99-ways-to-beat-apple', '99 Ways to beat apple', '-'),
			// ... with lots of spaces & caps
			array('99_ways_to_beat_apple', '99    ways   TO beat      APPLE', '_'),
			array('99-ways-to-beat-apple', '99    ways   TO beat      APPLE', '-'),
			// Invalid characters are removed
			array('each-gbp-is-now-worth-32-usd', 'Each GBP(£) is now worth 32 USD($)', '-'),
			// ... inc. separator
			array('is-it-reusable-or-re-usable', 'Is it reusable or re-usable?', '-'),
			// Doing some crazy UTF8 tests
			array('espana-wins', 'España-wins', '-', TRUE),
		);
	}

	/**
	 * Tests URL::title()
	 *
	 * @test
	 * @dataProvider provider_title
	 * @param string $title        Input to convert
	 * @param string $separator    Seperate to replace invalid characters with
	 * @param string $expected     Expected result
	 */
	public function test_title($expected, $title, $separator, $ascii_only = FALSE)
	{
		$this->assertSame(
			$expected,
			URL::title($title, $separator, $ascii_only)
		);
	}

	/**
	 * Provides test data for URL::query()
	 * @return array
	 */
	public function provider_query()
	{
		return array(
			array(array(), '', NULL),
			array(array('_GET' => array('test' => 'data')), '?test=data', NULL),
			array(array(), '?test=data', array('test' => 'data')),
			array(array('_GET' => array('more' => 'data')), '?more=data&test=data', array('test' => 'data')),
			array(array('_GET' => array('sort' => 'down')), '?test=data', array('test' => 'data'), FALSE),

			// http://dev.kohanaframework.org/issues/3362
			array(array(), '', array('key' => NULL)),
			array(array(), '?key=0', array('key' => FALSE)),
			array(array(), '?key=1', array('key' => TRUE)),
			array(array('_GET' => array('sort' => 'down')), '?sort=down&key=1', array('key' => TRUE)),
			array(array('_GET' => array('sort' => 'down')), '?sort=down&key=0', array('key' => FALSE)),

			// @issue 4240
			array(array('_GET' => array('foo' => array('a' => 100))), '?foo%5Ba%5D=100&foo%5Bb%5D=bar', array('foo' => array('b' => 'bar'))),
			array(array('_GET' => array('a' => 'a')), '?a=b', array('a' => 'b')),
		);
	}

	/**
	 * Tests URL::query()
	 *
	 * @test
	 * @dataProvider provider_query
	 * @param array $enviroment Set environment
	 * @param string $expected Expected result
	 * @param array $params Query string
	 * @param boolean $use_get Combine with GET parameters
	 */
	public function test_query($enviroment, $expected, $params, $use_get = TRUE)
	{
		$this->setEnvironment($enviroment);

		$this->assertSame(
			$expected,
			URL::query($params, $use_get)
		);
	}
}
