<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Unit tests for generic Request_Client class
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.request
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @author	   Andrew Coulton
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_Request_ClientTest extends Unittest_TestCase
{
	protected $_inital_request;
	protected static $_original_routes;

	// @codingStandardsIgnoreStart - PHPUnit does not follow standards
	/**
	 * Sets up a new route to ensure that we have a matching route for our
	 * Controller_RequestClientDummy class.
	 */
	public static function setUpBeforeClass()
	{
		// @codingStandardsIgnoreEnd
		parent::setUpBeforeClass();

		// Set a new Route to the ClientTest controller as the first route
		// This requires reflection as the API for editing defined routes is limited
		$route_class = new ReflectionClass('Route');
		$routes_prop = $route_class->getProperty('_routes');
		$routes_prop->setAccessible(TRUE);

		self::$_original_routes = $routes_prop->getValue('Route');

		$routes = array(
			'ko_request_clienttest' => new Route('<controller>/<action>/<data>',array('data'=>'.+'))
		) + self::$_original_routes;

		$routes_prop->setValue('Route',$routes);

	}

	// @codingStandardsIgnoreStart - PHPUnit does not follow standards
	/**
	 * Resets the application's routes to their state prior to this test case
	 */
	public static function tearDownAfterClass()
	{
		// @codingStandardsIgnoreEnd
		// Reset routes
		$route_class = new ReflectionClass('Route');
		$routes_prop = $route_class->getProperty('_routes');
		$routes_prop->setAccessible(TRUE);
		$routes_prop->setValue('Route',self::$_original_routes);

		parent::tearDownAfterClass();
	}

	// @codingStandardsIgnoreStart - PHPUnit does not follow standards
	public function setUp()
	{
		// @codingStandardsIgnoreEnd
		parent::setUp();
		$this->_initial_request = Request::$initial;
		Request::$initial = new Request('/');
	}

	// @codingStandardsIgnoreStart - PHPUnit does not follow standards
	public function tearDown()
	{
		// @codingStandardsIgnoreEnd
		Request::$initial = $this->_initial_request;
		parent::tearDown();
	}

	/**
	 * Generates an internal URI to the [Controller_RequestClientDummy] shunt
	 * controller - the URI contains an encoded form of the required server
	 * response.
	 *
	 * @param string $status  HTTP response code to issue
	 * @param array $headers  HTTP headers to send with the response
	 * @param string $body    A string to send back as response body (included in the JSON response)
	 * @return string
	 */
	protected function _dummy_uri($status, $headers, $body)
	{
		$data = array(
			'status' => $status,
			'header' => $headers,
			'body'   => $body
		);
		return "/requestclientdummy/fake".'/'.urlencode(http_build_query($data));
	}

	/**
	 * Shortcut method to generate a simple redirect URI - the first request will
	 * receive a redirect with the given HTTP status code and the second will
	 * receive a 200 response. The 'body' data value in the first response will
	 * be 'not-followed' and in the second response it will be 'followed'. This
	 * allows easy assertion that a redirect has taken place.
	 *
	 * @param string $status  HTTP response code to issue
	 * @return string
	 */
	protected function _dummy_redirect_uri($status)
	{
		return $this->_dummy_uri($status,
			array('Location' => $this->_dummy_uri(200, NULL, 'followed')),
			'not-followed');
	}

	/**
	 * Provider for test_follows_redirects
	 * @return array
	 */
	public function provider_follows_redirects()
	{
		return array(
			array(TRUE, $this->_dummy_uri(200, NULL, 'not-followed'), 'not-followed'),
			array(TRUE, $this->_dummy_redirect_uri(200), 'not-followed'),
			array(TRUE, $this->_dummy_redirect_uri(302), 'followed'),
			array(FALSE, $this->_dummy_redirect_uri(302), 'not-followed'),
		);
	}

	/**
	 * Tests that the client optionally follows properly formed redirects
	 *
	 * @dataProvider provider_follows_redirects
	 *
	 * @param  bool   $follow           Option value to set
	 * @param  string $request_url      URL to request initially (contains data to set up redirect etc)
	 * @param  string $expect_body      Body text expected in the eventual result
	 */
	public function test_follows_redirects($follow, $request_url, $expect_body)
	{
		$response = Request::factory($request_url,
			array('follow' => $follow))
			->execute();

		$data = json_decode($response->body(), TRUE);
		$this->assertEquals($expect_body, $data['body']);
	}

	/**
	 * Tests that only specified headers are resent following a redirect
	 */
	public function test_follows_with_headers()
	{
		$response = Request::factory(
			$this->_dummy_redirect_uri(301),
			array(
				'follow' => TRUE,
				'follow_headers' => array('Authorization', 'X-Follow-With-Value')
			))
			->headers(array(
				'Authorization' => 'follow',
				'X-Follow-With-Value' => 'follow',
				'X-Not-In-Follow' => 'no-follow'
			))
			->execute();

		$data = json_decode($response->body(),TRUE);
		$headers = $data['rq_headers'];

		$this->assertEquals('followed', $data['body']);
		$this->assertEquals('follow', $headers['authorization']);
		$this->assertEquals('follow', $headers['x-follow-with-value']);
		$this->assertFalse(isset($headers['x-not-in-follow']), 'X-Not-In-Follow should not be passed to next request');
	}

	/**
	 * Provider for test_follows_with_strict_method
	 *
	 * @return array
	 */
	public function provider_follows_with_strict_method()
	{
		return array(
			array(201, NULL, Request::POST, Request::GET),
			array(301, NULL, Request::GET, Request::GET),
			array(302, TRUE, Request::POST, Request::POST),
			array(302, FALSE, Request::POST, Request::GET),
			array(303, NULL, Request::POST, Request::GET),
			array(307, NULL, Request::POST, Request::POST),
		);
	}

	/**
	 * Tests that the correct method is used (allowing for the strict_redirect setting)
	 * for follow requests.
	 *
	 * @dataProvider provider_follows_with_strict_method
	 *
	 * @param string $status_code   HTTP response code to fake
	 * @param bool   $strict_redirect Option value to set
	 * @param string $orig_method   Request method for the original request
	 * @param string $expect_method Request method expected for the follow request
	 */
	public function test_follows_with_strict_method($status_code, $strict_redirect, $orig_method, $expect_method)
	{
		$response = Request::factory($this->_dummy_redirect_uri($status_code),
			array(
				'follow' => TRUE,
				'strict_redirect' => $strict_redirect
			))
			->method($orig_method)
			->execute();

		$data = json_decode($response->body(), TRUE);

		$this->assertEquals('followed', $data['body']);
		$this->assertEquals($expect_method, $data['rq_method']);
	}

	/**
	 * Provider for test_follows_with_body_if_not_get
	 *
	 * @return array
	 */
	public function provider_follows_with_body_if_not_get()
	{
		return array(
			array('GET','301',NULL),
			array('POST','303',NULL),
			array('POST','307','foo-bar')
		);
	}

	/**
	 * Tests that the original request body is sent when following a redirect
	 * (unless redirect method is GET)
	 *
	 * @dataProvider provider_follows_with_body_if_not_get
	 * @depends test_follows_with_strict_method
	 * @depends test_follows_redirects
	 *
	 * @param string $original_method  Request method to use for the original request
	 * @param string $status  Redirect status that will be issued
	 * @param string $expect_body      Expected value of body() in the second request
	 */
	public function test_follows_with_body_if_not_get($original_method, $status, $expect_body)
	{
		$response = Request::factory($this->_dummy_redirect_uri($status),
			array('follow' => TRUE))
			->method($original_method)
			->body('foo-bar')
			->execute();

		$data = json_decode($response->body(), TRUE);

		$this->assertEquals('followed', $data['body']);
		$this->assertEquals($expect_body, $data['rq_body']);
	}

	/**
	 * Provider for test_triggers_header_callbacks
	 *
	 * @return array
	 */
	public function provider_triggers_header_callbacks()
	{
		return array(
			// Straightforward response manipulation
			array(
				array('X-test-1' =>
					function($request, $response, $client)
					{
						$response->body(json_encode(array('body'=>'test1-body-changed')));
						return $response;
				}),
				$this->_dummy_uri(200, array('X-test-1' => 'foo'), 'test1-body'),
				'test1-body-changed'
			),
			// Subsequent request execution
			array(
				array('X-test-2' =>
					function($request, $response, $client)
					{
						return Request::factory($response->headers('X-test-2'));
				}),
				$this->_dummy_uri(200,
					array('X-test-2' => $this->_dummy_uri(200, NULL, 'test2-subsequent-body')),
					'test2-orig-body'),
				'test2-subsequent-body'
			),
			// No callbacks triggered
			array(
				array('X-test-3' =>
					function ($request, $response, $client)
					{
						throw new Exception("Unexpected execution of X-test-3 callback");
				}),
				$this->_dummy_uri(200, array('X-test-1' => 'foo'), 'test3-body'),
				'test3-body'
			),
			// Callbacks not triggered once a previous callback has created a new response
			array(
				array(
					'X-test-1' =>
						function($request, $response, $client)
						{
							return Request::factory($response->headers('X-test-1'));
						},
					'X-test-2' =>
						function($request, $response, $client)
						{
							return Request::factory($response->headers('X-test-2'));
						}
				),
				$this->_dummy_uri(200,
					array(
						'X-test-1' => $this->_dummy_uri(200, NULL, 'test1-subsequent-body'),
						'X-test-2' => $this->_dummy_uri(200, NULL, 'test2-subsequent-body')
					),
					'test2-orig-body'),
				'test1-subsequent-body'
			),
			// Nested callbacks are supported if callback creates new request
			array(
				array(
					'X-test-1' =>
						function($request, $response, $client)
						{
							return Request::factory($response->headers('X-test-1'));
						},
					'X-test-2' =>
						function($request, $response, $client)
						{
							return Request::factory($response->headers('X-test-2'));
						}
				),
				$this->_dummy_uri(200,
					array(
						'X-test-1' => $this->_dummy_uri(
							200,
							array('X-test-2' => $this->_dummy_uri(200, NULL, 'test2-subsequent-body')),
							'test1-subsequent-body'),
					),
					'test-orig-body'),
				'test2-subsequent-body'
			),
		);
	}

	/**
	 * Tests that header callbacks are triggered in sequence when specific headers
	 * are present in the response
	 *
	 * @dataProvider provider_triggers_header_callbacks
	 *
	 * @param array $callbacks     Array of header callbacks
	 * @param array  $headers      Headers that will be received in the response
	 * @param string $expect_body  Response body content to expect
	 */
	public function test_triggers_header_callbacks($callbacks, $uri, $expect_body)
	{
		$response = Request::factory($uri,
			array('header_callbacks' => $callbacks))
			->execute();

		$data = json_decode($response->body(), TRUE);

		$this->assertEquals($expect_body, $data['body']);
	}
	
	/**
	 * Tests that the Request_Client is protected from too many recursions of
	 * requests triggered by header callbacks.
	 *
	 */
	public function test_deep_recursive_callbacks_are_aborted()
	{
		$uri = $this->_dummy_uri('200', array('x-cb' => '1'), 'body');

		// Temporary property to track requests
		$this->requests_executed = 0;

		try
		{
			$response = Request::factory(
					$uri,
					array(
						'header_callbacks' => array(
							'x-cb' => 
								function ($request, $response, $client)
								{
									$client->callback_params('testcase')->requests_executed++;
									// Recurse into a new request
									return Request::factory($request->uri());
								}),
						'max_callback_depth' => 2,
						'callback_params' => array(
							'testcase' => $this,
						)
					))
					->execute();
		}
		catch (Request_Client_Recursion_Exception $e)
		{
			// Verify that two requests were executed
			$this->assertEquals(2, $this->requests_executed);
			return;
		}

		$this->fail('Expected Request_Client_Recursion_Exception was not thrown');
	}

	/**
	 * Header callback for testing that arbitrary callback_params are available
	 * to the callback.
	 *
	 * @param Request $request
	 * @param Response $response
	 * @param Request_Client $client
	 */
	public function callback_assert_params($request, $response, $client)
	{
		$this->assertEquals('foo', $client->callback_params('constructor_param'));
		$this->assertEquals('bar', $client->callback_params('setter_param'));
		$response->body('assertions_ran');
	}

	/**
	 * Test that arbitrary callback_params can be passed to the callback through
	 * the Request_Client and are assigned to subsequent requests
	 */
	public function test_client_can_hold_params_for_callbacks()
	{
		// Test with param in constructor
		$request = Request::factory(
				$this->_dummy_uri(
						302,
						array('Location' => $this->_dummy_uri('200',array('X-cb'=>'1'), 'followed')),
						'not-followed'),
				array(
					'follow' => TRUE,
					'header_callbacks' => array(
						'x-cb' => array($this, 'callback_assert_params'),
						'location' => 'Request_Client::on_header_location',
					),
					'callback_params' => array(
						'constructor_param' => 'foo'
					)
				));

		// Test passing param to setter
		$request->client()->callback_params('setter_param', 'bar');

		// Callback will throw assertion exceptions when executed
		$response = $request->execute();
		$this->assertEquals('assertions_ran', $response->body());
	}

} // End Kohana_Request_ClientTest


/**
 * Dummy controller class that acts as a shunt - passing back request information
 * in the response to allow inspection.
 */
class Controller_RequestClientDummy extends Controller {

	/**
	 * Takes a urlencoded 'data' parameter from the route and uses it to craft a
	 * response. Redirect chains can be tested by passing another encoded uri
	 * as a location header with an appropriate status code.
	 */
	public function action_fake()
	{
		parse_str(urldecode($this->request->param('data')), $data);
		$this->response->status(Arr::get($data, 'status', 200));
		$this->response->headers(Arr::get($data, 'header', array()));
		$this->response->body(json_encode(array(
			'body'=> Arr::get($data,'body','ok'),
			'rq_headers' => $this->request->headers(),
			'rq_body' => $this->request->body(),
			'rq_method' => $this->request->method(),
		)));
	}

} // End Controller_RequestClientDummy
