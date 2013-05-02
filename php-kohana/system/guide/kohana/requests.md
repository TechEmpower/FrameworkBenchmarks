# Requests

Kohana includes a flexible HMVC request system. It supports out of the box support for internal requests and external requests.

HMVC stands for `Hierarchical Model View Controller` and basically means requests can each have MVC triads called from inside each other.

The Request object in Kohana is HTTP/1.1 compliant.

## Creating Requests

Creating a request is very easy:

### Internal Requests

An internal request is a request calling to the internal application. It utilizes [routes](routing) to direct the application based on the URI that is passed to it. A basic internal request might look something like:

	$request = Request::factory('welcome');

In this example, the URI is 'welcome'.

#### The initial request

Since Kohana uses HMVC, you can call many requests inside each other. The first request (usually called from `index.php`) is called the "initial request". You can access this request via:

	Request::initial();

You should only use this method if you are absolutely sure you want the initial request. Otherwise you should use the `Request::current()` method.

#### Sub-requests

You can call a request at any time in your application by using the `Request::factory()` syntax. All of these requests will be considered sub-requests.

Other than this difference, they are exactly the same. You can detect if the request is a sub-request in your controller with the is_initial() method:

	$sub_request = ! $this->request->is_initial()

### External Requests

An external request calls out to a third party website.

You can use this to scrape HTML from a remote site, or make a REST call to a third party API:

	// This uses GET
	$request = Request::factory('http://www.google.com/');

	// This uses PUT
	$request = Request::factory('http://example.com/put_api')->method(Request::PUT)->body(json_encode('the body'))->headers('Content-Type', 'application/json');

	// This uses POST
	$request = Request::factory('http://example.com/post_api')->method(Request::POST)->post(array('foo' => 'bar', 'bar' => 'baz'));

## Executing Requests

To execute a request, use the `execute()` method on it. This will give you a [response](responses) object.

	$request = Request::factory('welcome');
	$response = $request->execute();

### Header callbacks
The request client supports header callbacks - an array of callbacks that will be triggered when a specified header is included in the response from a server. Header callbacks provide a powerful way to deal with scenarios including authentication, rate limiting, redirects and other application-specific use cases:

	$request = Request::factory('http://example.com/user', array(
		'header_callbacks' => array(
			'Content-Encoding' =>
				function (Request $request, Response $response, Request_Client $client)
				{
					// Uncompress the response
					$response->body(GZIP::expand($response->body()));
				},
			'X-Rate-Limited' =>
				function (Request $request, Response $response, Request_Client $client)
				{
					// Log the rate limit event
					// And perhaps set a deadlock in cache to prevent further requests
				},
			'WWW-Authenticate' =>
				function (Request $request, Response $response, Request_Client $client)
				{
					// Execute a request to refresh your OAuth token somehow
					// Have the original request resent
					return Request::factory($request->uri())
							->query($request->query())
							->headers('Authorization', 'token'.$token);
				}));

Where multiple headers are present in the response, callbacks will be executed in sequence. Callbacks can be any valid PHP callback type and have three possible return types:

Type              | Function
------------------|---------
[Request] object  | If a new request is returned, the request client will automatically assign properties, callbacks etc to match the original request and then execute the request. No further callbacks will be triggered for the original request, but the new request may trigger callbacks when executed.
[Response] object | If the callback returns a new response instance it will be returned to the application. No further callbacks will be triggered for the original request. The callback is responsible for setting any relevant callbacks and properties for the request it executes
NULL              | The callback can, if required, modify the provided Response object and return NULL. The modified response object will be passed into subsequent callbacks.

#### Nested requests
If your callback returns a new Request object, the request client will apply the same callback and property definitions to it before execution. This allows for nested requests - for example, you might need to re-authenticate before submitting a POST request and then being redirected to a new location. To avoid infinite recursion and fatal errors, the request client keeps track of the number of subrequests and will throw a [Request_Client_Recursion_Exception] if the recursion gets too deep. This behaviour is controlled by two properties: [Request_Client::callback_depth()] and [Request_Client::max_callback_depth()]. The default limit is 5 subrequests.

If your callback executes a new request itself and returns the response, it is responsible for dealing with any callbacks and request nesting itself. You may find the [Request_Client::assign_client_properties()] method useful in this case.

#### Callback parameters
Arbitrary parameters can be passed to the callbacks through the [Request_Client::callback_params()] property:

	$request = Request::factory('http://example.com/foo', array(
		'header_callbacks' => array(
			'X-Custom-1' =>
				function (Request $request, Response $response, Request_Client $client)
				{
					// Do something that needs an external parameter
					if ($client->callback_params('foo') == 'bar')
					{
						// etc
					}
				},
			)
		'callback_params' => array(
			'foo' => 'bar'
			)
		));
	
	// later on
	$request->client()->callback_params('foo',FALSE);

As with nested requests, callback_params will automatically be passed to subrequests if the callback returns a new Request object. If the callback returns a Response object, it is responsible for passing on any relevant parameters.

#### Following redirects
The request client ships with a standard callback to automatically follow redirects - [Request_Client::on_header_location()]. This will recursively follow redirects that are specified with a Location header and a status code in 201, 301, 302, 303, 307. This behaviour is disabled by default, but can be enabled by passing a set of options to the Request's constructor:

	$request = Request::factory('http://example.com/redirectme', array(
		'follow' => TRUE));

[!!] If you define additional header callbacks of your own, you will need to include the 'Location' callback in your callbacks array.

A number of options are available to control the behaviour of the [Request_Client] when following redirects.

Option           |Default                 |Function
-----------------|------------------------|---------
follow           | FALSE                  |Whether to follow redirects
follow_headers   | array('Authorization') |The keys of headers that will be re-sent with the redirected request
strict_redirect  | TRUE                   |Whether to use the original request method following to a 302 redirect (see below)

[!!] HTTP/1.1 specifies that a 302 redirect should be followed using the original request method. However, the vast majority of clients and servers get this wrong, with 302 widely used for 'POST - 302 redirect - GET' patterns. By default, Kohana's client is fully compliant with the HTTP spec. If you need to interact with non-compliant third party sites you may need to set strict_redirect FALSE to force the client to switch to GET following a 302 response.

You can easily alter this behaviour by configuring your own 'Location' header callback.

## Request Cache Control

You can cache requests for fast execution by passing a cache instance in as the second parameter of factory:

	$request = Request::factory('welcome', array('cache'=>Cache::instance()));

TODO
