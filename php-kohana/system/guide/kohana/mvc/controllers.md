# Controllers

A Controller is a class file that stands in between the models and the views in an application. It passes information on to the model when data needs to be changed and it requests information from the model when data needs to be loaded. Controllers then pass on the information of the model to the views where the final output can be rendered for the users.  Controllers essentially control the flow of the application.

Controllers are called by the [Request::execute()] function based on the [Route] that the url matched.  Be sure to read the [routing](routing) page to understand how to use routes to map urls to your controllers.

## Creating Controllers

In order to function, a controller must do the following:

* Reside in `classes/Controller` (or a sub-directory)
* Filename must match the class name exactly, e.g. `Articles.php`
* The class name must map to the filename (with `/` replaced with `_`) and each word is capitalized
* Must have the Controller class as a (grand)parent

Some examples of controller names and file locations:

	// classes/Controller/Foobar.php
	class Controller_Foobar extends Controller {
	
	// classes/Controller/Admin.php
	class Controller_Admin extends Controller {

Controllers can be in sub-folders:

	// classes/Controller/Baz/Bar.php
	class Controller_Baz_Bar extends Controller {
	
	// classes/Controller/Product/Category.php
	class Controller_Product_Category extends Controller {
	
[!!] Note that controllers in sub-folders can not be called by the default route, you will need to define a route that has a [directory](routing#directory) param or sets a default value for directory.

Controllers can extend other controllers.

	// classes/Controller/Users.php
	class Controller_Users extends Controller_Template
	
	// classes/Controller/Api.php
	class Controller_Api extends Controller_REST
	
[!!] [Controller_Template] is an example controller provided in Kohana.

You can also have a controller extend another controller to share common things, such as requiring you to be logged in to use all of those controllers.

	// classes/Controller/Admin.php
	class Controller_Admin extends Controller {
		// This controller would have a before() that checks if the user is logged in
	
	// classes/Controller/Admin/Plugins.php
	class Controller_Admin_Plugins extends Controller_Admin {
		// Because this controller extends Controller_Admin, it would have the same logged in check
		
## $this->request

Every controller has the `$this->request` property which is the [Request] object that called the controller.  You can use this to get information about the current request, as well as set the response body via `$this->response->body($ouput)`.

Here is a partial list of the properties and methods available to `$this->request`.  These can also be accessed via `Request::instance()`, but `$this->request` is provided as a shortcut.  See the [Request] class for more information on any of these. 

Property/method | What it does
--- | ---
[$this->request->route()](../api/Request#property:route) | The [Route] that matched the current request url
[$this->request->directory()](../api/Request#property:directory), <br /> [$this->request->controller](../api/Request#property:controller), <br /> [$this->request->action](../api/Request#property:action) | The directory, controller and action that matched for the current route
[$this->request->param()](../api/Request#param) | Any other params defined in your route

## $this->response
[$this->response->body()](../api/Response#property:body) | The content to return for this request
[$this->response->status()](../api/Response#property:status) | The HTTP status for the request (200, 404, 500, etc.)
[$this->response->headers()](../api/Response#property:headers) | The HTTP headers to return with the response


## Actions

You create actions for your controller by defining a public function with an `action_` prefix.  Any method that is not declared as `public` and prefixed with `action_` can NOT be called via routing.

An action method will decide what should be done based on the current request, it *controls* the application.  Did the user want to save a blog post?  Did they provide the necessary fields?   Do they have permission to do that?  The controller will call other classes, including models, to accomplish this.  Every action should set `$this->response->body($view)` to the [view file](mvc/views) to be sent to the browser, unless it redirected or otherwise ended the script earlier.

A very basic action method that simply loads a [view](mvc/views) file.

	public function action_hello()
	{
		$this->response->body(View::factory('hello/world')); // This will load views/hello/world.php
	}

### Parameters

Parameters are accessed by calling `$this->request->param('name')` where `name` is the name defined in the route.

	// Assuming Route::set('example','<controller>(/<action>(/<id>(/<new>)))');
	
	public function action_foobar()
	{
		$id = $this->request->param('id');
		$new = $this->request->param('new');

If that parameter is not set it will be returned as NULL.  You can provide a second parameter to set a default value if that param is not set.

	public function action_foobar()
	{
		// $id will be false if it was not supplied in the url
		$id = $this->request->param('user',FALSE);

### Examples

A view action for a product page.

	public function action_view()
	{
		$product = new Model_Product($this->request->param('id'));

		if ( ! $product->loaded())
		{
			throw HTTP_Exception::factory(404, 'Product not found!');
		}

		$this->response->body(View::factory('product/view')
			->set('product', $product));
	}

A user login action.

	public function action_login()
	{
		$view = View::factory('user/login');

		if ($this->request->post())
		{
			// Try to login
			if (Auth::instance()->login($this->request->post('username'), $this->request->post('password')))
			{
				$this->redirect('home', 302);
			}

			$view->errors = 'Invalid email or password';
		}

		$this->response->body($view);
	}

## Before and after

You can use the `before()` and `after()` functions to have code executed before or after the action is executed. For example, you could check if the user is logged in, set a template view, loading a required file, etc.

For example, if you look in `Controller_Template` you can see that in the be

You can check what action has been requested (via `$this->request->action`) and do something based on that, such as requiring the user to be logged in to use a controller, unless they are using the login action.

	// Checking auth/login in before, and redirecting if necessary:

	Controller_Admin extends Controller {

		public function before()
		{
			// If this user doesn't have the admin role, and is not trying to login, redirect to login
			if ( ! Auth::instance()->logged_in('admin') AND $this->request->action !== 'login')
			{
				$this->redirect('admin/login', 302);
			}
		}
		
		public function action_login() {
			...

### Custom __construct() function

In general, you should not have to change the `__construct()` function, as anything you need for all actions can be done in `before()`.  If you need to change the controller constructor, you must preserve the parameters or PHP will complain.  This is so the Request object that called the controller is available.  *Again, in most cases you should probably be using `before()`, and not changing the constructor*, but if you really, *really* need to it should look like this:

	// You should almost never need to do this, use before() instead!

	// Be sure Kohana_Request is in the params
	public function __construct(Request $request, Response $response)
	{
		// You must call parent::__construct at some point in your function
		parent::__construct($request, $response);
		
		// Do whatever else you want
	}

## Extending other controllers

TODO: More description and examples of extending other controllers, multiple extension, etc.
