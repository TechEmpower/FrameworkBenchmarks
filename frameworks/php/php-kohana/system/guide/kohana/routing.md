# Routing

Kohana provides a very powerful routing system.  In essence, routes provide an interface between the urls and your controllers and actions.  With the correct routes you could make almost any url scheme correspond to almost any arrangement of controllers, and you could change one without impacting the other.

As mentioned in the [Request Flow](flow) section, a request is handled by the [Request] class, which will look for a matching [Route] and load the appropriate controller to handle that request.

[!!] It is important to understand that **routes are matched in the order they are added**, and as soon as a URL matches a route, routing is essentially "stopped" and *the remaining routes are never tried*.  Because the default route matches almost anything, including an empty url, new routes must be place before it.

## Creating routes

If you look in `APPPATH/bootstrap.php` you will see the "default" route as follows:

	Route::set('default', '(<controller>(/<action>(/<id>)))')
	->defaults(array(
		'controller' => 'Welcome',
		'action'     => 'index',
	));
	
[!!] The default route is simply provided as a sample, you can remove it and replace it with your own routes.

So this creates a route with the name `default` that will match urls in the format of `(<controller>(/<action>(/<id>)))`.  

Let's take a closer look at each of the parameters of [Route::set], which are `name`, `uri`, and an optional array `regex`.

### Name

The name of the route must be a **unique** string.  If it is not it will overwrite the older route with the same name. The name is used for creating urls by reverse routing, or checking which route was matched.

### URI

The uri is a string that represents the format of urls that should be matched.  The tokens surrounded with `<>` are *keys* and anything surrounded with `()` are *optional* parts of the uri. In Kohana routes, any character is allowed and treated literally aside from `()<>`.  The `/` has no meaning besides being a character that must match in the uri.  Usually the `/` is used as a static seperator but as long as the regex makes sense, there are no restrictions to how you can format your routes.

Lets look at the default route again, the uri is `(<controller>(/<action>(/<id>)))`.  We have three keys or params: controller, action, and id.   In this case, the entire uri is optional, so a blank uri would match and the default controller and action (set by defaults(), [covered below](#defaults)) would be assumed resulting in the `Controller_Welcome` class being loaded and the `action_index` method being called to handle the request.

You can use any name you want for your keys, but the following keys have special meaning to the [Request] object, and will influence which controller and action are called:

 * **Directory** - The sub-directory of `classes/Controller` to look for the controller (\[covered below]\(#directory))
 * **Controller** - The controller that the request should execute.
 * **Action** - The action method to call.

### Regex

The Kohana route system uses [perl compatible regular expressions](http://perldoc.perl.org/perlre.html) in its matching process.  By default each key (surrounded by `<>`) will match `[^/.,;?\n]++` (or in english: anything that is not a slash, period, comma, semicolon, question mark, or newline).  You can define your own patterns for each key by passing an associative array of keys and patterns as an additional third argument to Route::set.

In this example, we have controllers in two directories, `admin` and `affiliate`.  Because this route will only match urls that begin with `admin` or `affiliate`, the default route would still work for controllers in `classes/Controller`.  

	Route::set('sections', '<directory>(/<controller>(/<action>(/<id>)))',
		array(
			'directory' => '(admin|affiliate)'
		))
		->defaults(array(
			'controller' => 'Home',
			'action'     => 'index',
		));

You can also use a less restrictive regex to match unlimited parameters, or to ignore overflow in a route.  In this example, the url `foobar/baz/and-anything/else_that/is-on-the/url` would be routed to `Controller_Foobar::action_baz()` and the `"stuff"` parameter would be `"and-anything/else_that/is-on-the/url"`.  If you wanted to use this for unlimited parameters, you could [explode](http://php.net/manual/en/function.explode.php) it, or you just ignore the overflow.  

	Route::set('default', '(<controller>(/<action>(/<stuff>)))', array('stuff' => '.*'))
		->defaults(array(
			'controller' => 'Welcome',
			'action' => 'index',
	  ));


### Default values

If a key in a route is optional (or not present in the route), you can provide a default value for that key by passing an associated array of keys and default values to [Route::defaults], chained after your [Route::set].  This can be useful to provide a default controller or action for your site, among other things.

[!!] The `controller` and `action` key must always have a value, so they either need to be required in your route (not inside of parentheses) or have a default value provided.

[!!] Kohana automatically converts controllers to follow the standard naming convention. For example /blog/view/123 would look for the controller Controller_Blog in classes/Controller/Blog.php and trigger the action_view() method on it.

In the default route, all the keys are optional, and the controller and action are given a default.   If we called an empty url, the defaults would fill in and `Controller_Welcome::action_index()` would be called.  If we called `foobar` then only the default for action would be used, so it would call `Controller_Foobar::action_index()` and finally, if we called `foobar/baz` then neither default would be used and `Controller_Foobar::action_baz()` would be called.

TODO: need an example here

You can also use defaults to set a key that isn't in the route at all.

TODO: example of either using directory or controller where it isn't in the route, but set by defaults

### Directory

## Route Filters

In 3.3, you can specify advanced routing schemes by using filter callbacks. When you need to match a route based on more than just the URI of a request, for example, based on the method request (GET/POST/DELETE), a filter will allow you to do so. These filters will receive the `Route` object being tested, the currently matched `$params` array, and the `Request` object as the three parameters. Here's a simple example:

	Route::set('save-form', 'save')
		->filter(function($route, $params, $request)
		{
			if ($request->method() !== HTTP_Request::POST)
			{
				return FALSE; // This route only matches POST requests
			}
		});

Filters can also replace or alter the array of parameters:

	Route::set('rest-api', 'api/<action>')
		->filter(function($route, $params, $request)
		{
			// Prefix the method to the action name
			$params['action'] = strtolower($request->method()).'_'.$params['action'];
			return $params; // Returning an array will replace the parameters
		})
		->defaults(array(
			'controller' => 'api',
		));

If you are using php 5.2, you can still use any valid callback for this behavior:

	Route::set('testing', 'foo')
		->filter(array('Class', 'method_to_process_my_uri'));

## Examples

There are countless other possibilities for routes. Here are some more examples:

    /*
     * Authentication shortcuts
     */
    Route::set('auth', '<action>',
      array(
        'action' => '(login|logout)'
      ))
      ->defaults(array(
        'controller' => 'Auth'
      ));
      
    /*
     * Multi-format feeds
     *   452346/comments.rss
     *   5373.json
     */
    Route::set('feeds', '<user_id>(/<action>).<format>',
      array(
        'user_id' => '\d+',
        'format' => '(rss|atom|json)',
      ))
      ->defaults(array(
        'controller' => 'Feeds',
        'action' => 'status',
      ));
    
    /*
     * Static pages
     */
    Route::set('static', '<path>.html',
      array(
        'path' => '[a-zA-Z0-9_/]+',
      ))
      ->defaults(array(
        'controller' => 'Static',
        'action' => 'index',
      ));
      
    /*
     * You don't like slashes?
     *   EditGallery:bahamas
     *   Watch:wakeboarding
     */
    Route::set('gallery', '<action>(<controller>):<id>',
      array(
        'controller' => '[A-Z][a-z]++',
        'action'     => '[A-Z][a-z]++',
      ))
      ->defaults(array(
        'controller' => 'Slideshow',
      ));
      
    /*
     * Quick search
     */
    Route::set('search', ':<query>', array('query' => '.*'))
      ->defaults(array(
        'controller' => 'Search',
        'action' => 'index',
      ));

## Request parameters

The `directory`, `controller` and `action` can be accessed from the [Request] as public properties like so:

	// From within a controller:
	$this->request->action();
	$this->request->controller();
	$this->request->directory();
	
	// Can be used anywhere:
	Request::current()->action();
	Request::current()->controller();
	Request::current()->directory();

All other keys specified in a route can be accessed via [Request::param()]:

	// From within a controller:
	$this->request->param('key_name');
	
	// Can be used anywhere:
	Request::current()->param('key_name');

The [Request::param] method takes an optional second argument to specify a default return value in case the key is not set by the route. If no arguments are given, all keys are returned as an associative array.  In addition, `action`, `controller` and `directory` are not accessible via [Request::param()].

For example, with the following route:

	Route::set('ads','ad/<ad>(/<affiliate>)')
	->defaults(array(
		'controller' => 'ads',
		'action'     => 'index',
	));
	
If a url matches the route, then `Controller_Ads::index()` will be called.  You can access the parameters by using the `param()` method of the controller's [Request]. Remember to define a default value (via the second, optional parameter of [Request::param]) if you didn't in `->defaults()`.

	class Controller_Ads extends Controller {
		public function action_index()
		{
			$ad = $this->request->param('ad');
			$affiliate = $this->request->param('affiliate',NULL);
		}
	

## Where should routes be defined?

The established convention is to either place your custom routes in the `MODPATH/<module>/init.php` file of your module if the routes belong to a module, or simply insert them into the `APPPATH/bootstrap.php` file (be sure to put them **above** the default route) if they are specific to the application. Of course, nothing stops you from including them from an external file, or even generating them dynamically.

## A deeper look at how routes work

TODO: talk about how routes are compiled

## Creating URLs and links using routes

Along with Kohana's powerful routing capabilities are included some methods for generating URLs for your routes' uris. You can always specify your uris as a string using [URL::site] to create a full URL like so:

    URL::site('admin/edit/user/'.$user_id);

However, Kohana also provides a method to generate the uri from the route's definition. This is extremely useful if your routing could ever change since it would relieve you from having to go back through your code and change everywhere that you specified a uri as a string. Here is an example of dynamic generation that corresponds to the `feeds` route example from above:

    Route::get('feeds')->uri(array(
      'user_id' => $user_id,
      'action' => 'comments',
      'format' => 'rss'
    ));

Let's say you decided later to make that route definition more verbose by changing it to `feeds/<user_id>(/<action>).<format>`. If you wrote your code with the above uri generation method you wouldn't have to change a single line! When a part of the uri is enclosed in parentheses and specifies a key for which there in no value provided for uri generation and no default value specified in the route, then that part will be removed from the uri. An example of this is the `(/<id>)` part of the default route; this will not be included in the generated uri if an id is not provided.

One method you might use frequently is the shortcut [Request::uri] which is the same as the above except it assumes the current route, directory, controller and action. If our current route is the default and the uri was `users/list`, we can do the following to generate uris in the format `users/view/$id`:

    $this->request->uri(array('action' => 'view', 'id' => $user_id));
    
Or if within a view, the preferable method is:

    Request::instance()->uri(array('action' => 'view', 'id' => $user_id));

TODO: examples of using html::anchor in addition to the above examples

## Testing routes

TODO: mention bluehawk's devtools module