# Migrating from 3.2.x

## HVMC Isolation

HVMC Sub-request isolation has been improved to prevent exceptions leaking from this inner to the outer request. If you were previously catching any exceptions from sub-requests, you should now be checking the [Response] object returned from [Request::execute].

## HTTP Exceptions

The use of HTTP Exceptions is now encouraged over manually setting the [Response] status to, for example, '404'. This allows for easier custom error pages (detailed below);

The full list of supported codes can be seen in the SYSPATH/classes/HTTP/Exception/ folder.

Syntax:

    throw HTTP_Exception::factory($code, $message, array $variables, Exception $previous);

Examples:

    // Page Not Found
    throw HTTP_Exception::factory(404, 'The requested URL :uri was not found on this server.', array(
            ':uri' => $this->request->uri(),
        ));

    // Unauthorized / Login Requied
    throw HTTP_Exception::factory(401)->authenticate('Basic realm="MySite"');

    // Forbidden / Permission Deined
    throw HTTP_Exception::factory(403);

## Redirects (HTTP 300, 301, 302, 303, 307)

Redirects are no longer issued against the [Request] object. The new syntax from inside a controller is:

    $this->redirect('http://www.google.com', 302);

or from outside a controller:

    HTTP::redirect('http://www.google.com', 302);

## Custom Error Pages (HTTP 500, 404, 403, 401 etc)

Custom error pages are now easier than ever to implement, thanks to some of the changes brought about by the HVMC and Redirect changes above.

See [Custom Error Pages](tutorials/error-pages) for more details.

## Browser cache checking (ETags)

The Response::check_cache method has moved to [HTTP::check_cache], with an alias at [Controller::check_cache]. Previously, this method would be used from a controller like this:

    $this->response->check_cache(sha1('my content'), Request $this->request);

Now, there are two options for using the method:

    $this->check_cache(sha1('my content'));

which is an alias for:

    HTTP::check_cache($this->request, $this->response, sha1('my content'));

## PSR-0 support (file/class naming conventions)

With the introduction of [PSR-0](https://github.com/php-fig/fig-standards/blob/master/accepted/PSR-0.md) support, the autoloading of classes is case sensitive. Now, the file (and folder) names must match the class name exactly.

Examples:

    Kohana_Core

would be located in

    classes/Kohana/Core.php

and

    Kohana_HTTP_Header

would be located in

    classes/Kohana/HTTP/Header.php

This also affects dynamically named classes such as drivers and ORMs. So for example, in the database config using `'mysql'` as the type instead of `'MySQL'` would throw a class not found error.

## Query Builder Identifier Escaping

The query builder will no longer detect columns like `COUNT("*")`. Instead, you will need to use `DB::expr()` any time you need an unescaped column. For example:

    DB::select(DB::expr('COUNT(*)'))->from('users')->execute()

## Route Filters

In `3.3.0`, you can no longer pass a callback to `Route::uri()`. Instead, we've added the ability to define one or more filters which will be able to decide if the route matches and will also allow you to change any of the parameters. These filters will receive the `Route` object being tested, the currently matched `$params` array, and the `Request` object as the three parameters.

    Route::set('route-name', 'some/uri/<id>')
        ->filter(function($route, $params, $request) {
            // Returning FALSE will make this route not match
            // Returning an array will replace the $params sent to the controller
        });

These filters can be used for things like prepending the request method to the action, checking if a resource exists before matching the route, or any other logic that the URI alone cannot provide. You can add as many filters as needed so it's useful to keep filters as small as possible to make them reusable.

See [Routing](routing#route-filters) for more details.