# Error/Exception Handling

Kohana provides both an exception handler and an error handler that transforms errors into exceptions using PHP's [ErrorException](http://php.net/errorexception) class. Many details of the error and the internal state of the application is displayed by the handler:

1. Exception class
2. Error level
3. Error message
4. Source of the error, with the error line highlighted
5. A [debug backtrace](http://php.net/debug_backtrace) of the execution flow
6. Included files, loaded extensions, and global variables

## Example

Click any of the links to toggle the display of additional information:

<div>{{userguide/examples/error}}</div>

## Disabling Error/Exception Handling

If you do not want to use the internal error handling, you can disable it (highly discouraged) when calling [Kohana::init]:

    Kohana::init(array('errors' => FALSE));

## Error Reporting

By default, Kohana displays all errors, including strict mode warnings. This is set using [error_reporting](http://php.net/error_reporting):

    error_reporting(E_ALL | E_STRICT);

When you application is live and in production, a more conservative setting is recommended, such as ignoring notices:

    error_reporting(E_ALL & ~E_NOTICE);

If you get a white screen when an error is triggered, your host probably has disabled displaying errors. You can turn it on again by adding this line just after your `error_reporting` call:

    ini_set('display_errors', TRUE);

Errors should **always** be displayed, even in production, because it allows you to use [exception and error handling](debugging.errors) to serve a nice error page rather than a blank white screen when an error happens.

## HTTP Exception Handling

Kohana comes with a robust system for handing http errors. It includes exception classes for each http status code. To trigger a 404 in your application (the most common scenario):

	throw HTTP_Exception::factory(404, 'File not found!');

To register error pages for these, using 404 as an example:

    class HTTP_Exception_404 extends Kohana_HTTP_Exception_404 {

        public function get_response()
        {
            $response = Response::factory();

            $view = View::factory('errors/404');

            // We're inside an instance of Exception here, all the normal stuff is available.
            $view->message = $this->getMessage();

            $response->body($view->render());

            return $response;
        }

    }