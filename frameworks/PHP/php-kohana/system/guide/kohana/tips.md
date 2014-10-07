# Tips and Common Mistakes

This is a collection of tips and common mistakes or errors you may encounter. 

## Never edit the `system` folder!

You should (almost) never edit the system folder.  Any change you want to make to files in system and modules can be made via the [cascading filesystem](files) and [transparent extension](extension) and won't break when you try to update your Kohana version.  

## Don't try and use one route for everything

Kohana 3 [routes](routing) are very powerful and flexible, don't be afraid to use as many as you need to make your app function the way you want!

## Files not found on some systems

As of Kohana 3.3, classes are autoloaded using the case-sensitive PSR-0 autoloader. This means that using the class Foo {} with a file in classes/foo.php will work on case-insensitive file systems (such as the default HFS+ FS used in Mac OS X) but will fail when used on a case-sensitive FS (typical on many production Linux servers).

## Handling lots of routes

Sometimes your application is sufficiently complex that you have many routes and it becomes unmanageable to put them all in bootstrap.php. If this is the case, simply make a `routes.php` file in APPPATH and require that in your bootstrap: `require_once APPPATH.'routes'.EXT;`

## Reflection_Exception

If you get a Reflection_Exception when setting up your site, it is almost certainly because your [Kohana::init] 'base_url' setting is wrong.  If your base url is correct something is probably wrong with your [routes](routing).

	ReflectionException [ -1 ]: Class controller_<something> does not exist
	// where <something> is part of the url you entered in your browser

### Solution  {#reflection-exception-solution}

Set your [Kohana::init] 'base_url' to the correct setting. The base url should be the path to your index.php file relative to the webserver document root.

## ORM/Session __sleep() bug

There is a bug in php which can corrupt your session after a fatal error.  A production server shouldn't have uncaught fatal errors, so this bug should only happen during development, when you do something stupid and cause a fatal error.  On the next page load you will get a database connection error, then all subsequent page loads will display the following error:

	ErrorException [ Notice ]: Undefined index: id
	MODPATH/orm/classes/kohana/orm.php [ 1308 ]

### Solution   {#orm-session-sleep-solution}

To fix this, clear your cookies for that domain to reset your session.  This should never happen on a production server, so you won't have to explain to your clients how to clear their cookies.  You can see the [discussion on this issue](http://dev.kohanaframework.org/issues/3242) for more details.
