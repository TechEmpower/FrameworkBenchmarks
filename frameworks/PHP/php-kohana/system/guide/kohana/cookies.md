# Cookies

Kohana provides classes that make it easy to work with both cookies and sessions. At a high level both sessions and cookies provide the same functionality. They allow the developer to store temporary or persistent information about a specific client for later retrieval, usually to make something persistent between requests.

[Cookies](http://en.wikipedia.org/wiki/HTTP_cookie) should be used for storing non-private data that is persistent for a long period of time. For example storing a user preference or a language setting. Use the [Cookie] class for getting and setting cookies.

[!!] Kohana uses "signed" cookies. Every cookie that is stored is combined with a secure hash to prevent modification of the cookie.  If a cookie is modified outside of Kohana the hash will be incorrect and the cookie will be deleted.  This hash is generated using [Cookie::salt()], which uses the [Cookie::$salt] property. You must define this setting in your bootstrap.php:

	Cookie::$salt = 'foobar';

Or define an extended cookie class in your application:

	class Cookie extends Kohana_Cookie
	{
		public static $salt = 'foobar';
	}

You should set the salt to a secure value. The example above is only for demonstrative purposes.

Nothing stops you from using `$_COOKIE` like normal, but you can not mix using the Cookie class and the regular `$_COOKIE` global, because the hash that Kohana uses to sign cookies will not be present, and Kohana will delete the cookie.

## Storing, Retrieving, and Deleting Data

[Cookie] and [Session] provide a very similar API for storing data. The main difference between them is that sessions are accessed using an object, and cookies are accessed using a static class.

### Storing Data

Storing session or cookie data is done using the [Cookie::set] method:

    // Set cookie data
    Cookie::set($key, $value);

    // Store a user id
    Cookie::set('user_id', 10);

### Retrieving Data

Getting session or cookie data is done using the [Cookie::get] method:

    // Get cookie data
    $data = Cookie::get($key, $default_value);

    // Get the user id
    $user = Cookie::get('user_id');

### Deleting Data

Deleting session or cookie data is done using the [Cookie::delete] method:
    
    // Delete cookie data
    Cookie::delete($key);

    // Delete the user id
    Cookie::delete('user_id');

## Cookie Settings

All of the cookie settings are changed using static properties. You can either change these settings in `bootstrap.php` or by using [transparent extension](extension).  Always check these settings before making your application live, as many of them will have a direct affect on the security of your application.

The most important setting is [Cookie::$salt], which is used for secure signing. This value should be changed and kept secret:

    Cookie::$salt = 'your secret is safe with me';

[!!] Changing this value will render all cookies that have been set before invalid.

By default, cookies are stored until the browser is closed. To use a specific lifetime, change the [Cookie::$expiration] setting:

    // Set cookies to expire after 1 week
    Cookie::$expiration = 604800;

    // Alternative to using raw integers, for better clarity
    Cookie::$expiration = Date::WEEK;

The path that the cookie can be accessed from can be restricted using the [Cookie::$path] setting.

    // Allow cookies only when going to /public/*
    Cookie::$path = '/public/';

The domain that the cookie can be accessed from can also be restricted, using the [Cookie::$domain] setting.

    // Allow cookies only on the domain www.example.com
    Cookie::$domain = 'www.example.com';

If you want to make the cookie accessible on all subdomains, use a dot at the beginning of the domain.

    // Allow cookies to be accessed on example.com and *.example.com
    Cookie::$domain = '.example.com';

To only allow the cookie to be accessed over a secure (HTTPS) connection, use the [Cookie::$secure] setting.

    // Allow cookies to be accessed only on a secure connection
    Cookie::$secure = TRUE;
    
    // Allow cookies to be accessed on any connection
    Cookie::$secure = FALSE;

To prevent cookies from being accessed using Javascript, you can change the [Cookie::$httponly] setting.

    // Make cookies inaccessible to Javascript
    Cookie::$httponly = TRUE;