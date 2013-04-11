# Sessions

Kohana provides classes that make it easy to work with both cookies and sessions. At a high level both sessions and cookies provide the same functionality. They allow the developer to store temporary or persistent information about a specific client for later retrieval, usually to make something persistent between requests.

Sessions should be used for storing temporary or private data.  Very sensitive data should be stored using the [Session] class with the "database" or "native" adapters. When using the "cookie" adapter, the session should always be encrypted.

[!!] For more information on best practices with session variables see [the seven deadly sins of sessions](http://lists.nyphp.org/pipermail/talk/2006-December/020358.html).

## Storing, Retrieving, and Deleting Data

[Cookie] and [Session] provide a very similar API for storing data. The main difference between them is that sessions are accessed using an object, and cookies are accessed using a static class.

Accessing the session instance is done using the [Session::instance] method:

    // Get the session instance
    $session = Session::instance();

When using sessions, you can also get all of the current session data using the [Session::as_array] method:

    // Get all of the session data as an array
    $data = $session->as_array();

You can also use this to overload the `$_SESSION` global to get and set data in a way more similar to standard PHP:

    // Overload $_SESSION with the session data
    $_SESSION =& $session->as_array();
    
    // Set session data
    $_SESSION[$key] = $value;

### Storing Data

Storing session or cookie data is done using the `set` method:

    // Set session data
    $session->set($key, $value);
	// Or
	Session::instance()->set($key, $value);

    // Store a user id
    $session->set('user_id', 10);

### Retrieving Data

Getting session or cookie data is done using the `get` method:

    // Get session data
    $data = $session->get($key, $default_value);

    // Get the user id
    $user = $session->get('user_id');

### Deleting Data

Deleting session or cookie data is done using the `delete` method:

    // Delete session data
    $session->delete($key);


    // Delete the user id
    $session->delete('user_id');

## Session Configuration

Always check these settings before making your application live, as many of them will have a direct affect on the security of your application.

## Session Adapters

When creating or accessing an instance of the [Session] class you can decide which session adapter or driver you wish to use. The session adapters that are available to you are:

Native
: Stores session data in the default location for your web server. The storage location is defined by [session.save_path](http://php.net/manual/session.configuration.php#ini.session.save-path) in `php.ini` or defined by [ini_set](http://php.net/ini_set).

Database
: Stores session data in a database table using the [Session_Database] class. Requires the [Database] module to be enabled.

Cookie
: Stores session data in a cookie using the [Cookie] class. **Sessions will have a 4KB limit when using this adapter, and should be encrypted.**

The default adapter can be set by changing the value of [Session::$default]. The default adapter is "native".

To access a Session using the default adapter, simply call [Session::instance()].  To access a Session using something other than the default, pass the adapter name to `instance()`, for example: `Session::instance('cookie')`


### Session Adapter Settings

You can apply configuration settings to each of the session adapters by creating a session config file at `APPPATH/config/session.php`. The following sample configuration file defines all the settings for each adapter:

[!!] As with cookies, a "lifetime" setting of "0" means that the session will expire when the browser is closed.

    return array(
        'native' => array(
            'name' => 'session_name',
            'lifetime' => 43200,
        ),
        'cookie' => array(
            'name' => 'cookie_name',
            'encrypted' => TRUE,
            'lifetime' => 43200,
        ),
        'database' => array(
            'name' => 'cookie_name',
            'encrypted' => TRUE,
            'lifetime' => 43200,
            'group' => 'default',
            'table' => 'table_name',
            'columns' => array(
                'session_id'  => 'session_id',
        		'last_active' => 'last_active',
        		'contents'    => 'contents'
            ),
            'gc' => 500,
        ),
    );

#### Native Adapter

Type      | Setting   | Description                                       | Default
----------|-----------|---------------------------------------------------|-----------
`string`  | name      | name of the session                               | `"session"`
`integer` | lifetime  | number of seconds the session should live for     | `0`

#### Cookie Adapter

Type      | Setting   | Description                                       | Default
----------|-----------|---------------------------------------------------|-----------
`string`  | name      | name of the cookie used to store the session data | `"session"`
`boolean` | encrypted | encrypt the session data using [Encrypt]?         | `FALSE`
`integer` | lifetime  | number of seconds the session should live for     | `0`

#### Database Adapter

Type      | Setting   | Description                                       | Default
----------|-----------|---------------------------------------------------|-----------
`string`  | group     | [Database::instance] group name                   | `"default"`
`string`  | table     | table name to store sessions in                   | `"sessions"`
`array`   | columns   | associative array of column aliases               | `array`
`integer` | gc        | 1:x chance that garbage collection will be run    | `500`
`string`  | name      | name of the cookie used to store the session data | `"session"`
`boolean` | encrypted | encrypt the session data using [Encrypt]?         | `FALSE`
`integer` | lifetime  | number of seconds the session should live for     | `0`

##### Table Schema

You will need to create the session storage table in the database. This is the default schema:

    CREATE TABLE  `sessions` (
        `session_id` VARCHAR(24) NOT NULL,
        `last_active` INT UNSIGNED NOT NULL,
        `contents` TEXT NOT NULL,
        PRIMARY KEY (`session_id`),
        INDEX (`last_active`)
    ) ENGINE = MYISAM;

##### Table Columns

You can change the column names to match an existing database schema when connecting to a legacy session table. The default value is the same as the key value.

session_id
: the name of the "id" column

last_active
: UNIX timestamp of the last time the session was updated

contents
: session data stored as a serialized string, and optionally encrypted