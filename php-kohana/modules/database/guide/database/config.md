# Configuration

The default config file is located in `MODPATH/database/config/database.php`.  You should copy this file to `APPPATH/config/database.php` and make changes there, in keeping with the [cascading filesystem](../kohana/files).

The database configuration file contains an array of configuration groups. The structure of each database configuration group, called an "instance", looks like this:

    string INSTANCE_NAME => array(
        'type'         => string DATABASE_TYPE,
        'connection'   => array CONNECTION_ARRAY,
        'table_prefix' => string TABLE_PREFIX,
        'charset'      => string CHARACTER_SET,
    ),
	
Understanding each of these settings is important.

INSTANCE_NAME
:  Connections can be named anything you want, but you should always have at least one connection called "default".

DATABASE_TYPE
:  One of the installed database drivers. Kohana comes with "mysql" and "pdo" drivers.  Drivers must extend the Database class.

CONNECTION_ARRAY
:  Specific driver options for connecting to your database. (Driver options are explained [below](#connection-settings).)

TABLE_PREFIX
:  Prefix that will be added to all table names by the [query builder](#query_building).


## Example

The example file below shows 2 MySQL connections, one local and one remote.

    return array
    (
        'default' => array
        (
            'type'       => 'mysql',
            'connection' => array(
                'hostname'   => 'localhost',
                'username'   => 'dbuser',
                'password'   => 'mypassword',
                'persistent' => FALSE,
                'database'   => 'my_db_name',
            ),
            'table_prefix' => '',
            'charset'      => 'utf8',
        ),
        'remote' => array(
            'type'       => 'mysql',
            'connection' => array(
                'hostname'   => '55.55.55.55',
                'username'   => 'remote_user',
                'password'   => 'mypassword',
                'persistent' => FALSE,
                'database'   => 'my_remote_db_name',
            ),
            'table_prefix' => '',
            'charset'      => 'utf8',
        ),
    );

## Connections and Instances

Each configuration group is referred to as a database instance. Each instance can be accessed by calling [Database::instance].  If you don't provide a parameter, the default instance is used.

	// This would connect to the database defined as 'default'
    $default = Database::instance();
	
	// This would connect to the database defined as 'remote'
    $remote  = Database::instance('remote');

To disconnect the database, simply destroy the object:

    unset($default)
	
	// Or
	
	unset(Database::$instances['default']);

If you want to disconnect all of the database instances at once:

    Database::$instances = array();

## Connection Settings

Every database driver has different connection settings.

### MySQL

A [MySQL database](http://www.php.net/manual/en/book.mysql.php) can accept the following options in the `connection` array:

Type      | Option     |  Description               | Default value
----------|------------|----------------------------| -------------------------
`string`  | hostname   | Hostname of the database   | `localhost`
`integer` | port       | Port number                | `NULL`
`string`  | socket     | UNIX socket                | `NULL`
`string`  | username   | Database username          | `NULL`
`string`  | password   | Database password          | `NULL`
`boolean` | persistent | Persistent connections     | `FALSE`
`string`  | database   | Database name              | `kohana`

### PDO

A [PDO database](http://php.net/manual/en/book.pdo.php) can accept these options in the `connection` array:

Type      | Option     |  Description               | Default value
----------|------------|----------------------------| -------------------------
`string`  | dsn        | PDO data source identifier | `localhost`
`array`   | options    | Driver-specific options    | none
`string`  | username   | Database username          | `NULL`
`string`  | password   | Database password          | `NULL`
`boolean` | persistent | Persistent connections     | `FALSE`

The connection character set should be configured using the DSN string or `options` array.

[!!] If you are using PDO and are not sure what to use for the `dsn` option, review [PDO::__construct](http://php.net/pdo.construct).