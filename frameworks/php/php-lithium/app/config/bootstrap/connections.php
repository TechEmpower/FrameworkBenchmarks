<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

/**
 * ### Configuring backend database connections
 *
 * Lithium supports a wide variety relational and non-relational databases, and is designed to allow
 * and encourage you to take advantage of multiple database technologies, choosing the most optimal
 * one for each task.
 *
 * As with other `Adaptable`-based configurations, each database configuration is defined by a name,
 * and an array of information detailing what database adapter to use, and how to connect to the
 * database server. Unlike when configuring other classes, `Connections` uses two keys to determine
 * which class to select. First is the `'type'` key, which specifies the type of backend to
 * connect to. For relational databases, the type is set to `'database'`. For HTTP-based backends,
 * like CouchDB, the type is `'http'`. Some backends have no type grouping, like MongoDB, which is
 * unique and connects via a custom PECL extension. In this case, the type is set to `'MongoDb'`,
 * and no `'adapter'` key is specified. In other cases, the `'adapter'` key identifies the unique
 * adapter of the given type, i.e. `'MySql'` for the `'database'` type, or `'CouchDb'` for the
 * `'http'` type. Note that while adapters are always specified in CamelCase form, types are
 * specified either in CamelCase form, or in underscored form, depending on whether an `'adapter'`
 * key is specified. See the examples below for more details.
 *
 * ### Multiple environments
 *
 * As with other `Adaptable` classes, `Connections` supports optionally specifying different
 * configurations per named connection, depending on the current environment. For information on
 * specifying environment-based configurations, see the `Environment` class.
 *
 * @see lithium\core\Adaptable
 * @see lithium\core\Environment
 */
use lithium\data\Connections;

/**
 * Uncomment this configuration to use MongoDB as your default database.
 */
// Connections::add('default', array(
// 	'type' => 'MongoDb',
// 	'host' => 'localhost',
// 	'database' => 'my_app'
// ));

/**
 * Uncomment this configuration to use CouchDB as your default database.
 */
// Connections::add('default', array(
// 	'type' => 'http',
// 	'adapter' => 'CouchDb',
// 	'host' => 'localhost',
// 	'database' => 'my_app'
// ));

/**
 * Uncomment this configuration to use MySQL as your default database.
 */
 Connections::add('default', array(
 	'type' => 'database',
 	'adapter' => 'MySql',
 	'host' => '192.168.100.102',
 	'login' => 'benchmarkdbuser',
 	'password' => 'benchmarkdbpass',
 	'database' => 'hello_world',
 	'encoding' => 'UTF-8'
 ));

?>