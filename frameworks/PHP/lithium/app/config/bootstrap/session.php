<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

/**
 * This configures your session storage. The Cookie storage adapter must be connected first, since
 * it intercepts any writes where the `'expires'` key is set in the options array.
 * The default name is based on the lithium app path. Remember, if your app is numeric or has
 * special characters you might want to use Inflector::slug() or set this manually.
 */
use lithium\storage\Session;

$name = basename(LITHIUM_APP_PATH);
Session::config(array(
	// 'cookie' => array('adapter' => 'Cookie', 'name' => $name),
	'default' => array('adapter' => 'Php', 'session.name' => $name)
));

/**
 * Uncomment the lines below to enable forms-based authentication. This configuration will attempt
 * to authenticate users against a `Users` model. In a controller, run
 * `Auth::check('default', $this->request)` to authenticate a user. This will check the POST data of
 * the request (`lithium\action\Request::$data`) to see if the fields match the `'fields'` key of
 * the configuration below. If successful, it will write the data returned from `Users::first()` to
 * the session using the default session configuration.
 *
 * Once the session data is written, you can call `Auth::check('default')` to check authentication
 * status or retrieve the user's data from the session. Call `Auth::clear('default')` to remove the
 * user's authentication details from the session. This effectively logs a user out of the system.
 * To modify the form input that the adapter accepts, or how the configured model is queried, or how
 * the data is stored in the session, see the `Form` adapter API or the `Auth` API, respectively.
 *
 * @see lithium\security\auth\adapter\Form
 * @see lithium\action\Request::$data
 * @see lithium\security\Auth
 */
// use lithium\security\Auth;

// Auth::config(array(
// 	'default' => array(
// 		'adapter' => 'Form',
// 		'model' => 'Users',
// 		'fields' => array('username', 'password')
// 	)
// ));

?>