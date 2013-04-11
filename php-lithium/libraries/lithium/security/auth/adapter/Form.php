<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\security\auth\adapter;

use lithium\core\Libraries;
use UnexpectedValueException;
use lithium\security\Password;

/**
 * The `Form` adapter provides basic authentication facilities for checking credentials submitted
 * via a web form against a database. To perform an authentication check, the adapter accepts
 * an instance of a `Request` object which contains the submitted form data in its `$data` property.
 *
 * When a request is submitted, the adapter will take the form data from the `Request` object,
 * apply any filters as appropriate (see the `'filters'` configuration setting below), and
 * query a model class using using the filtered data. The data is then checked against any
 * validators configured, which can programmatically check submitted values against database values.
 *
 * By default, the adapter uses a model called `Users`, and lookup fields called `'username'` and
 * `'password'`. These can be customized by setting the `'model'` and `'fields'` configuration keys,
 * respectively. The `'model'` key accepts either a model name (i.e. `Customers`), or a
 * fully-namespaced model class name (i.e. `my_app\models\Customers`). The `'fields'` setting
 * accepts an array of field names to use when looking up a user. An example configuration,
 * including a custom model class and lookup fields might look like the following:
 *
 * {{{
 * Auth::config(array(
 * 	'customer' => array(
 * 		'adapter' => 'Form',
 * 		'model' => 'Customers',
 * 		'fields' => array('email', 'password')
 * 	)
 * ));
 * }}}
 *
 * If the field names present in the form match the fields used in the database lookup, the above
 * will suffice. If, however, the form fields must be matched to different database field names,
 * you can specify an array which matches up the form field names to their corresponding database
 * field names. Suppose, for example, user authentication information in a MongoDB database is
 * nested within a sub-object called `login`. The adapter could be configured as follows:
 *
 * {{{
 * Auth::config(array(
 * 	'customer' => array(
 * 		'adapter' => 'Form',
 * 		'model' => 'Customers',
 * 		'fields' => array('username' => 'login.username', 'password' => 'login.password'),
 * 		'scope' => array('active' => true)
 * 	)
 * ));
 * }}}
 *
 * Note that any additional fields may be specified which should be included in the query. For
 * example, if a user must select a group when logging in, you may override the `'fields'` key with
 * that value as well (i.e. `'fields' => array('username', 'password', 'group')`). If a field is
 * specified which is not present in the request data, the value in the authentication query will be
 * `null`). Note that this will only submit data that is specified in the incoming request. If you
 * would like to further limit the query using fixed conditions, use the `'scope'` key, as shown in
 * the example above.
 *
 * ## Pre-Query Filtering
 *
 * As mentioned, prior to any queries being executed, the request data is modified by any filters
 * configured. Filters are callbacks which accept the value of a submitted form field as input, and
 * return a modified version of the value as output. Filters can be any PHP callable, i.e. a closure
 * or `array('ClassName', 'method')`.
 *
 * For example, if you're doing simple password hashing against a legacy application, you can
 * configure the adapter as follows:
 *
 * {{{
 * Auth::config(array(
 * 	'default' => array(
 * 		'adapter' => 'Form',
 * 		'filters' => array('password' => array('lithium\util\String', 'hash')),
 * 		'validators' => array('password' => false)
 * 	)
 * ));
 * }}}
 *
 * This applies the default system hash (SHA 512) against the password prior to using it in the
 * query, and overrides `'validators'` to disable the default crypto-based query validation that
 * would occur after the query.
 *
 * Note that if you are specifying the `'fields'` configuration using key / value pairs, the key
 * used to specify the filter must match the key side of the `'fields'` assignment. Additionally,
 * specifying a filter with no key allows the entire data array to be filtered, as in the following:
 *
 * {{{
 * Auth::config(array(
 * 	'default' => array(
 * 		'adapter' => 'Form',
 * 		'filters' => array(function ($data) {
 * 			// Make any modifications to $data, including adding/removing keys
 * 			return $data;
 * 		})
 * 	)
 * ));
 * }}}
 *
 * For more information, see the `_filters()` method or the `$_filters` property.
 *
 * ## Post-Query Validation
 *
 * In addition to filtering data, you can also apply validators to do check submitted form data
 * against database values programmatically. For example, the default adapter uses a cryptographic
 * hash function which operates in constant time to validate passwords. Configuring this validator
 * manually would work as follows:
 *
 * {{{
 * use lithium\security\Password;
 *
 * Auth::config(array(
 * 	'default' => array(
 * 		'adapter' => 'Form',
 * 		'validators' => array(
 * 			'password' => function($form, $data) {
 * 				return Password::check($form, $data);
 * 			}
 * 		)
 * 	)
 * ));
 * }}}
 *
 * As with filters, each validator can be defined as any PHP callable, and must be keyed using the
 * name of the form field submitted (if form and database field names do not match). If a validator
 * is specified with no key, it will apply to all data submitted. See the `$_validators` property
 * and the `_validate()` method for more information.
 *
 * @see lithium\net\http\Request::$data
 * @see lithium\data\Model::find()
 * @see lithium\util\String::hash()
 */
class Form extends \lithium\core\Object {

	/**
	 * The name of the model class to query against. This can either be a model name (i.e.
	 * `'Users'`), or a fully-namespaced class reference (i.e. `'app\models\Users'`). When
	 * authenticating users, the magic `first()` method is invoked against the model to return the
	 * first record found when combining the conditions in the `$_scope` property with the
	 * authentication data yielded from the `Request` object in `Form::check()`. (Note that the
	 * model method called is configurable using the `$_query` property).
	 *
	 * @see lithium\security\auth\adapter\Form::$_query
	 * @var string
	 */
	protected $_model = '';

	/**
	 * The list of fields to extract from the `Request` object and use when querying the database.
	 * This can either be a simple array of field names, or a set of key/value pairs, which map
	 * the field names in the request to database field names.
	 *
	 * For example, if you had a form field name `username`, which mapped to a database field named
	 * username, you could use the following in the `'fields'` configuration:
	 *
	 * {{{ embed:lithium\tests\cases\security\auth\adapter\FormTest::testMixedFieldMapping(3-3) }}}
	 *
	 * This is especially relevant for document databases, where you may want to map a form field to
	 * a nested document field:
	 *
	 * {{{
	 * 'fields' => array('username' => 'login.username', 'password'),
	 * }}}
	 *
	 * @var array
	 */
	protected $_fields = array();

	/**
	 * Additional data to apply to the model query conditions when looking up users, i.e.
	 * `array('active' => true)` to disallow authenticating against inactive user accounts.
	 *
	 * @var array
	 */
	protected $_scope = array();

	/**
	 * Callback filters to apply to request data before using it in the authentication query. Each
	 * key in the array must match a request field specified in the `$_fields` property, and each
	 * value must either be a reference to a function or method name, or a closure. For example, to
	 * automatically hash passwords using simple SHA 512 hashing, the `Form` adapter could be
	 * configured with the following: `array('password' => array('lithium\util\String', 'hash'))`.
	 *
	 * Optionally, you can specify a callback with no key, which will receive (and can modify) the
	 * entire credentials array before the query is executed, as in the following example:
	 *
	 * {{{
	 * 	Auth::config(array(
	 * 		'members' => array(
	 * 			'adapter' => 'Form',
	 * 			'model' => 'Member',
	 * 			'fields' => array('email', 'password'),
	 * 			'filters' => array(function($data) {
	 * 				// If the user is outside the company, then their account must have the
	 * 				// 'private' field set to true in order to log in:
	 * 				if (!preg_match('/@mycompany\.com$/', $data['email'])) {
	 * 					$data['private'] = true;
	 * 				}
	 * 				return $data;
	 * 			})
	 * 		)
	 * 	));
	 * }}}
	 *
	 * @see lithium\security\auth\adapter\Form::$_fields
	 * @var array
	 */
	protected $_filters = array();

	/**
	 * An array of callbacks, keyed by form field name, which make an assertion about a piece of
	 * submitted form data. Each validator should accept the value of the form field submitted
	 * (which will be modified by any applied filters), and return a boolean value indicating the
	 * success of the validation. If a validator is specified with no key, it will receive all form
	 * data and all database data. See the `_validate()` method for more information.
	 *
	 * @see lithium\security\auth\adapter\Form::_validate()
	 * @var array
	 */
	protected $_validators = array();

	/**
	 * If you require custom model logic in your authentication query, use this setting to specify
	 * which model method to call, and this method will receive the authentication query. In return,
	 * the `Form` adapter expects a `Record` object which implements the `data()` method. See the
	 * constructor for more information on setting this property. Defaults to `'first'`, which
	 * calls, for example, `Users::first()`.
	 *
	 * @see lithium\security\auth\adapter\Form::__construct()
	 * @see lithium\data\entity\Record::data()
	 * @var string
	 */
	protected $_query = 'first';

	/**
	 * List of configuration properties to automatically assign to the properties of the adapter
	 * when the class is constructed.
	 *
	 * @var array
	 */
	protected $_autoConfig = array('model', 'fields', 'scope', 'filters', 'validators', 'query');

	/**
	 * Sets the initial configuration for the `Form` adapter, as detailed below.
	 *
	 * @see lithium\security\auth\adapter\Form::$_model
	 * @see lithium\security\auth\adapter\Form::$_fields
	 * @see lithium\security\auth\adapter\Form::$_filters
	 * @see lithium\security\auth\adapter\Form::$_validators
	 * @see lithium\security\auth\adapter\Form::$_query
	 * @param array $config Sets the configuration for the adapter, which has the following options:
	 *              - `'model'` _string_: The name of the model class to use. See the `$_model`
	 *                property for details.
	 *              - `'fields'` _array_: The model fields to query against when taking input from
	 *                the request data. See the `$_fields` property for details.
	 *              - `'scope'` _array_: Any additional conditions used to constrain the
	 *                authentication query. For example, if active accounts in an application have
	 *                an `active` field which must be set to `true`, you can specify
	 *                `'scope' => array('active' => true)`. See the `$_scope` property for more
	 *                details.
	 *              - `'filters'` _array_: Named callbacks to apply to request data before the user
	 *                lookup query is generated. See the `$_filters` property for more details.
	 *              - `'validators'` _array_: Named callbacks to apply to fields in request data and
	 *                corresponding fields in database data in order to do programmatic
	 *                authentication checks after the query has occurred. See the `$_validators`
	 *                property for more details.
	 *              - `'query'` _string_: Determines the model method to invoke for authentication
	 *                checks. See the `$_query` property for more details.
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'model' => 'Users',
			'query' => 'first',
			'filters' => array(),
			'validators' => array(),
			'fields' => array('username', 'password')
		);
		$config += $defaults;

		$password = function($form, $data) {
			return Password::check($form, $data);
		};
		$config['validators'] = array_filter($config['validators'] + compact('password'));

		parent::__construct($config + $defaults);
	}

	/**
	 * Initializes values configured in the constructor.
	 *
	 * @return void
	 */
	protected function _init() {
		parent::_init();

		foreach ($this->_fields as $key => $val) {
			if (is_int($key)) {
				unset($this->_fields[$key]);
				$this->_fields[$val] = $val;
			}
		}
		$this->_model = Libraries::locate('models', $this->_model);
	}

	/**
	 * Called by the `Auth` class to run an authentication check against a model class using the
	 * credentials in a data container (a `Request` object), and returns an array of user
	 * information on success, or `false` on failure.
	 *
	 * @param object $credentials A data container which wraps the authentication credentials used
	 *               to query the model (usually a `Request` object). See the documentation for this
	 *               class for further details.
	 * @param array $options Additional configuration options. Not currently implemented in this
	 *              adapter.
	 * @return array Returns an array containing user information on success, or `false` on failure.
	 */
	public function check($credentials, array $options = array()) {
		$model = $this->_model;
		$query = $this->_query;
		$data = $this->_filters($credentials->data);

		$validate = array_flip(array_intersect_key($this->_fields, $this->_validators));
		$conditions = $this->_scope + array_diff_key($data, $validate);
		$user = $model::$query(compact('conditions') + $options);

		if (!$user) {
			return false;
		}
		return $this->_validate($user, $data);
	}

	/**
	 * A pass-through method called by `Auth`. Returns the value of `$data`, which is written to
	 * a user's session. When implementing a custom adapter, this method may be used to modify or
	 * reject data before it is written to the session.
	 *
	 * @param array $data User data to be written to the session.
	 * @param array $options Adapter-specific options. Not implemented in the `Form` adapter.
	 * @return array Returns the value of `$data`.
	 */
	public function set($data, array $options = array()) {
		return $data;
	}

	/**
	 * Called by `Auth` when a user session is terminated. Not implemented in the `Form` adapter.
	 *
	 * @param array $options Adapter-specific options. Not implemented in the `Form` adapter.
	 * @return void
	 */
	public function clear(array $options = array()) {
	}

	/**
	 * Iterates over the filters configured in `$_filters` which are applied to submitted form data
	 * _before_ it is used in the query.
	 *
	 * @see lithium\security\auth\adapter\Form::$_filters
	 * @param array $data The array of raw form data to be filtered.
	 * @return array Callback result.
	 */
	protected function _filters($data) {
		$result = array();

		foreach ($this->_fields as $from => $to) {
			$result[$to] = isset($data[$from]) ? $data[$from] : null;

			if (!isset($this->_filters[$from])) {
				$result[$to] = is_scalar($result[$to]) ? $result[$to] : null;
				continue;
			}
			if ($this->_filters[$from] === false) {
				continue;
			}
			if (!is_callable($this->_filters[$from])) {
				$message = "Authentication filter for `{$from}` is not callable.";
				throw new UnexpectedValueException($message);
			}
			$result[$to] = call_user_func($this->_filters[$from], $result[$to]);
		}
		if (!isset($this->_filters[0])) {
			return $result;
		}
		if (!is_callable($this->_filters[0])) {
			throw new UnexpectedValueException("Authentication filter is not callable.");
		}
		return call_user_func($this->_filters[0], $result);
	}

	/**
	 * After an authentication query against the configured model class has occurred, this method
	 * iterates over the configured validators and checks each one by passing the submitted form
	 * value as the first parameter, and the corresponding database value as the second. The
	 * validator then returns a boolean to indicate success. If the validator fails, it will cause
	 * the entire authentication operation to fail. Note that any filters applied to a form field
	 * will affect the data passed to the validator.
	 *
	 * @see lithium\security\auth\adapter\Form::__construct()
	 * @see lithium\security\auth\adapter\Form::$_validators
	 * @param object $user The user object returned from the database. This object must support a
	 *               `data()` method, which returns the object's array representation, and
	 *               also returns individual field values by name.
	 * @param array $data The form data submitted in the request and passed to `Form::check()`.
	 * @return array Returns an array of authenticated user data on success, otherwise `false` if
	 *               any of the configured validators fails. See `'validators'` in the `$config`
	 *               parameter of `__construct()`.
	 */
	protected function _validate($user, array $data) {
		foreach ($this->_validators as $field => $validator) {
			if (!isset($this->_fields[$field]) || $field === 0) {
				continue;
			}

			if (!is_callable($validator)) {
				$message = "Authentication validator for `{$field}` is not callable.";
				throw new UnexpectedValueException($message);
			}

			$field = $this->_fields[$field];
			$value = isset($data[$field]) ? $data[$field] : null;

			if (!call_user_func($validator, $value, $user->data($field))) {
				return false;
			}
		}
		$user = $user->data();

		if (!isset($this->_validators[0])) {
			return $user;
		}
		if (!is_callable($this->_validators[0])) {
			throw new UnexpectedValueException("Authentication validator is not callable.");
		}
		return call_user_func($this->_validators[0], $data, $user) ? $user : false;
	}
}

?>