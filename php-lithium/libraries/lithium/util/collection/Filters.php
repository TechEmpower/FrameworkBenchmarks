<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\util\collection;

/**
 * The `Filters` class is the basis of Lithium's method filtering system: an efficient way to enable
 * event-driven communication between classes without tight coupling and without depending on a
 * centralized publish/subscribe system.
 *
 * In Lithium itself, when creating a method that can be filtered, a method is implemented as a
 * [ closure](http://us2.php.net/manual/en/functions.anonymous.php) and is passed to either
 * `Object::_filter()` or `StaticObject::_filter()`. Each object internally maintains its own list
 * of filters, which are applied in these methods and passed to `Filters::run()`.
 *
 * When implementing a custom filter system outside of Lithium, you can create your own list of
 * filters, and pass it to `$options['data']` in the `run()` method.
 *
 * When creating a filter to apply to a method, you need the name of the method you want to call,
 * along with a **closure**, that defines what you want the filter to do.  All filters take the same
 * 3 parameters: `$self`,`$params`, and `$chain`.
 *
 * - `$self`: If the filter is applied on an object instance, then `$self` will be that instance. If
 * applied to a static class, then `$self` will be a string containing the fully-namespaced class
 * name.
 *
 * - `$params`: Contains an associative array of the parameters that are passed into the method. You
 * can modify or inspect these parameters before allowing the method to continue.
 *
 * - `$chain`: Finally, `$chain` contains the list of filters in line to be executed (as an
 * instance of the `Filters` class).  At the bottom of `$chain` is the method itself.  This is why
 * most filters contain a line that looks like this:
 *
 * {{{return $chain->next($self, $params, $chain);}}}
 *
 * This passes control to the next filter in the chain, and finally, to the method itself.  This
 * allows you to interact with the return value as well as the parameters.
 *
 * Within the framework, you can call `applyFilter()` on any object (static or instantiated) and
 * pass the name of the method you would like to filter, along with the filter itself. For example:
 *
 * {{{use lithium\action\Dispatcher;
 *
 * Dispatcher::applyFilter('run', function($self, $params, $chain) {
 * 	// Custom pre-dispatch logic goes here
 * 	$response = $chain->next($self, $params, $chain);
 *
 * 	// $response now contains a Response object with the result of the dispatched request,
 * 	// and can be modified as appropriate
 * 	// ...
 * 	return $response;
 * });}}}
 *
 * The logic in the closure will now be executed on every call to `Dispatcher::run()`, and
 * `$response` will always be modified by any custom logic present before being returned from
 * `run()`.
 *
 * @see lithium\util\collection\Filters::run()
 * @see lithium\core\Object::_filter()
 * @see lithium\core\StaticObject::_filter()
 * @see lithium\core\Object::applyFilter()
 * @see lithium\core\StaticObject::applyFilter()
 */
class Filters extends \lithium\util\Collection {

	/**
	 * An array of filters indexed by class and method name, stored so that they can be lazily
	 * applied to classes which are not loaded yet.
	 *
	 * @var array
	 */
	protected static $_lazyFilters = array();

	/**
	 * This is the list of configuration settings that will be automatically applied to the
	 * properties of each `Filters` instance.
	 *
	 * @var array
	 */
	protected $_autoConfig = array('data', 'class', 'method');

	/**
	 * The fully-namespaced class name of the class containing the method being filtered.
	 *
	 * @see lithium\util\collection\Filters::method()
	 * @var string
	 */
	protected $_class = null;

	/**
	 * The name of the method being filtered by the current instance of `Filters`.
	 *
	 * @see lithium\util\collection\Filters::method()
	 * @var string
	 */
	protected $_method = null;

	/**
	 * Lazily applies a filter to a method of a static class.
	 *
	 * This method is useful if you want to apply a filter inside a global bootstrap file to a
	 * static class which may or may not be loaded during every request, or which may be loaded
	 * lazily elsewhere in your application. If the class is already loaded, the filter will be
	 * applied immediately.
	 *
	 * However, if the class has not been loaded, the filter will be stored and applied to the class
	 * the first time the method specified in `$method` is called. This works for any class which
	 * extends `StaticObject`.
	 *
	 * @see lithium\core\StaticObject
	 * @param string $class The fully namespaced name of a **static** class to which the filter will
	 *               be applied. The class name specified in `$class` **must** extend
	 *               `StaticObject`, or else statically implement the `applyFilter()` method.
	 * @param string $method The method to which the filter will be applied.
	 * @param closure $filter The filter to apply to the class method.
	 * @return void
	 */
	public static function apply($class, $method, $filter) {
		if (class_exists($class, false)) {
			return $class::applyFilter($method, $filter);
		}
		static::$_lazyFilters[$class][$method][] = $filter;
	}

	/**
	 * Checks to see if the given class / method has any filters which have been applied lazily,
	 * and not yet attached.
	 *
	 * If a filter has been lazily applied (using `Filters::apply()`) to a class which is/was not
	 * yet loaded, checks to see if the filter is still being held, or has been applied. The filter
	 * will not be applied until the method being filtered has been called.
	 *
	 * @see lithium\util\collection\Filters::apply()
	 * @param string $class Fully-namespaced class name.
	 * @param string $method Method name.
	 * @return boolean
	 */
	public static function hasApplied($class, $method) {
		return isset(static::$_lazyFilters[$class][$method]);
	}

	/**
	 * Collects a set of filters to iterate. Creates a filter chain for the given class/method,
	 * executes it, and returns the value.
	 *
	 * @param mixed $class The class for which this filter chain is being created. If this is the
	 *        result of a static method call, `$class` should be a string. Otherwise, it should
	 *        be the instance of the object making the call.
	 * @param array $params An associative array of the given method's parameters.
	 * @param array $options The configuration options with which to create the filter chain.
	 *        Mainly, these options allow the `Filters` object to be queried for details such as
	 *        which class / method initiated it. Available keys:
	 *
	 *        -'class': The name of the class that initiated the filter chain.
	 *        -'method': The name of the method that initiated the filter chain.
	 *        -`'data'` _array_: An array of callable objects (usually closures) to be iterated
	 *          through. By default, execution will be nested such that the first item will be
	 *          executed first, and will be the last to return.
	 * @return Returns the value returned by the first closure in `$options['data`]`.
	 */
	public static function run($class, $params, array $options = array()) {
		$defaults = array('class' => null, 'method' => null, 'data' => array());
		$options += $defaults;
		$lazyFilterCheck = (is_string($class) && $options['method']);

		if (($lazyFilterCheck) && isset(static::$_lazyFilters[$class][$options['method']])) {
			$filters = static::$_lazyFilters[$class][$options['method']];
			unset(static::$_lazyFilters[$class][$options['method']]);
			$options['data'] = array_merge($filters, $options['data']);

			foreach ($filters as $filter) {
				$class::applyFilter($options['method'], $filter);
			}
		}

		$chain = new Filters($options);
		$next = $chain->rewind();
		return $next($class, $params, $chain);
	}

	/**
	 * Provides short-hand convenience syntax for filter chaining.
	 *
	 * @see lithium\core\Object::applyFilter()
	 * @see lithium\core\Object::_filter()
	 * @param object $self The object instance that owns the filtered method.
	 * @param array $params An associative array containing the parameters passed to the filtered
	 *              method.
	 * @param array $chain The Filters object instance containing this chain of filters.
	 * @return mixed Returns the return value of the next filter in the chain.
	 */
	public function next($self, $params, $chain) {
		if (empty($self) || empty($chain)) {
			return parent::next();
		}
		$next = parent::next();
		return $next($self, $params, $chain);
	}

	/**
	 * Gets the method name associated with this filter chain.  This is the method being filtered.
	 *
	 * @param boolean $full Whether to return the method name including the class name or not.
	 * @return string
	 */
	public function method($full = false) {
		return $full ? $this->_class . '::' . $this->_method : $this->_method;
	}
}

?>